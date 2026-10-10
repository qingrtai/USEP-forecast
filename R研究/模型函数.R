# 02 与 04 共用的评分、特征选择、标准化和滚动预测函数，由 02 第 15–134 行原样移出。
# 调用前须加载 forecast、xgboost、keras3，并已定义 mase_scale、price_features、warmup_date、refit_days、prepare_features() 等对象。
score <- function(actual, predicted) {
error <- predicted - actual
denominator <- abs(actual) + abs(predicted)
smape <- ifelse(denominator == 0, 0, 200 * abs(error) / denominator)
return(data.frame(N = length(error), RMSE = sqrt(mean(error^2)), MAE = mean(abs(error)), MASE = mean(abs(error)) / mase_scale, sMAPE = mean(smape)))
}
feature_names <- function(kind, step = 4L, tpc = FALSE) {
features <- if (kind == "DynamicRegression") c("USEP_MA7", "USEP_SD7") else price_features
if (step >= 1L) features <- c(features, "DemandMean_lag1")
if (step >= 2L) features <- c(features, "PrimaryReserve_lag1", "ContingencyReserve_lag1", "Regulation_lag1")
if (step >= 3L) features <- c(features, "BrentSGD_lag1")
if (step >= 4L) features <- c(features, "TempSG_model_lag1", "Monday", "Tuesday", "Wednesday", "Thursday", "Saturday", "Weekend", "PublicHoliday")
if (tpc) features <- c(features, "TPCRegime")
return(features)
}
# 矩阵缺值只用该次训练的中位数；保留训练内标准化参数供 LSTM 反复使用。
fit_scaler <- function(x) {
medians <- apply(x, 2, median, na.rm = TRUE)
for (j in seq_len(ncol(x))) x[is.na(x[, j]), j] <- medians[j]
center <- colMeans(x)
scale <- apply(x, 2, sd)
scale[scale == 0] <- 1
return(list(medians = medians, center = center, scale = scale))
}
transform_x <- function(x, scaler, standardize = FALSE) {
for (j in seq_len(ncol(x))) x[is.na(x[, j]), j] <- scaler$medians[j]
if (standardize) x <- sweep(sweep(x, 2, scaler$center, "-"), 2, scaler$scale, "/")
return(x)
}
make_sequences <- function(x, index, length) {
sequences <- array(0, dim = c(length(index), length, ncol(x)))
for (j in seq_along(index)) sequences[j, , ] <- x[(index[j] - length + 1L):index[j], , drop = FALSE]
return(sequences)
}
new_lstm <- function(config, n_features, seed) {
clear_session()
set_random_seed(seed)
model <- keras_model_sequential(input_shape = c(config$sequence, n_features))
model <- layer_lstm(model, units = config$units)
model <- layer_dropout(model, rate = config$dropout)
model <- layer_dense(model, units = 1L)
compile(model, optimizer = optimizer_adam(learning_rate = config$learning_rate), loss = "mse")
return(model)
}
# 内部早停只看当前训练历史的最后 28 天；随后用选定轮数重新拟合完整训练历史。
fit_xgb <- function(x, y, config, seed) {
set.seed(seed)
params <- c(list(objective = "reg:squarederror", eval_metric = "rmse", nthread = 1L, seed = seed), config)
n <- nrow(x)
inner <- seq_len(n - 28L)
holdout <- (n - 27L):n
early <- xgb.train(params = params, data = xgb.DMatrix(x[inner, , drop = FALSE], label = y[inner]), nrounds = 600L, evals = list(validation = xgb.DMatrix(x[holdout, , drop = FALSE], label = y[holdout])), early_stopping_rounds = 20L, verbose = 0L)
rounds <- which.min(as.numeric(attr(early, "evaluation_log")[[2]]))
model <- xgb.train(params = params, data = xgb.DMatrix(x, label = y), nrounds = rounds, verbose = 0L)
return(model)
}
fit_lstm <- function(x, y, index, config, seed) {
inner_index <- head(index, -28L)
holdout_index <- tail(index, 28L)
inner_scaler <- fit_scaler(x[seq_len(max(inner_index)), , drop = FALSE])
inner_x <- transform_x(x, inner_scaler, TRUE)
inner_y_mean <- mean(y[inner_index])
inner_y_sd <- sd(y[inner_index])
early <- new_lstm(config, ncol(x), seed)
history <- fit(early, make_sequences(inner_x, inner_index, config$sequence), (y[inner_index] - inner_y_mean) / inner_y_sd, validation_data = list(make_sequences(inner_x, holdout_index, config$sequence), (y[holdout_index] - inner_y_mean) / inner_y_sd), epochs = 100L, batch_size = 32L, shuffle = FALSE, callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 10L)), verbose = 0L)
epochs <- which.min(history$metrics$val_loss)
scaler <- fit_scaler(x[seq_len(max(index)), , drop = FALSE])
scaled_x <- transform_x(x, scaler, TRUE)
y_mean <- mean(y[index])
y_sd <- sd(y[index])
model <- new_lstm(config, ncol(x), seed)
fit(model, make_sequences(scaled_x, index, config$sequence), (y[index] - y_mean) / y_sd, epochs = epochs, batch_size = 32L, shuffle = FALSE, verbose = 0L)
return(list(model = model, scaler = scaler, y_mean = y_mean, y_sd = y_sd))
}
# 所有候选使用同样的逐日日期；周内只更新状态和输入，模型参数每七日重估。
rolling_predict <- function(kind, config, dates, step = 4L, tpc = FALSE, seed = 2026L) {
prediction <- numeric(length(dates))
features <- feature_names(kind, step, tpc)
for (j in seq_along(dates)) {
date <- dates[j]
if ((j - 1L) %% refit_days == 0L) {
cutoff <- date - 1L
dat <- prepare_features(cutoff, min(max(dates), cutoff + refit_days))
train_index <- which(dat$Date <= cutoff & dat$Date >= warmup_date)
history_index <- which(dat$Date <= cutoff)
y <- dat$DailyUSEP
if (kind %in% c("TSLM", "DynamicRegression", "XGBoost", "LSTM")) {
x <- as.matrix(dat[features])
scaler <- fit_scaler(x[train_index, , drop = FALSE])
plain_x <- transform_x(x, scaler)
}
if (kind == "ETS") model <- ets(ts(y[history_index], frequency = 7), model = config$model, damped = config$damped)
if (kind == "SARIMA") model <- Arima(ts(y[history_index], frequency = 7), order = config$order, seasonal = list(order = config$seasonal, period = 7), include.constant = TRUE, method = "ML")
if (kind == "TSLM") model <- lm(DailyUSEP ~ ., data = data.frame(DailyUSEP = y[train_index], plain_x[train_index, , drop = FALSE]))
if (kind == "DynamicRegression") model <- Arima(ts(y[train_index], frequency = 7), xreg = plain_x[train_index, , drop = FALSE], order = config$order, seasonal = list(order = config$seasonal, period = 7), include.constant = TRUE, method = "ML")
if (kind == "XGBoost") model <- fit_xgb(plain_x[train_index, , drop = FALSE], y[train_index], config, seed)
if (kind == "LSTM") {
model <- fit_lstm(x, y, train_index, config, seed)
message(paste("LSTM 已训练至", cutoff, "种子", seed))
}
}
current <- match(date, dat$Date)
if (kind == "SeasonalNaive") prediction[j] <- dat$DailyUSEP[current - 7L]
if (kind == "ETS") prediction[j] <- as.numeric(forecast(ets(ts(dat$DailyUSEP[seq_len(current - 1L)], frequency = 7), model = model, use.initial.values = TRUE), h = 1L)$mean)
if (kind == "SARIMA") prediction[j] <- as.numeric(forecast(Arima(ts(dat$DailyUSEP[seq_len(current - 1L)], frequency = 7), model = model), h = 1L)$mean)
if (kind == "TSLM") prediction[j] <- as.numeric(predict(model, newdata = as.data.frame(plain_x[current, , drop = FALSE])))
if (kind == "DynamicRegression") {
past <- which(dat$Date < date & dat$Date >= warmup_date)
updated <- Arima(ts(dat$DailyUSEP[past], frequency = 7), xreg = plain_x[past, , drop = FALSE], model = model)
prediction[j] <- as.numeric(forecast(updated, xreg = plain_x[current, , drop = FALSE], h = 1L)$mean)
}
if (kind == "XGBoost") prediction[j] <- as.numeric(predict(model, xgb.DMatrix(plain_x[current, , drop = FALSE])))
if (kind == "LSTM") {
scaled_x <- transform_x(x, model$scaler, TRUE)
prediction[j] <- as.numeric(predict(model$model, make_sequences(scaled_x, current, config$sequence), verbose = 0L)) * model$y_sd + model$y_mean
}
}
stopifnot(all(is.finite(prediction)))
return(prediction)
}
