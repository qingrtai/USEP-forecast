# 先运行 01_数据处理与变量生成.R；本文件延续其对象，不重新读取或修改原始 Excel。
# 六模型及 benchmark；2025 年选设置，2026 年只按每七日扩展重估，不重新调参。
# LSTM 首次配置后端时单独运行：keras3::install_keras(backend = "tensorflow")
library(forecast)
library(xgboost)
library(keras3)
# 接口依据：https://xgboost.readthedocs.io/en/latest/r_docs/R-package/docs/reference/xgb.train.html
# 接口依据：https://keras3.posit.co/reference/fit.keras.src.models.model.Model.html
validation_dates <- daily$Date[daily$Sample == "Validation"]
test_dates <- daily$Date[daily$Sample == "Test"]
train_y <- daily$DailyUSEP[daily$Date <= train_end]
mase_scale <- mean(abs(train_y[8:length(train_y)] - train_y[1:(length(train_y) - 7L)]))
spike_threshold <- as.numeric(quantile(train_y, 0.95, type = 7))
# 指标统一用 SGD/MWh 原尺度；MASE 分母和尖峰阈值只从初始训练期计算。
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
# SARIMA 在初始训练期按 AICc 留六个候选，再比较 2025 年滚动误差。
arima_grid <- expand.grid(p = 0:2, d = 0:1, q = 0:2, P = 0:1, D = 0:1, Q = 0:1)
arima_grid <- arima_grid[arima_grid$d + arima_grid$D <= 1L, ]
arima_candidates <- function(kind, step = 4L, tpc = FALSE) {
features <- feature_names(kind, step, tpc)
train <- initial_data[initial_data$Date >= warmup_date, ]
series <- if (kind == "SARIMA") train_y else train$DailyUSEP
xreg <- if (kind == "SARIMA") NULL else as.matrix(train[features])
criterion <- numeric(nrow(arima_grid))
for (i in seq_len(nrow(arima_grid))) {
row <- arima_grid[i, ]
fit <- tryCatch(Arima(ts(series, frequency = 7), xreg = xreg, order = c(row$p, row$d, row$q), seasonal = list(order = c(row$P, row$D, row$Q), period = 7), include.constant = TRUE, method = "ML"), error = function(e) NULL)
criterion[i] <- if (is.null(fit)) Inf else fit$aicc
}
selected <- head(order(criterion), 6L)
stopifnot(all(is.finite(criterion[selected])))
return(lapply(selected, function(i) list(order = as.integer(unlist(arima_grid[i, c("p", "d", "q")])), seasonal = as.integer(unlist(arima_grid[i, c("P", "D", "Q")])))))
}
ets_candidates <- list(list(model = "ANA", damped = FALSE), list(model = "AAA", damped = FALSE), list(model = "AAA", damped = TRUE))
set.seed(2026)
xgb_grid <- expand.grid(max_depth = c(2L, 3L, 4L), eta = c(0.03, 0.1), min_child_weight = c(1, 5), subsample = c(0.8, 1), colsample_bytree = c(0.8, 1), lambda = c(1, 5))
xgb_grid <- xgb_grid[sample(nrow(xgb_grid), 12L), ]
xgb_candidates <- lapply(seq_len(nrow(xgb_grid)), function(i) as.list(xgb_grid[i, ]))
lstm_grid <- expand.grid(sequence = c(7L, 14L, 28L), units = c(16L, 32L), dropout = c(0.1, 0.2), learning_rate = c(0.001, 0.0005))
lstm_grid <- lstm_grid[sample(nrow(lstm_grid), 12L), ]
lstm_candidates <- lapply(seq_len(nrow(lstm_grid)), function(i) as.list(lstm_grid[i, ]))
validation_log <- data.frame()
validation_actual <- daily$DailyUSEP[match(validation_dates, daily$Date)]
# 候选失败写入验证日志，不能将缺失预测当成成功；最优候选必须完成整年验证。
tune <- function(kind, candidates, step = 4L, tpc = FALSE) {
predictions <- vector("list", length(candidates))
rmse <- rep(Inf, length(candidates))
for (i in seq_along(candidates)) {
message(paste("验证", kind, "M", step, "TPC", tpc, "候选", i))
started <- proc.time()[["elapsed"]]
failure <- ""
record_failure <- function(e) {
failure <<- conditionMessage(e)
return(NULL)
}
predictions[i] <- list(tryCatch(rolling_predict(kind, candidates[[i]], validation_dates, step, tpc), error = record_failure))
if (!is.null(predictions[[i]])) rmse[i] <- sqrt(mean((predictions[[i]] - validation_actual)^2))
validation_log <<- rbind(validation_log, data.frame(Model = kind, Step = step, TPC = tpc, Candidate = i, RMSE = rmse[i], Seconds = proc.time()[["elapsed"]] - started, Failure = failure))
}
stopifnot(any(is.finite(rmse)))
best <- which.min(rmse)
return(list(config = candidates[[best]], prediction = predictions[[best]], tpc = tpc, step = step, RMSE = rmse[best]))
}
get_candidates <- function(kind, step = 4L, tpc = FALSE) {
if (kind == "ETS") return(ets_candidates)
if (kind %in% c("SARIMA", "DynamicRegression")) return(arima_candidates(kind, step, tpc))
if (kind == "TSLM") return(list(list()))
if (kind == "XGBoost") return(xgb_candidates)
if (kind == "LSTM") return(lstm_candidates)
}
# TPC 的保留条件：全年 RMSE 至少改善 1%，且至少三个季度改善。
tpc_log <- data.frame()
select_tpc <- function(kind) {
without <- tune(kind, get_candidates(kind, 4L, FALSE), 4L, FALSE)
with <- tune(kind, get_candidates(kind, 4L, TRUE), 4L, TRUE)
quarter <- (as.integer(format(validation_dates, "%m")) - 1L) %/% 3L + 1L
quarter_gain <- vapply(1:4, function(q) sqrt(mean((without$prediction[quarter == q] - validation_actual[quarter == q])^2)) - sqrt(mean((with$prediction[quarter == q] - validation_actual[quarter == q])^2)), numeric(1))
gain <- 1 - with$RMSE / without$RMSE
keep <- gain >= 0.01 && sum(quarter_gain > 0) >= 3L
tpc_log <<- rbind(tpc_log, data.frame(Model = kind, RelativeImprovement = gain, BetterQuarters = sum(quarter_gain > 0), KeepTPC = keep))
return(if (keep) with else without)
}
selected <- list()
selected$ETS <- tune("ETS", get_candidates("ETS"))
selected$SARIMA <- tune("SARIMA", get_candidates("SARIMA"))
for (kind in c("TSLM", "DynamicRegression", "XGBoost", "LSTM")) selected[[kind]] <- select_tpc(kind)
# M0–M4 分别调参；各步骤沿用全模型在验证期确定的 TPC 选择。
incremental <- list()
for (kind in c("DynamicRegression", "XGBoost")) {
for (step in 0:4) {
key <- paste0(kind, "_M", step)
incremental[[key]] <- if (step == 4L) selected[[kind]] else tune(kind, get_candidates(kind, step, selected[[kind]]$tpc), step, selected[[kind]]$tpc)
}
}
# LSTM 最终用三个固定种子求均值；区间初始化采用相同三种子均值的 2025 年样本外误差。
selected$LSTM$prediction <- rowMeans(sapply(c(2026L, 2027L, 2028L), function(seed) rolling_predict("LSTM", selected$LSTM$config, validation_dates, 4L, selected$LSTM$tpc, seed)))
# 推荐模型按验证 RMSE 决定；在最优值 1% 以内优先选择结构更简单者。
validation_rmse <- vapply(selected, function(x) sqrt(mean((x$prediction - validation_actual)^2)), numeric(1))
simplicity <- c("ETS", "SARIMA", "TSLM", "DynamicRegression", "XGBoost", "LSTM")
eligible <- names(validation_rmse)[validation_rmse <= min(validation_rmse) * 1.01]
recommended <- simplicity[simplicity %in% eligible][1]
freeze <- list(models = selected, incremental = incremental, recommended = recommended, temperature_lambda = temperature_lambda, spike_threshold = spike_threshold, mase_scale = mase_scale, refit_days = refit_days, interval_window = 180L, interval_min = 100L, seeds = c(2026L, 2027L, 2028L))
saveRDS(freeze, file.path(output_dir, "冻结的研究设置.rds"))
write_xlsx(list(Validation = validation_log, TPC = tpc_log), file.path(output_dir, "验证与参数选择.xlsx"))
test_prediction <- data.frame(Date = test_dates, Actual = daily$DailyUSEP[match(test_dates, daily$Date)])
test_prediction$SeasonalNaive <- rolling_predict("SeasonalNaive", list(), test_dates)
for (kind in names(selected)) {
config <- selected[[kind]]
if (kind == "LSTM") test_prediction[[kind]] <- rowMeans(sapply(c(2026L, 2027L, 2028L), function(seed) rolling_predict(kind, config$config, test_dates, 4L, config$tpc, seed)))
if (kind != "LSTM") test_prediction[[kind]] <- rolling_predict(kind, config$config, test_dates, 4L, config$tpc)
}
# 每日区间仅用此前已实现的误差，最多 180 条；80% 和 95% 的有限样本分位秩向上取整。
intervals <- data.frame()
validation_predictions <- data.frame(Date = validation_dates, Actual = validation_actual)
validation_predictions$SeasonalNaive <- rolling_predict("SeasonalNaive", list(), validation_dates)
for (kind in names(selected)) validation_predictions[[kind]] <- selected[[kind]]$prediction
models <- c("SeasonalNaive", names(selected))
for (kind in models) {
past_errors <- abs(validation_predictions[[kind]] - validation_actual)
for (i in seq_along(test_dates)) {
calibration <- tail(past_errors, 180L)
stopifnot(length(calibration) >= 100L)
for (alpha in c(0.20, 0.05)) {
rank <- min(length(calibration), ceiling((length(calibration) + 1L) * (1 - alpha)))
radius <- sort(calibration)[rank]
intervals <- rbind(intervals, data.frame(Date = test_dates[i], Model = kind, Level = 1 - alpha, Actual = test_prediction$Actual[i], Prediction = test_prediction[[kind]][i], Lower = test_prediction[[kind]][i] - radius, Upper = test_prediction[[kind]][i] + radius))
}
past_errors <- c(past_errors, abs(test_prediction[[kind]][i] - test_prediction$Actual[i]))
}
}
intervals$Covered <- intervals$Actual >= intervals$Lower & intervals$Actual <= intervals$Upper
intervals$Width <- intervals$Upper - intervals$Lower
intervals$IntervalScore <- intervals$Width + 2 / (1 - intervals$Level) * pmax(intervals$Lower - intervals$Actual, 0) + 2 / (1 - intervals$Level) * pmax(intervals$Actual - intervals$Upper, 0)
interval_summary <- intervals |> group_by(Model, Level) |> summarise(N = n(), Coverage = mean(Covered), MeanWidth = mean(Width), MeanIntervalScore = mean(IntervalScore), .groups = "drop")
# 同一测试期报告总体、普通日、尖峰日，以及 2026-02-28 前后表现；空子样本不改变阈值。
overall <- data.frame()
subperiod <- data.frame()
spike <- test_prediction$Actual > spike_threshold
groups <- list(Normal = !spike, Spike = spike, PreStress = test_dates < as.Date("2026-02-28"), Stress = test_dates >= as.Date("2026-02-28"))
for (kind in models) {
overall <- rbind(overall, cbind(Model = kind, score(test_prediction$Actual, test_prediction[[kind]])))
for (group in names(groups)) {
keep <- groups[[group]]
if (any(keep)) subperiod <- rbind(subperiod, cbind(Model = kind, Group = group, score(test_prediction$Actual[keep], test_prediction[[kind]][keep]), Underprediction = mean(test_prediction[[kind]][keep] < test_prediction$Actual[keep]), MeanShortfall = mean(pmax(test_prediction$Actual[keep] - test_prediction[[kind]][keep], 0))))
}
}
overall <- overall[order(overall$RMSE), ]
benchmark_rmse <- overall$RMSE[overall$Model == "SeasonalNaive"]
overall$RMSEImprovementVsBenchmark <- 1 - overall$RMSE / benchmark_rmse
incremental_prediction <- data.frame(Date = test_dates, Actual = test_prediction$Actual)
incremental_scores <- data.frame()
for (key in names(incremental)) {
kind <- sub("_M[0-4]$", "", key)
config <- incremental[[key]]
prediction <- if (config$step == 4L) test_prediction[[kind]] else rolling_predict(kind, config$config, test_dates, config$step, config$tpc)
incremental_prediction[[key]] <- prediction
incremental_scores <- rbind(incremental_scores, cbind(Model = kind, Step = config$step, score(test_prediction$Actual, prediction)))
}
incremental_scores <- incremental_scores |> group_by(Model) |> arrange(Step, .by_group = TRUE) |> mutate(MarginalRMSEImprovement = 1 - RMSE / lag(RMSE), CumulativeRMSEImprovement = 1 - RMSE / first(RMSE)) |> ungroup()
# 使用已经冻结的设置生成最后一日预测，不按测试排名改选模型。
next_prediction <- data.frame(Date = end_date + 1L, Model = models, Prediction = NA_real_)
for (kind in models) {
if (kind == "SeasonalNaive") next_prediction$Prediction[next_prediction$Model == kind] <- rolling_predict(kind, list(), end_date + 1L)
if (kind %in% names(selected)) {
config <- selected[[kind]]
forecast <- if (kind == "LSTM") mean(vapply(c(2026L, 2027L, 2028L), function(seed) rolling_predict(kind, config$config, end_date + 1L, 4L, config$tpc, seed), numeric(1))) else rolling_predict(kind, config$config, end_date + 1L, 4L, config$tpc)
next_prediction$Prediction[next_prediction$Model == kind] <- forecast
}
}
# 保存不同周训练截止点生成的测试特征；这些特征实际供对应周的预测使用。
collect_test_features <- function(i) {
cutoff <- test_dates[i] - 1L
dat <- prepare_features(cutoff, min(end_date, cutoff + refit_days))
dat <- dat[dat$Date > cutoff, ]
dat$PreprocessingCutoff <- cutoff
return(dat)
}
processed_test <- bind_rows(lapply(seq(1L, length(test_dates), by = refit_days), collect_test_features))
write_xlsx(list(Overall = overall, Subperiod = subperiod, Intervals = interval_summary, Incremental = incremental_scores, TPC = tpc_log, NextDay = next_prediction, Recommendation = data.frame(Model = recommended, ValidationRMSE = validation_rmse[recommended], SpikeThreshold = spike_threshold)), file.path(output_dir, "研究结果.xlsx"))
write_xlsx(list(Test = test_prediction, Validation = validation_predictions, Intervals = intervals, Incremental = incremental_prediction, Test_Features = processed_test), file.path(output_dir, "逐日预测与模型变量.xlsx"))
saveRDS(list(freeze = freeze, selected = selected, validation = validation_predictions, test = test_prediction), file.path(output_dir, "研究运行对象.rds"))
message(paste("研究完成，按验证期确定的推荐模型为", recommended, "；结果保存在", output_dir))
