# 先完成 01、02；本文件沿用冻结设置做 2026 年 9 月独立检验、2026-10-01 一日预测和 10 月路径预测，不重跑 02，不覆盖 results/ 中已有文件。
# 将工作目录设为本文件、八个 Excel 和 raw_202609/ 所在文件夹再运行。LSTM 训练耗时较长，建议后台运行：
#   mkdir -p results/04_中间结果 && nohup Rscript 04_9月检验与实际预测.R > results/04_中间结果/运行日志.txt 2>&1 &
# 每段结果保存在 results/04_中间结果/（不提交），中断后重跑会直接读取已完成的段；删除该文件夹即全部重算。
library(readxl)
library(dplyr)
library(writexl)
library(forecast)
library(xgboost)
library(keras3)
library(ggplot2)
freeze_end <- as.Date("2026-08-31")
new_end_date <- as.Date("2026-09-30")
output_file <- "9月检验与实际预测.xlsx"
figure_files <- c("fig5_september_check.png", "fig6_october_forecast.png")
checkpoint_dir <- file.path("results", "04_中间结果")
existing <- list.files("results", full.names = TRUE)
existing_md5 <- tools::md5sum(existing[file_test("-f", existing) & !basename(existing) %in% c(output_file, figure_files)])
dir.create(checkpoint_dir, showWarnings = FALSE)
# 9 月五站温度按文件名中的站点代码对应 01 的站点键；"-" 为缺测，数值左侧有空格，文件为 UTF-8 带 BOM。
station_codes <- c(S24 = "Changi", S44 = "Jurong_West", S109 = "Ang_Mo_Kio", S104 = "Admiralty", S60 = "Sentosa_Island")
read_september <- function(code) {
raw <- read.csv(file.path("raw_202609", paste0("DAILYDATA_", code, "_202609.csv")), fileEncoding = "UTF-8-BOM", colClasses = "character", check.names = FALSE)
value <- trimws(raw[[grep("^Mean Temperature", names(raw))]])
value[value == "-"] <- NA_character_
x <- data.frame(Date = as.Date(sprintf("%s-%02d-%02d", trimws(raw$Year), as.integer(raw$Month), as.integer(raw$Day))), Value = as.numeric(value))
stopifnot(identical(x$Date, seq(freeze_end + 1L, new_end_date, by = "day")), identical(is.na(x$Value), is.na(value)))
names(x)[2] <- station_codes[[code]]
return(x)
}
september_temp <- Reduce(function(a, b) merge(a, b, by = "Date"), lapply(names(station_codes), read_september))
# 01 原样执行（不写文件），只在两处接入：研究截止日延长到 2026-09-30；TempSG.xlsx 读入后接上 9 月五站温度。缺测仍由 prepare_features() 按 01 的 Ridge 方法补值。
hooks <- c(end_date = 0L, temperature = 0L)
for (e in as.list(parse("01_数据处理与变量生成.R", encoding = "UTF-8"))) {
if (is.call(e) && deparse(e[[1]]) %in% c("write_xlsx", "saveRDS", "message")) next
eval(e, globalenv())
if (identical(e, quote(end_date <- as.Date("2026-08-31")))) {
end_date <- new_end_date
hooks["end_date"] <- hooks["end_date"] + 1L
}
if (identical(e, quote(temp$Date <- as.Date(temp$Date)))) {
stopifnot(max(temp$Date) == freeze_end)
temp <- rbind(temp, september_temp[c("Date", stations)])
hooks["temperature"] <- hooks["temperature"] + 1L
}
}
stopifnot(all(hooks == 1L))
daily$Sample[daily$Date > freeze_end & daily$Date <= end_date] <- "PostFreeze"
frozen_daily <- readRDS(file.path(output_dir, "数据处理对象.rds"))$daily
stopifnot(isTRUE(all.equal(daily[daily$Date <= freeze_end, ], frozen_daily[frozen_daily$Date <= freeze_end, ])), !anyNA(daily$DailyUSEP), max(daily$Date) == end_date + 1L)
# 样本日期、MASE 分母和尖峰阈值取自 02 的赋值语句，模型函数与 02 共用；02 的建模和写文件语句不执行。
from_02 <- c("validation_dates", "test_dates", "train_y", "mase_scale", "spike_threshold")
for (e in as.list(parse("02_模型验证与研究结果.R", encoding = "UTF-8"))) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% from_02) eval(e, globalenv())
source("模型函数.R", encoding = "UTF-8")
freeze <- readRDS(file.path(output_dir, "冻结的研究设置.rds"))
stopifnot(isTRUE(all.equal(mase_scale, freeze$mase_scale)), isTRUE(all.equal(spike_threshold, freeze$spike_threshold)), identical(temperature_lambda, freeze$temperature_lambda), max(test_dates) == freeze_end)
read_dated <- function(sheet) {
x <- as.data.frame(read_excel(file.path(output_dir, "逐日预测与模型变量.xlsx"), sheet = sheet))
x$Date <- as.Date(x$Date)
x$PreviousDay <- daily$DailyUSEP[match(x$Date - 1L, daily$Date)]
return(x)
}
validation <- read_dated("Validation")
test <- read_dated("Test")
stopifnot(identical(validation$Date, validation_dates), identical(test$Date, test_dates))
kinds <- c("SeasonalNaive", "PreviousDay", "ETS", "SARIMA", "TSLM", "DynamicRegression", "XGBoost", "LSTM")
# 每段结果单独保存；文件已存在时直接读取，不重新计算。
checkpoint <- function(name, value) {
file <- file.path(checkpoint_dir, paste0(name, ".rds"))
if (file.exists(file)) return(readRDS(file))
saveRDS(value, file)
message(format(Sys.time(), "%H:%M:%S"), " 已保存 ", name)
return(value)
}
# 9 月独立检验：冻结设置下逐日一日超前预测，每 7 日扩展重估；LSTM 取三种子均值。
september_dates <- seq(freeze_end + 1L, end_date, by = "day")
september <- data.frame(Date = september_dates, Actual = daily$DailyUSEP[match(september_dates, daily$Date)], PreviousDay = daily$DailyUSEP[match(september_dates - 1L, daily$Date)])
september$SeasonalNaive <- rolling_predict("SeasonalNaive", list(), september_dates)
for (kind in c("ETS", "SARIMA", "TSLM", "DynamicRegression", "XGBoost")) september[[kind]] <- checkpoint(paste0("september_", kind), rolling_predict(kind, freeze$models[[kind]]$config, september_dates, 4L, freeze$models[[kind]]$tpc))
september$LSTM <- rowMeans(sapply(freeze$seeds, function(seed) checkpoint(paste0("september_LSTM_", seed), rolling_predict("LSTM", freeze$models$LSTM$config, september_dates, 4L, freeze$models$LSTM$tpc, seed))))
# 9-01 的截止日与 02 的 NextDay 相同，可用来核对本环境能否复现冻结预测。
frozen_next <- as.data.frame(read_excel(file.path(output_dir, "研究结果.xlsx"), sheet = "NextDay"))
reproduce <- data.frame(Date = september_dates[1], Model = frozen_next$Model, Frozen02 = frozen_next$Prediction, Rerun04 = unlist(september[1, frozen_next$Model]))
reproduce$Difference <- reproduce$Rerun04 - reproduce$Frozen02
# 区间沿用 02 的规则：只用此前已实现的绝对误差，最多 180 条，有限样本分位秩向上取整；2025 验证与 2026 测试误差作为初始校准，9 月误差逐日加入。误差一律为实际减预测，正值表示低估。
rank_quantile <- function(x, level) sort(x)[min(length(x), ceiling((length(x) + 1L) * level))]
interval_radius <- function(errors, level) {
calibration <- tail(abs(errors), freeze$interval_window)
stopifnot(length(calibration) >= freeze$interval_min)
return(rank_quantile(calibration, level))
}
past_errors <- lapply(setNames(kinds, kinds), function(kind) c(validation$Actual - validation[[kind]], test$Actual - test[[kind]]))
september_daily <- data.frame()
for (kind in kinds) {
for (i in seq_along(september_dates)) {
prediction <- september[[kind]][i]
radius <- c(interval_radius(past_errors[[kind]], 0.80), interval_radius(past_errors[[kind]], 0.95))
september_daily <- rbind(september_daily, data.frame(Date = september_dates[i], Model = kind, Actual = september$Actual[i], Prediction = prediction, Error = september$Actual[i] - prediction, Lower80 = prediction - radius[1], Upper80 = prediction + radius[1], Lower95 = prediction - radius[2], Upper95 = prediction + radius[2]))
past_errors[[kind]] <- c(past_errors[[kind]], september$Actual[i] - prediction)
}
}
september_daily$Covered80 <- september_daily$Actual >= september_daily$Lower80 & september_daily$Actual <= september_daily$Upper80
september_daily$Covered95 <- september_daily$Actual >= september_daily$Lower95 & september_daily$Actual <= september_daily$Upper95
september_scores <- bind_rows(lapply(kinds, function(kind) {
x <- september_daily[september_daily$Model == kind, ]
cbind(Model = kind, score(x$Actual, x$Prediction), MeanError = mean(x$Error), Coverage80 = mean(x$Covered80), Coverage95 = mean(x$Covered95), MeanWidth95 = mean(x$Upper95 - x$Lower95), TestRMSE = score(test$Actual, test[[kind]])$RMSE)
}))
september_scores <- september_scores[order(september_scores$RMSE), ]
# DM 检验同 03：forecast::dm.test，h=1，含 HLN 修正；统计量为负表示 LSTM 损失更小。n=30，只作描述性参考。
september_dm <- bind_rows(lapply(setdiff(kinds, "LSTM"), function(other) bind_rows(lapply(c(2, 1), function(power) {
e1 <- september$Actual - september$LSTM
e2 <- september$Actual - september[[other]]
dm <- dm.test(e1, e2, alternative = "two.sided", h = 1L, power = power)
data.frame(Model1 = "LSTM", Model2 = other, Sample = "September 2026", Loss = if (power == 2) "Squared" else "Absolute", N = length(e1), MeanLossDifference = mean(abs(e1)^power - abs(e2)^power), DM = unname(dm$statistic), p = dm$p.value)
}))))
# 10 月路径为情景预测：LSTM 递归，价格滞后用前一步预测值；需求、备用、调频、BrentSGD 和五站温度取截止日前 7 日均值并保持不变；星期与公众假期按真实日历。
scenario_columns <- c("DemandMean", "PrimaryReserve", "ContingencyReserve", "Regulation", "BrentSGD", stations)
calendar_rows <- function(dates) {
x <- data.frame(Date = dates)
x$DayOfWeek <- (as.POSIXlt(x$Date)$wday + 6L) %% 7L
x$Weekend <- as.integer(x$DayOfWeek >= 5L)
x$PublicHoliday <- as.integer(x$Date %in% holiday_dates)
x$Monday <- as.integer(x$DayOfWeek == 0L)
x$Tuesday <- as.integer(x$DayOfWeek == 1L)
x$Wednesday <- as.integer(x$DayOfWeek == 2L)
x$Thursday <- as.integer(x$DayOfWeek == 3L)
x$Saturday <- as.integer(x$DayOfWeek == 5L)
x$TPCRegime <- as.integer(x$Date >= as.Date("2023-07-01"))
return(x)
}
stopifnot(isTRUE(all.equal(calendar_rows(daily$Date), daily[names(calendar_rows(daily$Date[1]))])))
# prepare_features() 读取全局 daily；这里让它改读含情景值的路径数据，特征计算本身不变。
prepare_on <- function(data, cutoff, through) {
f <- prepare_features
environment(f) <- list2env(list(daily = data), parent = environment(prepare_features))
return(f(cutoff, through))
}
lstm_config <- freeze$models$LSTM$config
lstm_features <- feature_names("LSTM", 4L, freeze$models$LSTM$tpc)
# 训练步骤与 rolling_predict() 中的 LSTM 相同：截止日前的历史、28 日预热、训练内标准化与早停。
fit_lstm_at <- function(cutoff, seed) {
dat <- prepare_features(cutoff, cutoff)
return(fit_lstm(as.matrix(dat[lstm_features]), dat$DailyUSEP, which(dat$Date <= cutoff & dat$Date >= warmup_date), lstm_config, seed))
}
# 三个种子的模型在截止日各训练一次；每一步取三种子均值作为预测，并作为下一步的价格滞后。第 1 步的输入全部为已实现数据，即一日超前预测。
recursive_lstm <- function(origin, horizon) {
cutoff <- origin - 1L
dates <- origin + seq_len(horizon) - 1L
filled <- prepare_features(cutoff, cutoff)
scenario <- colMeans(filled[filled$Date > cutoff - 7L, scenario_columns])
future <- calendar_rows(dates)
future[scenario_columns] <- as.list(scenario)
future$Sample <- "Scenario"
path <- bind_rows(daily[daily$Date <= cutoff, ], future)
models <- lapply(freeze$seeds, function(seed) fit_lstm_at(cutoff, seed))
prediction <- numeric(horizon)
for (h in seq_len(horizon)) {
dat <- prepare_on(path, cutoff, dates[h])
x <- as.matrix(dat[lstm_features])
current <- match(dates[h], dat$Date)
prediction[h] <- mean(vapply(models, function(model) as.numeric(predict(model$model, make_sequences(transform_x(x, model$scaler, TRUE), current, lstm_config$sequence), verbose = 0L)) * model$y_sd + model$y_mean, numeric(1)))
path$DailyUSEP[path$Date == dates[h]] <- prediction[h]
}
stopifnot(all(is.finite(prediction)))
return(list(path = data.frame(Origin = origin, Date = dates, Horizon = seq_len(horizon), Prediction = prediction), scenario = scenario))
}
# 递归回测：以 2026 测试期每周重估日为起点，凡 31 日路径在数据截止日（2026-09-30）前已全部实现的都纳入，按步长取实际减预测误差。
horizon <- 31L
origins <- test_dates[seq(1L, length(test_dates), by = refit_days)]
origins <- origins[origins + horizon - 1L <= new_end_date]
backtest <- bind_rows(lapply(origins, function(origin) checkpoint(paste0("backtest_", origin), recursive_lstm(origin, horizon))$path))
backtest$Actual <- daily$DailyUSEP[match(backtest$Date, daily$Date)]
backtest$Error <- backtest$Actual - backtest$Prediction
horizon_errors <- backtest |> group_by(Horizon) |> summarise(N = n(), MAE = mean(abs(Error)), RMSE = sqrt(mean(Error^2)), MeanError = mean(Error), Radius80 = rank_quantile(abs(Error), 0.80), Radius95 = rank_quantile(abs(Error), 0.95), .groups = "drop") |> as.data.frame()
# 每个步长只有约 35 个误差，分位数随步长跳动；区间半径取到该步长为止的累计最大值，使区间随步长单调变宽。
horizon_errors$Radius80_Band <- cummax(horizon_errors$Radius80)
horizon_errors$Radius95_Band <- cummax(horizon_errors$Radius95)
# 以 2026-09-30 为数据截止：LSTM 10 月路径，区间为点预测加减上述单调化的同步长回测误差分位数。
next_date <- end_date + 1L
october_dates <- next_date + seq_len(horizon) - 1L
october_lstm <- checkpoint("october_LSTM", recursive_lstm(next_date, horizon))
october <- data.frame(Date = october_dates, Horizon = seq_len(horizon), Weekday = weekdays(october_dates), PublicHoliday = as.integer(october_dates %in% holiday_dates), LSTM = october_lstm$path$Prediction)
october[c("LSTM_Lower80", "LSTM_Upper80")] <- list(october$LSTM - horizon_errors$Radius80_Band, october$LSTM + horizon_errors$Radius80_Band)
october[c("LSTM_Lower95", "LSTM_Upper95")] <- list(october$LSTM - horizon_errors$Radius95_Band, october$LSTM + horizon_errors$Radius95_Band)
# 对照：ETS、SARIMA 只用价格历史，按冻结规格在截止日拟合后直接做 31 步预测，区间为模型区间。
history <- ts(daily$DailyUSEP[daily$Date <= end_date], frequency = 7)
native <- list(ETS = ets(history, model = freeze$models$ETS$config$model, damped = freeze$models$ETS$config$damped), SARIMA = Arima(history, order = freeze$models$SARIMA$config$order, seasonal = list(order = freeze$models$SARIMA$config$seasonal, period = 7), include.constant = TRUE, method = "ML"))
for (kind in names(native)) {
fc <- forecast(native[[kind]], h = horizon, level = c(80, 95))
october[c(kind, paste0(kind, c("_Lower80", "_Upper80", "_Lower95", "_Upper95")))] <- list(as.numeric(fc$mean), as.numeric(fc$lower[, 1]), as.numeric(fc$upper[, 1]), as.numeric(fc$lower[, 2]), as.numeric(fc$upper[, 2]))
}
october$Actual <- daily$DailyUSEP[match(october_dates, daily$Date)]
scenario_inputs <- data.frame(Variable = names(october_lstm$scenario), Value = unname(october_lstm$scenario), Window = paste(end_date - 6L, "to", end_date), Note = "Held constant for every October date (scenario assumption); TempSG_model is the mean of the five stations")
# 2026-10-01 一日预测：截止日 2026-09-30；LSTM 即 10 月路径第 1 步。区间用验证、测试和 9 月的已实现误差（最多 180 条）。实际值仅作事后对照。
next_day <- data.frame(Date = next_date, Model = kinds, Prediction = NA_real_)
next_day$Prediction[next_day$Model == "SeasonalNaive"] <- rolling_predict("SeasonalNaive", list(), next_date)
next_day$Prediction[next_day$Model == "PreviousDay"] <- daily$DailyUSEP[daily$Date == end_date]
for (kind in c("ETS", "SARIMA", "TSLM", "DynamicRegression", "XGBoost")) next_day$Prediction[next_day$Model == kind] <- checkpoint(paste0("nextday_", kind), rolling_predict(kind, freeze$models[[kind]]$config, next_date, 4L, freeze$models[[kind]]$tpc))
next_day$Prediction[next_day$Model == "LSTM"] <- october$LSTM[1]
stopifnot(isTRUE(all.equal(next_day$Prediction[next_day$Model == "ETS"], october$ETS[1])), isTRUE(all.equal(next_day$Prediction[next_day$Model == "SARIMA"], october$SARIMA[1], tolerance = 1e-6)))
for (level in c(80, 95)) {
radius <- vapply(next_day$Model, function(kind) interval_radius(past_errors[[kind]], level / 100), numeric(1))
next_day[[paste0("Lower", level)]] <- next_day$Prediction - radius
next_day[[paste0("Upper", level)]] <- next_day$Prediction + radius
}
next_day$ActualExPost <- daily$DailyUSEP[daily$Date == next_date]
next_day$ErrorExPost <- next_day$ActualExPost - next_day$Prediction
write_xlsx(list(Sept_Daily = september_daily, Sept_Scores = september_scores, Sept_DM = september_dm, NextDay_20261001 = next_day, October_Path = october, Horizon_Errors = horizon_errors, Scenario_Inputs = scenario_inputs, Recursive_Backtest = backtest, Check_20260901 = reproduce), file.path(output_dir, output_file))
# 英文图，配色与 03 相同。
figure_theme <- theme_minimal(base_size = 10) + theme(legend.position = "top", panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0, colour = "#52514e", size = 7.5))
panel_models <- c("LSTM", "XGBoost", "ETS", "SARIMA")
panel_labels <- setNames(sprintf("%s (September RMSE %.2f)", panel_models, september_scores$RMSE[match(panel_models, september_scores$Model)]), panel_models)
fig5_data <- september_daily[september_daily$Model %in% panel_models, ]
fig5_data$Panel <- factor(panel_labels[fig5_data$Model], levels = panel_labels)
fig5 <- ggplot(fig5_data, aes(Date)) + geom_ribbon(aes(ymin = Lower95, ymax = Upper95, fill = "95% empirical interval")) + geom_line(aes(y = Prediction, colour = "Forecast"), linewidth = 0.45) + geom_line(aes(y = Actual, colour = "Actual"), linewidth = 0.4) + geom_point(aes(y = Actual, colour = "Actual"), size = 0.8) + facet_wrap(~Panel, ncol = 2) + scale_fill_manual(values = "#b7d3f6", name = NULL) + scale_colour_manual(values = c(Actual = "#0b0b0b", Forecast = "#2a78d6"), name = NULL) + labs(x = NULL, y = "Daily USEP (SGD/MWh)", caption = "Source: EMC USEP; MSS daily temperature for September 2026. One-day-ahead forecasts for 1-30 September 2026 under frozen\nsettings with 7-day expanding refits; intervals from up to 180 previously realised absolute errors. Authors' calculations in R.") + figure_theme
ggsave(file.path(output_dir, "fig5_september_check.png"), fig5, width = 7, height = 5.2, dpi = 300, bg = "white")
recent <- data.frame(Date = daily$Date[daily$Date >= as.Date("2026-08-01") & daily$Date <= end_date], Actual = daily$DailyUSEP[daily$Date >= as.Date("2026-08-01") & daily$Date <= end_date])
paths <- bind_rows(lapply(c("LSTM", "ETS", "SARIMA"), function(kind) data.frame(Date = october$Date, Series = paste(kind, "path"), Value = october[[kind]])))
paths$Series <- factor(paths$Series, levels = c("LSTM path", "ETS path", "SARIMA path"))
fig6 <- ggplot() + geom_ribbon(data = october, aes(Date, ymin = LSTM_Lower95, ymax = LSTM_Upper95, fill = "LSTM 95%")) + geom_ribbon(data = october, aes(Date, ymin = LSTM_Lower80, ymax = LSTM_Upper80, fill = "LSTM 80%")) + geom_vline(xintercept = end_date + 0.5, linetype = "dashed", colour = "#52514e", linewidth = 0.3) + geom_line(data = recent, aes(Date, Actual), colour = "#0b0b0b", linewidth = 0.4) + geom_line(data = paths, aes(Date, Value, colour = Series, linetype = Series), linewidth = 0.5) + geom_point(data = october[!is.na(october$Actual), ], aes(Date, Actual), colour = "#0b0b0b", size = 1.4) + annotate("text", x = end_date, y = Inf, label = "Data cut-off 2026-09-30", hjust = 1.05, vjust = 1.5, size = 2.8, colour = "#52514e") + scale_fill_manual(values = c("LSTM 95%" = "#cde2fb", "LSTM 80%" = "#86b6ef"), name = NULL) + scale_colour_manual(values = c("LSTM path" = "#2a78d6", "ETS path" = "#eb6834", "SARIMA path" = "#1baf7a"), name = NULL) + scale_linetype_manual(values = c("LSTM path" = "solid", "ETS path" = "dashed", "SARIMA path" = "dotdash"), name = NULL) + labs(x = NULL, y = "Daily USEP (SGD/MWh)", caption = "Source: EMC USEP; MSS daily temperature. Black line: actual daily USEP, August-September 2026; black dot: 1 October actual (ex post).\nLSTM path is recursive: price lags use earlier forecasts; demand, reserve, regulation, BrentSGD and temperature are held at their\n24-30 September means (scenario assumption). LSTM bands: running maximum over horizons of absolute-error quantiles from recursive\nback-tests starting weekly from 1 January to 27 August 2026. ETS and SARIMA: native 31-step forecasts. Authors' calculations in R.") + figure_theme
ggsave(file.path(output_dir, "fig6_october_forecast.png"), fig6, width = 7, height = 4.4, dpi = 300, bg = "white")
stopifnot(identical(tools::md5sum(names(existing_md5)), existing_md5))
message("9 月检验与实际预测完成：结果写入 ", file.path(output_dir, output_file), "；已有结果文件未改动。")
