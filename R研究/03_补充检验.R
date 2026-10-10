# 先完成 01、02；本文件只读取其冻结结果做补充检验，不重跑 02，不覆盖 results/ 中已有文件。
# 将工作目录设为本文件和八个 Excel 所在文件夹再运行；额外依赖 urca、ggplot2、png。
library(readxl)
library(dplyr)
library(writexl)
library(forecast)
library(urca)
library(ggplot2)
supplement_file <- "补充检验结果.xlsx"
residual_models <- c("ETS", "SARIMA", "TSLM", "DynamicRegression")
figure_files <- c("fig1_price_history.png", "fig2_acf_pacf.png", "fig3_test_predictions.png", "fig4_lstm_interval.png", paste0("fig_residuals_", residual_models, ".png"))
frozen_files <- file.path("results", setdiff(list.files("results"), c(supplement_file, figure_files)))
frozen_md5 <- tools::md5sum(frozen_files)
# 01 末尾会把数据处理结果写回 results/；这里只执行其余语句，取得 daily、initial_data 和 prepare_features()。
for (e in as.list(parse("01_数据处理与变量生成.R", encoding = "UTF-8"))) if (!(is.call(e) && deparse(e[[1]]) %in% c("write_xlsx", "saveRDS", "message"))) eval(e, globalenv())
stopifnot(isTRUE(all.equal(daily, readRDS(file.path(output_dir, "数据处理对象.rds"))$daily)))
# 评分、特征组合与缺值处理函数直接取自 02，保证与冻结结果同一定义；02 的建模和写文件语句不执行。
from_02 <- c("validation_dates", "test_dates", "train_y", "mase_scale", "spike_threshold", "score", "feature_names", "fit_scaler", "transform_x")
for (e in as.list(parse("02_模型验证与研究结果.R", encoding = "UTF-8"))) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% from_02) eval(e, globalenv())
freeze <- readRDS(file.path(output_dir, "冻结的研究设置.rds"))
stopifnot(isTRUE(all.equal(mase_scale, freeze$mase_scale)), isTRUE(all.equal(spike_threshold, freeze$spike_threshold)), identical(temperature_lambda, freeze$temperature_lambda))
read_dated <- function(sheet) {
x <- as.data.frame(read_excel(file.path(output_dir, "逐日预测与模型变量.xlsx"), sheet = sheet))
x$Date <- as.Date(x$Date)
return(x)
}
validation <- read_dated("Validation")
test <- read_dated("Test")
incremental <- read_dated("Incremental")
intervals <- read_dated("Intervals")
stopifnot(identical(validation$Date, validation_dates), identical(test$Date, test_dates), identical(incremental$Date, test_dates), isTRUE(all.equal(test$Actual, daily$DailyUSEP[match(test_dates, daily$Date)])))
# 前一日价格基准：用目标日前一天的日均 USEP 作为预测，与季节朴素一样不需要估计。
validation$PreviousDay <- daily$DailyUSEP[match(validation$Date - 1L, daily$Date)]
test$PreviousDay <- daily$DailyUSEP[match(test$Date - 1L, daily$Date)]
models <- c("SeasonalNaive", "PreviousDay", "ETS", "SARIMA", "TSLM", "DynamicRegression", "XGBoost", "LSTM")
samples <- list(Validation = validation, Test = test, Pooled = rbind(validation, test))
overall <- as.data.frame(read_excel(file.path(output_dir, "研究结果.xlsx"), sheet = "Overall"))
for (kind in overall$Model) stopifnot(isTRUE(all.equal(score(test$Actual, test[[kind]])$RMSE, overall$RMSE[overall$Model == kind])))
# 平稳性：初始训练期水平、对数、一阶差分和周季节差分。ADF 含截距、在最多 24 阶内按 AIC 选滞后；KPSS 检验水平平稳、短窗宽。urca 只给临界值，因此报告统计量与 5% 临界值。
stopifnot(all(train_y > 0))
forms <- list(Level = train_y, Log = log(train_y), FirstDifference = diff(train_y), WeeklySeasonalDifference = diff(train_y, lag = 7L))
stationarity <- data.frame()
for (form in names(forms)) {
adf <- ur.df(forms[[form]], type = "drift", lags = 24L, selectlags = "AIC")
kpss <- ur.kpss(forms[[form]], type = "mu", lags = "short")
adf_lags <- sum(grepl("^z.diff.lag", rownames(coef(adf@testreg))))
stationarity <- rbind(stationarity, data.frame(Series = form, N = length(forms[[form]]), ADF_Lags = adf_lags, ADF_Tau = adf@teststat[1, "tau2"], ADF_CV5 = adf@cval["tau2", "5pct"], ADF_RejectUnitRoot5 = adf@teststat[1, "tau2"] < adf@cval["tau2", "5pct"], KPSS_Bandwidth = kpss@lag, KPSS_Stat = kpss@teststat, KPSS_CV5 = kpss@cval[1, "5pct"], KPSS_RejectStationarity5 = kpss@teststat > kpss@cval[1, "5pct"]))
}
# 基准：两种朴素预测的验证和测试误差；各模型 MAE 与两种基准 MAE 之比，小于 1 表示优于基准。
benchmark <- data.frame()
for (period in c("Validation", "Test")) {
p <- samples[[period]]
for (kind in models) benchmark <- rbind(benchmark, cbind(Period = period, Model = kind, score(p$Actual, p[[kind]]), MAERatioVsSeasonalNaive = mean(abs(p[[kind]] - p$Actual)) / mean(abs(p$SeasonalNaive - p$Actual)), MAERatioVsPreviousDay = mean(abs(p[[kind]] - p$Actual)) / mean(abs(p$PreviousDay - p$Actual))))
}
# 样本外一步误差定义为预测减实际；Ljung-Box 滞后 14，样本外误差不扣模型自由度。
ljung_box <- data.frame()
for (kind in models) {
row <- data.frame(Model = kind)
for (period in c("Validation", "Test")) {
error <- samples[[period]][[kind]] - samples[[period]]$Actual
lb <- Box.test(error, lag = 14L, type = "Ljung-Box")
row[paste0(period, c("_N", "_Q", "_df", "_p"))] <- list(length(error), unname(lb$statistic), unname(lb$parameter), lb$p.value)
}
row$Test_MeanError <- mean(test[[kind]] - test$Actual)
ljung_box <- rbind(ljung_box, row)
}
# 样本内残差：在初始训练期末按冻结规格各拟合一次，即 02 在 2024-12-31 截止时的首个模型。
train_index <- which(initial_data$Date >= warmup_date & initial_data$Date <= train_end)
fit_in_sample <- function(kind) {
config <- freeze$models[[kind]]$config
if (kind == "ETS") return(ets(ts(train_y, frequency = 7), model = config$model, damped = config$damped))
if (kind == "SARIMA") return(Arima(ts(train_y, frequency = 7), order = config$order, seasonal = list(order = config$seasonal, period = 7), include.constant = TRUE, method = "ML"))
x <- as.matrix(initial_data[feature_names(kind, 4L, freeze$models[[kind]]$tpc)])
plain_x <- transform_x(x, fit_scaler(x[train_index, , drop = FALSE]))
y <- initial_data$DailyUSEP
if (kind == "DynamicRegression") return(Arima(ts(y[train_index], frequency = 7), xreg = plain_x[train_index, , drop = FALSE], order = config$order, seasonal = list(order = config$seasonal, period = 7), include.constant = TRUE, method = "ML"))
model <- lm(DailyUSEP ~ ., data = data.frame(DailyUSEP = y[train_index], plain_x[train_index, , drop = FALSE]))
model$method <- "TSLM"
return(model)
}
# checkresiduals() 整页绘图，不能附图注；保存后在图片下方补一行来源说明。
add_source <- function(file, note) {
image <- png::readPNG(file)
band <- 80L
png(file, width = ncol(image), height = nrow(image) + band, res = 220)
grid::grid.raster(image, y = 1, height = nrow(image) / (nrow(image) + band), just = "top")
grid::grid.text(note, x = 0.02, y = band / 2 / (nrow(image) + band), just = "left", gp = grid::gpar(fontsize = 7, col = "#52514e"))
dev.off()
}
# Ljung-Box 滞后 14，自由度按 checkresiduals() 默认：ARIMA 类扣除 p+q+P+Q，ETS 与回归不扣；TSLM 另报 checkresiduals() 对 lm 默认的 Breusch-Godfrey 检验。
residual_tests <- data.frame()
for (kind in residual_models) {
model <- fit_in_sample(kind)
specification <- if (kind == "TSLM") paste("OLS with", length(coef(model)) - 1L, "regressors") else as.character(model)
figure <- file.path(output_dir, paste0("fig_residuals_", kind, ".png"))
png(figure, width = 2000, height = 1400, res = 220)
lb <- checkresiduals(model, lag = 14L, test = "LB")
dev.off()
add_source(figure, paste0("Source: EMC USEP and model inputs, training period to 2024-12-31; residuals from ", specification, " under frozen settings; authors' calculations in R."))
residual_tests <- rbind(residual_tests, data.frame(Model = kind, Specification = specification, N = length(residuals(model)), Test = "Ljung-Box", Lag = 14L, ModelDF = 14L - unname(lb$parameter), Statistic = unname(lb$statistic), df = unname(lb$parameter), p = lb$p.value))
if (kind == "TSLM") {
bg <- checkresiduals(model, lag = 14L, plot = FALSE)
residual_tests <- rbind(residual_tests, data.frame(Model = kind, Specification = specification, N = length(residuals(model)), Test = "Breusch-Godfrey", Lag = 14L, ModelDF = NA_integer_, Statistic = unname(bg$statistic), df = unname(bg$parameter), p = bg$p.value))
}
}
# DM 检验用 forecast::dm.test（h=1，含 HLN 小样本修正）；统计量为负表示 Model1 损失更小。合并样本为 2025 验证加 2026 测试。
dm_row <- function(first, second, sample, power, data = samples[[sample]]) {
e1 <- data[[first]] - data$Actual
e2 <- data[[second]] - data$Actual
dm <- dm.test(e1, e2, alternative = "two.sided", h = 1L, power = power)
return(data.frame(Model1 = first, Model2 = second, Sample = sample, Loss = if (power == 2) "Squared" else "Absolute", N = length(e1), MeanLossDifference = mean(abs(e1)^power - abs(e2)^power), DM = unname(dm$statistic), p = dm$p.value))
}
dm_lstm <- bind_rows(lapply(setdiff(models, "LSTM"), function(other) rbind(dm_row("LSTM", other, "Test", 2), dm_row("LSTM", other, "Test", 1), dm_row("LSTM", other, "Pooled", 2))))
block_pairs <- list(c("ETS", "SARIMA"), c("DynamicRegression", "ETS"), c("TSLM", "DynamicRegression"), c("XGBoost", "ETS"), c("XGBoost", "SARIMA"))
dm_blocks <- bind_rows(lapply(block_pairs, function(pair) dm_row(pair[1], pair[2], "Test", 2)))
dm_blocks <- rbind(dm_blocks, dm_row("DynamicRegression_M2", "DynamicRegression_M1", "Test", 2, incremental), dm_row("XGBoost_M2", "XGBoost_M1", "Test", 2, incremental))
# 描述性诊断；偏度为矩估计 g1 = m3 / m2^1.5。滞后相关只用训练期内前一日已有观测的日期。
skewness <- function(x) mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
train_days <- daily[daily$Date <= train_end, ]
study_days <- daily[daily$Date <= end_date, ]
test_days <- daily[daily$Sample == "Test", ]
lagged <- initial_data[!is.na(initial_data$PrimaryReserve_lag1), ]
diagnostics <- rbind(
data.frame(Item = "MASE denominator", Sample = "Train", N = length(train_y) - 7L, Value = mase_scale, Note = "Mean |y_t - y_(t-7)|, 2021-01-08 to 2024-12-31; same denominator for all models and periods"),
data.frame(Item = "Skewness of daily USEP (level)", Sample = c("Train", "Full sample"), N = c(nrow(train_days), nrow(study_days)), Value = c(skewness(train_days$DailyUSEP), skewness(study_days$DailyUSEP)), Note = "g1 = m3 / m2^1.5"),
data.frame(Item = "Skewness of daily USEP (log)", Sample = c("Train", "Full sample"), N = c(nrow(train_days), nrow(study_days)), Value = c(skewness(log(train_days$DailyUSEP)), skewness(log(study_days$DailyUSEP))), Note = "g1 = m3 / m2^1.5"),
data.frame(Item = paste("Correlation with same-day USEP:", c("ContingencyReserve_lag1", "Regulation_lag1", "PrimaryReserve_lag1")), Sample = "Train", N = nrow(lagged), Value = c(cor(lagged$ContingencyReserve_lag1, lagged$DailyUSEP), cor(lagged$Regulation_lag1, lagged$DailyUSEP), cor(lagged$PrimaryReserve_lag1, lagged$DailyUSEP)), Note = "Pearson; 2021-01-02 to 2024-12-31"),
data.frame(Item = paste("PrimaryReserve", c("mean", "median", "skewness")), Sample = "Train", N = nrow(train_days), Value = c(mean(train_days$PrimaryReserve), median(train_days$PrimaryReserve), skewness(train_days$PrimaryReserve)), Note = "Daily mean of 48 periods, SGD/MWh"),
data.frame(Item = paste("PrimaryReserve", c("mean", "median", "skewness")), Sample = "Test", N = nrow(test_days), Value = c(mean(test_days$PrimaryReserve), median(test_days$PrimaryReserve), skewness(test_days$PrimaryReserve)), Note = "Daily mean of 48 periods, SGD/MWh"),
data.frame(Item = "Correlation: ContingencyReserve vs Regulation", Sample = "Train", N = nrow(train_days), Value = cor(train_days$ContingencyReserve, train_days$Regulation), Note = "Pearson; same-day daily means"),
data.frame(Item = "LSTM mean error", Sample = "Test", N = nrow(test), Value = mean(test$LSTM - test$Actual), Note = "Prediction minus actual, SGD/MWh; negative = under-prediction on average"))
# 增量表累计与边际改善按未四舍五入的测试 RMSE 重新计算，并与 02 的结果核对。
incremental_scores <- as.data.frame(read_excel(file.path(output_dir, "研究结果.xlsx"), sheet = "Incremental"))
for (kind in c("DynamicRegression", "XGBoost")) {
rmse <- vapply(0:4, function(step) score(test$Actual, incremental[[paste0(kind, "_M", step)]])$RMSE, numeric(1))
stopifnot(isTRUE(all.equal(rmse, incremental_scores$RMSE[incremental_scores$Model == kind])))
diagnostics <- rbind(diagnostics, data.frame(Item = paste0("Incremental ", kind, " M", 1:4, " cumulative RMSE improvement vs M0"), Sample = "Test", N = nrow(test), Value = 1 - rmse[2:5] / rmse[1], Note = "Unrounded; 1 - RMSE(Mk) / RMSE(M0)"))
diagnostics <- rbind(diagnostics, data.frame(Item = paste0("Incremental ", kind, " M", 1:4, " marginal RMSE improvement vs previous step"), Sample = "Test", N = nrow(test), Value = 1 - rmse[2:5] / rmse[1:4], Note = "Unrounded; 1 - RMSE(Mk) / RMSE(Mk-1)"))
}
write_xlsx(list(Stationarity = stationarity, Benchmark = benchmark, LjungBox_OOS = ljung_box, Residuals_InSample = residual_tests, DM_LSTM = dm_lstm, DM_Blocks = dm_blocks, Diagnostics = diagnostics), file.path(output_dir, supplement_file))
# 报告用英文图；配色为固定分类顺序，阈值与参考线用中性灰。
figure_theme <- theme_minimal(base_size = 10) + theme(legend.position = "top", panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0, colour = "#52514e", size = 7.5))
sample_labels <- c(Train = "Training (2021-2024)", Validation = "Validation (2025)", Test = "Test (Jan-Aug 2026)")
history <- study_days[c("Date", "DailyUSEP", "Sample")]
history$Sample <- factor(sample_labels[history$Sample], levels = sample_labels)
fig1 <- ggplot(history, aes(Date, DailyUSEP, colour = Sample)) + geom_line(linewidth = 0.3) + geom_hline(yintercept = spike_threshold, linetype = "dashed", colour = "#52514e", linewidth = 0.4) + annotate("text", x = max(history$Date), y = spike_threshold, label = sprintf("Training 95th percentile: %.2f", spike_threshold), hjust = 1, vjust = -0.6, size = 2.8, colour = "#52514e") + scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a"), name = NULL) + labs(x = NULL, y = "Daily USEP (SGD/MWh)", caption = "Source: EMC half-hourly USEP, daily mean of 48 periods, 2021-01-01 to 2026-08-31; authors' calculations in R.") + figure_theme
ggsave(file.path(output_dir, "fig1_price_history.png"), fig1, width = 7, height = 3.6, dpi = 300, bg = "white")
acf_values <- Acf(train_y, lag.max = 28L, plot = FALSE)
pacf_values <- Pacf(train_y, lag.max = 28L, plot = FALSE)
correlogram <- rbind(data.frame(Panel = "ACF", Lag = as.numeric(acf_values$lag), Value = as.numeric(acf_values$acf)), data.frame(Panel = "PACF", Lag = as.numeric(pacf_values$lag), Value = as.numeric(pacf_values$acf)))
correlogram <- correlogram[correlogram$Lag > 0, ]
bound <- qnorm(0.975) / sqrt(length(train_y))
fig2 <- ggplot(correlogram, aes(Lag, Value)) + geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.3) + geom_hline(yintercept = c(-bound, bound), linetype = "dashed", colour = "#52514e", linewidth = 0.3) + geom_segment(aes(xend = Lag, yend = 0), colour = "#2a78d6", linewidth = 0.6) + facet_wrap(~Panel, nrow = 1) + scale_x_continuous(breaks = seq(0, 28, 7)) + labs(x = "Lag (days)", y = "Correlation", caption = "Source: EMC USEP, training period 2021-01-01 to 2024-12-31; dashed lines are approximate 95% bounds\n(+/-1.96/sqrt(n)); authors' calculations in R.") + figure_theme
ggsave(file.path(output_dir, "fig2_acf_pacf.png"), fig2, width = 7, height = 3.2, dpi = 300, bg = "white")
panel_models <- c("ETS", "XGBoost", "LSTM")
panel_labels <- setNames(sprintf("%s (test RMSE %.2f)", panel_models, vapply(panel_models, function(kind) score(test$Actual, test[[kind]])$RMSE, numeric(1))), panel_models)
fig3_data <- bind_rows(lapply(panel_models, function(kind) data.frame(Model = panel_labels[[kind]], Date = rep(test$Date, 2L), Series = rep(c("Actual", "Forecast"), each = nrow(test)), Value = c(test$Actual, test[[kind]]))))
fig3_data$Model <- factor(fig3_data$Model, levels = panel_labels)
fig3 <- ggplot(fig3_data, aes(Date, Value, colour = Series)) + geom_line(linewidth = 0.35) + facet_wrap(~Model, ncol = 1) + scale_colour_manual(values = c(Actual = "#0b0b0b", Forecast = "#2a78d6"), name = NULL) + labs(x = NULL, y = "Daily USEP (SGD/MWh)", caption = "Source: EMC USEP; one-day-ahead rolling forecasts, 2026-01-01 to 2026-08-31,\nfrozen settings with 7-day expanding refits; authors' calculations in R.") + figure_theme
ggsave(file.path(output_dir, "fig3_test_predictions.png"), fig3, width = 7, height = 6.5, dpi = 300, bg = "white")
lstm_interval <- intervals[intervals$Model == "LSTM" & abs(intervals$Level - 0.95) < 1e-9, ]
stopifnot(nrow(lstm_interval) == length(test_dates))
fig4 <- ggplot(lstm_interval, aes(Date)) + geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = "95% empirical interval")) + geom_line(aes(y = Prediction, colour = "LSTM forecast"), linewidth = 0.4) + geom_line(aes(y = Actual, colour = "Actual"), linewidth = 0.35) + scale_fill_manual(values = "#b7d3f6", name = NULL) + scale_colour_manual(values = c(Actual = "#0b0b0b", "LSTM forecast" = "#2a78d6"), name = NULL) + labs(x = NULL, y = "Daily USEP (SGD/MWh)", caption = sprintf("Source: EMC USEP; LSTM mean of three seeds. Interval radius = empirical quantile of up to 180 previously realised absolute errors\n(2025 validation errors as initial calibration); empirical coverage %.1f%%; authors' calculations in R.", 100 * mean(lstm_interval$Covered))) + figure_theme
ggsave(file.path(output_dir, "fig4_lstm_interval.png"), fig4, width = 7, height = 3.6, dpi = 300, bg = "white")
stopifnot(identical(tools::md5sum(frozen_files), frozen_md5))
message("补充检验完成：结果写入 ", file.path(output_dir, supplement_file), "；冻结结果文件未改动。")
