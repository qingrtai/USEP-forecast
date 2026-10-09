# 将工作目录设为本文件和八个 Excel 所在文件夹，再从头运行；原始 Excel 不会被修改。
# 首次使用时安装依赖：install.packages(c("readxl", "dplyr", "tidyr", "writexl", "forecast", "xgboost", "keras3"))
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
set.seed(2026)
data_dir <- "."
output_dir <- file.path(data_dir, "results")
dir.create(output_dir, showWarnings = FALSE)
start_date <- as.Date("2021-01-01")
end_date <- as.Date("2026-08-31")
train_end <- as.Date("2024-12-31")
validation_end <- as.Date("2025-12-31")
refit_days <- 7L
# 只读取原始数据列；Excel 右侧的来源说明不作为变量。
read_market <- function(file, value_column, variable) {
raw <- as.data.frame(read_excel(file.path(data_dir, file), sheet = "Raw", .name_repair = "minimal"))
raw <- data.frame(Date = as.Date(raw[["DATE"]]), Period = as.integer(raw[["PERIOD"]]), Value = as.numeric(raw[[value_column]]))
raw <- raw[order(raw$Date, raw$Period), ]
stopifnot(!anyDuplicated(raw[c("Date", "Period")]), all(raw$Period %in% 1:48), !anyNA(raw))
daily <- raw |> group_by(Date) |> summarise(N = n(), Value = mean(Value), .groups = "drop")
stopifnot(all(daily$N == 48L))
daily$N <- NULL
names(daily)[2] <- variable
return(as.data.frame(daily))
}
# 半小时价格和需求各按每天完整 48 个时段取算术平均；不删除负价格或尖峰。
usep <- read_market("USEP.xlsx", "USEP ($/MWh)", "DailyUSEP")
demand <- read_market("NEM_Demand_Forecast.xlsx", "DEMAND (MW)", "DemandMean")
primary <- read_market("Primary_Reserve_Price.xlsx", "PRICE ($/MWh)", "PrimaryReserve")
contingency <- read_market("Contingency_Reserve_Price.xlsx", "PRICE ($/MWh)", "ContingencyReserve")
regulation <- read_market("Regulation_Price.xlsx", "PRICE ($/MWh)", "Regulation")
# Brent 原文件按日期倒序排列，在 R 中转为升序。
brent_raw <- read_excel(file.path(data_dir, "BrentUSD.xlsx"), sheet = "Raw", .name_repair = "minimal")
brent <- data.frame(Date = as.Date(brent_raw[[1]]), BrentUSD = as.numeric(brent_raw[[2]]))
brent <- brent[order(brent$Date), ]
stopifnot(!anyDuplicated(brent$Date), !anyNA(brent))
# MAS 的年份、月份为分组标签；这里只恢复标签，尾部说明不当作汇率观测。
fx_raw <- read_excel(file.path(data_dir, "USD_SGD.xlsx"), sheet = "Raw", col_types = "text", .name_repair = "minimal")
fx <- data.frame(Year = fx_raw[[1]], Month = fx_raw[[2]], Day = fx_raw[[3]], USD_SGD = fx_raw[[4]])
fx[fx == ""] <- NA_character_
fx <- fx |> fill(Year, Month, .direction = "down")
fx$Day <- suppressWarnings(as.integer(fx$Day))
fx$USD_SGD <- suppressWarnings(as.numeric(fx$USD_SGD))
fx <- fx[!is.na(fx$Day) & !is.na(fx$USD_SGD), ]
fx$Month <- match(fx$Month, month.abb)
fx$Date <- as.Date(sprintf("%s-%02d-%02d", fx$Year, fx$Month, fx$Day))
fx <- fx[order(fx$Date), c("Date", "USD_SGD")]
stopifnot(!anyDuplicated(fx$Date), !anyNA(fx))
# 先建立更长的日历再向前延续报价，保证 2021 年初能用到 2020-12-31 的汇率。
fuel <- data.frame(Date = seq(min(min(brent$Date), min(fx$Date)), end_date + 1L, by = "day"))
brent$BrentSourceDate <- brent$Date
fx$FXSourceDate <- fx$Date
fuel <- fuel |> left_join(brent, by = "Date") |> left_join(fx, by = "Date") |> fill(BrentUSD, USD_SGD, BrentSourceDate, FXSourceDate, .direction = "down")
fuel$BrentSGD <- fuel$BrentUSD * fuel$USD_SGD
fuel <- fuel[fuel$Date >= start_date, ]
stopifnot(all(fuel$BrentSourceDate <= fuel$Date), all(fuel$FXSourceDate <= fuel$Date), all(fuel$USD_SGD[fuel$Date <= as.Date("2021-01-03")] == 1.3221))
# 五站原值与模型重建值分开保留；原始均值必须有五站观测才计算。
stations <- c("Changi", "Jurong_West", "Ang_Mo_Kio", "Admiralty", "Sentosa_Island")
temp <- as.data.frame(read_excel(file.path(data_dir, "TempSG.xlsx"), sheet = "TempSG_Five_Stations", .name_repair = "minimal"))
temp <- temp[c("Date", stations)]
temp$Date <- as.Date(temp$Date)
temp$N_stations_available <- rowSums(!is.na(temp[stations]))
temp$TempSG_raw <- rowMeans(temp[stations], na.rm = FALSE)
stopifnot(!anyDuplicated(temp$Date), !anyNA(temp$Changi))
# 假日含正式补休日和全国投票日；采用已修订的 2022、2023 年日期。
# 来源：https://www.mom.gov.sg/employment-practices/public-holidays
# 2022 修订：https://www.mom.gov.sg/newsroom/press-releases/2021/1021-revised-dates-for-hari-raya-puasa-and--hari-raya-haji-public-holidays-in-2022
# 2023 修订：https://www.mom.gov.sg/newsroom/press-releases/2022/0929-revised-date-for-vesak-day-2023
holidays_2021 <- paste0("2021-", c("01-01", "02-12", "02-13", "04-02", "05-01", "05-13", "05-26", "07-20", "08-09", "11-04", "12-25"))
holidays_2022 <- paste0("2022-", c("01-01", "02-01", "02-02", "04-15", "05-01", "05-02", "05-03", "05-15", "05-16", "07-10", "07-11", "08-09", "10-24", "12-25", "12-26"))
holidays_2023 <- paste0("2023-", c("01-01", "01-02", "01-22", "01-23", "01-24", "04-07", "04-22", "05-01", "06-02", "06-29", "08-09", "09-01", "11-12", "11-13", "12-25"))
holidays_2024 <- paste0("2024-", c("01-01", "02-10", "02-11", "02-12", "03-29", "04-10", "05-01", "05-22", "06-17", "08-09", "10-31", "12-25"))
holidays_2025 <- paste0("2025-", c("01-01", "01-29", "01-30", "03-31", "04-18", "05-01", "05-03", "05-12", "06-07", "08-09", "10-20", "12-25"))
holidays_2026 <- paste0("2026-", c("01-01", "02-17", "02-18", "03-21", "04-03", "05-01", "05-27", "05-31", "06-01", "08-09", "08-10", "11-08", "11-09", "12-25"))
holiday_dates <- as.Date(c(holidays_2021, holidays_2022, holidays_2023, holidays_2024, holidays_2025, holidays_2026))
# 增加 2026-09-01 一行，仅供最后一次历史条件下的一日预测，不进入测试评分。
daily <- data.frame(Date = seq(start_date, end_date + 1L, by = "day"))
daily <- daily |> left_join(usep, by = "Date") |> left_join(demand, by = "Date") |> left_join(primary, by = "Date") |> left_join(contingency, by = "Date") |> left_join(regulation, by = "Date") |> left_join(fuel, by = "Date") |> left_join(temp, by = "Date")
daily$DayOfWeek <- (as.POSIXlt(daily$Date)$wday + 6L) %% 7L
daily$Weekend <- as.integer(daily$DayOfWeek >= 5L)
daily$PublicHoliday <- as.integer(daily$Date %in% holiday_dates)
daily$Monday <- as.integer(daily$DayOfWeek == 0L)
daily$Tuesday <- as.integer(daily$DayOfWeek == 1L)
daily$Wednesday <- as.integer(daily$DayOfWeek == 2L)
daily$Thursday <- as.integer(daily$DayOfWeek == 3L)
daily$Saturday <- as.integer(daily$DayOfWeek == 5L)
daily$TPCRegime <- as.integer(daily$Date >= as.Date("2023-07-01"))
daily$Sample <- ifelse(daily$Date <= train_end, "Train", ifelse(daily$Date <= validation_end, "Validation", ifelse(daily$Date <= end_date, "Test", "Forecast")))
stopifnot(all(diff(daily$Date) == 1L), !anyNA(daily$DailyUSEP[daily$Date <= end_date]))
# 重建某站相对 Changi 的温差；其他站缺测温差用当前训练期中位数处理。
ridge_inputs <- function(dat, target, medians = NULL) {
others <- setdiff(stations, c("Changi", target))
x <- as.matrix(dat[others]) - dat$Changi
month <- as.integer(format(dat$Date, "%m"))
x <- cbind(x, MonthSin = sin(2 * pi * month / 12), MonthCos = cos(2 * pi * month / 12))
if (is.null(medians)) medians <- apply(x, 2, median, na.rm = TRUE)
for (j in seq_len(ncol(x))) x[is.na(x[, j]), j] <- medians[j]
return(list(x = x, medians = medians))
}
fit_station <- function(dat, target, lambda) {
observed <- dat[!is.na(dat[[target]]) & !is.na(dat$Changi), ]
stopifnot(nrow(observed) >= 180L)
input <- ridge_inputs(observed, target)
center <- colMeans(input$x)
scale <- apply(input$x, 2, sd)
scale[scale == 0] <- 1
x <- cbind(Intercept = 1, sweep(sweep(input$x, 2, center, "-"), 2, scale, "/"))
y <- observed[[target]] - observed$Changi
penalty <- diag(c(0, rep(lambda, ncol(x) - 1L)))
coef <- solve(crossprod(x) + penalty, crossprod(x, y))
return(list(target = target, medians = input$medians, center = center, scale = scale, coef = coef))
}
predict_station <- function(model, dat) {
input <- ridge_inputs(dat, model$target, model$medians)
x <- cbind(Intercept = 1, sweep(sweep(input$x, 2, model$center, "-"), 2, model$scale, "/"))
return(as.numeric(x %*% model$coef) + dat$Changi)
}
# 正则化只在 2021–2024 年内选择；三个时间段分别模拟连续 7 天和 36 天遮蔽。
temperature_cv <- data.frame()
temperature_lambda <- setNames(numeric(4L), stations[-1])
for (station in stations[-1]) {
for (lambda in c(0.1, 1, 10)) {
errors <- numeric()
for (cutoff in as.Date(c("2021-12-31", "2022-12-31", "2023-12-31"))) {
cutoff <- as.Date(cutoff, origin = "1970-01-01")
model <- fit_station(temp[temp$Date <= cutoff, ], station, lambda)
for (block in c(7L, 36L)) {
validation <- temp[temp$Date > cutoff & temp$Date <= cutoff + block & !is.na(temp[[station]]), ]
masked <- validation
masked[[station]] <- NA_real_
errors <- c(errors, predict_station(model, masked) - validation[[station]])
}
}
temperature_cv <- rbind(temperature_cv, data.frame(Station = station, Lambda = lambda, N = length(errors), MAE = mean(abs(errors)), RMSE = sqrt(mean(errors^2))))
}
candidate <- temperature_cv[temperature_cv$Station == station, ]
temperature_lambda[station] <- candidate$Lambda[which.min(candidate$RMSE)]
}
# 每次扩展训练时重新拟合站点模型，仅填缺失单元，实测温度保持不变。
prepare_features <- function(cutoff, through = cutoff + refit_days) {
dat <- daily[daily$Date <= through, ]
for (station in stations[-1]) {
model <- fit_station(dat[dat$Date <= cutoff, ], station, temperature_lambda[station])
missing <- which(is.na(dat[[station]]) & !is.na(dat$Changi))
dat[missing, station] <- predict_station(model, dat[missing, ])
}
dat$TempSG_model <- rowMeans(dat[stations], na.rm = FALSE)
dat$TempSG_reconstructed <- dat$N_stations_available < 5L
dat$USEP_lag1 <- lag(dat$DailyUSEP, 1L)
dat$USEP_lag2 <- lag(dat$DailyUSEP, 2L)
dat$USEP_lag7 <- lag(dat$DailyUSEP, 7L)
dat$USEP_MA7 <- vapply(seq_len(nrow(dat)), function(i) if (i > 7L) mean(dat$DailyUSEP[(i - 7L):(i - 1L)]) else NA_real_, numeric(1))
dat$USEP_SD7 <- vapply(seq_len(nrow(dat)), function(i) if (i > 7L) sd(dat$DailyUSEP[(i - 7L):(i - 1L)]) else NA_real_, numeric(1))
dat$DemandMean_lag1 <- lag(dat$DemandMean, 1L)
dat$PrimaryReserve_lag1 <- lag(dat$PrimaryReserve, 1L)
dat$ContingencyReserve_lag1 <- lag(dat$ContingencyReserve, 1L)
dat$Regulation_lag1 <- lag(dat$Regulation, 1L)
dat$BrentSGD_lag1 <- lag(dat$BrentSGD, 1L)
dat$TempSG_model_lag1 <- lag(dat$TempSG_model, 1L)
return(dat)
}
# 七日窗口与最长序列共用 28 日预热；LSTM 序列中的前七行滞后缺值用该次训练中位数处理。
price_features <- c("USEP_lag1", "USEP_lag2", "USEP_lag7", "USEP_MA7", "USEP_SD7")
external_features <- c("DemandMean_lag1", "PrimaryReserve_lag1", "ContingencyReserve_lag1", "Regulation_lag1", "BrentSGD_lag1", "TempSG_model_lag1", "Monday", "Tuesday", "Wednesday", "Thursday", "Saturday", "Weekend", "PublicHoliday")
all_features <- c(price_features, external_features)
warmup_date <- start_date + 28L
initial_data <- prepare_features(train_end, train_end)
write_xlsx(list(Daily_Raw = daily[daily$Date <= end_date, ], Train_Features = initial_data, Temperature_CV = temperature_cv, Holiday_Dates = data.frame(Date = holiday_dates)), file.path(output_dir, "数据处理结果.xlsx"))
saveRDS(list(daily = daily, temperature_lambda = temperature_lambda), file.path(output_dir, "数据处理对象.rds"))
message("数据处理完成：原始分类文件保留全部日期；研究样本为 2021-01-01 至 2026-08-31。")
