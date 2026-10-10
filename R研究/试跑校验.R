# Rscript 试跑校验.R results/smoke_<run>；只读工作簿，不运行任何模型。
suppressPackageStartupMessages({library(readxl); library(dplyr)})
out <- commandArgs(trailingOnly = TRUE)[1]
stopifnot(length(out) == 1L, dir.exists(out))
for (id in c("05", "06", "07")) {
  file <- file.path(out, paste0("扩充_", id, ".xlsx"))
  stopifnot(file.exists(file))
  d <- as.data.frame(read_excel(file, sheet = "Daily"))
  t <- as.data.frame(read_excel(file, sheet = "Tuning"))
  f <- as.data.frame(read_excel(file, sheet = "Frozen"))
  s <- as.data.frame(read_excel(file, sheet = "SeedDaily"))
  stopifnot(identical(names(d), c("Date", "Period", "Model", "Prediction", "Actual")),
    all(c("Model", "Candidate", "Settings", "ValidationRMSE", "Seconds", "Failure") %in% names(t)),
    all(d$Period == "Validation"), !anyNA(d), all(is.finite(d$Prediction)),
    !anyDuplicated(d[c("Date", "Period", "Model")]), all(table(d$Model) == 14L),
    min(as.Date(d$Date)) == as.Date("2025-01-01"), max(as.Date(d$Date)) == as.Date("2025-01-14"),
    all(is.na(t$Failure) | t$Failure == ""), all(t$N == 14L), all(is.finite(t$ValidationRMSE)), all(t$Mode == "smoke"),
    all(!f$IsFinal), all(f$Mode == "smoke"), all(as.Date(s$Cutoff) < as.Date(s$Date)),
    setequal(unique(as.Date(s$Cutoff)), as.Date(c("2024-12-31", "2025-01-07"))))
  expected_models <- switch(id, `05` = c("ETS", "SARIMA", paste0("DynamicRegression_M", 0:4), "DHR", "Prophet"),
    `06` = c("RandomForest", "SVR", "XGBoost_Transformed"), `07` = c("LSTM_Improved", "GRU", "NNAR"))
  stopifnot(setequal(f$Model, expected_models))
  for (model in expected_models) {
    target <- if (id == "05" || model == "NNAR") c("Level", "Log") else c("Level", "Log", "Difference")
    stopifnot(setequal(unique(t$Transform[t$Model == model]), target))
    if (id == "07") stopifnot(setequal(unique(s$Seed[s$Model == model]), 2026:2028))
    choice <- f[f$Model == model, ]
    selected_rmse <- t$ValidationRMSE[t$Model == model & t$Candidate == choice$Candidate]
    stopifnot(isTRUE(all.equal(mean(selected_rmse), choice$SelectionRMSE, tolerance = 1e-10)))
    selected <- s[s$Model == model, ]
    average <- aggregate(Prediction ~ Date, selected, mean)
    published <- d[d$Model == model, ]
    stopifnot(isTRUE(all.equal(average$Prediction[match(published$Date, average$Date)], published$Prediction, tolerance = 1e-10)))
  }
  cat("PASS", id, "schema/dates/14-days/all-models/transforms/seeds/ensemble/cutoffs/no-failures\n")
  print(t[, c("Model", "Candidate", "Seed", "Stage", "N", "Seconds", "Failure")], row.names = FALSE)
}
# 不运行模型，检查完整网格数量、原 02 的 XGBoost 抽样设置及目标还原。
source("06_机器学习扩充.R", encoding = "UTF-8")
stopifnot(length(p2_candidates("full")) == 90L, length(p2_candidates("smoke")) == 9L)
e <- new.env(parent = globalenv())
for (statement in as.list(parse("02_模型验证与研究结果.R", encoding = "UTF-8"))) {
  if (identical(statement, quote(set.seed(2026)))) eval(statement, e)
  if (is.call(statement) && identical(statement[[1]], as.name("<-")) && is.name(statement[[2]]) && as.character(statement[[2]]) %in% c("xgb_grid", "xgb_candidates")) eval(statement, e)
}
generated <- Filter(function(x) x$Model == "XGBoost_Transformed" && x$Transform == "Level", p2_candidates("full"))
stopifnot(identical(lapply(generated, `[[`, "Parameters"), e$xgb_candidates))
source("07_神经网络扩充.R", encoding = "UTF-8")
stopifnot(length(p3_candidates("full")) == 76L, length(p3_candidates("smoke")) == 10L)
dat <- data.frame(DailyUSEP = c(100, 120, 90), USEP_lag1 = c(80, 100, 120))
for (transform in c("Level", "Log", "Difference")) stopifnot(isTRUE(all.equal(phase_inverse(phase_target(dat, transform), dat, 1:3, transform), dat$DailyUSEP)))
cat("PASS full-grid counts (06=90,07=76), original XGBoost grid identity, all target inverses\n")
# 信息边界扰动检查：只在内存副本改变未来观测，原始文件不写回。
meta <- as.data.frame(read_excel(file.path(out, "扩充_07.xlsx"), sheet = "Metadata"))
snapshot <- file.path("results", "扩充_中间结果", "07", "smoke", meta$Value[meta$Item == "Fingerprint"], "data.rds")
list2env(readRDS(snapshot), globalenv())
for (statement in as.list(parse("01_数据处理与变量生成.R", encoding = "UTF-8")))
  if (is.call(statement) && identical(statement[[1]], as.name("<-")) && is.call(statement[[3]]) && identical(statement[[3]][[1]], as.name("function"))) eval(statement, globalenv())
source("模型函数.R", encoding = "UTF-8")
saved_daily <- daily
cutoff <- as.Date("2024-12-31"); target <- cutoff + 1L
a <- prepare_features(cutoff, cutoff + 7L)
cols <- c("DailyUSEP", "DemandMean", "PrimaryReserve", "ContingencyReserve", "Regulation", "BrentSGD", stations)
daily[daily$Date >= target, cols] <- daily[daily$Date >= target, cols] + 999
b <- prepare_features(cutoff, cutoff + 7L)
stopifnot(identical(a[a$Date == target, all_features], b[b$Date == target, all_features]))
daily <- saved_daily
inner_cutoff <- as.Date("2024-12-03")
a <- prepare_features(inner_cutoff, cutoff)
daily[daily$Date > inner_cutoff, stations] <- daily[daily$Date > inner_cutoff, stations] + 999
b <- prepare_features(inner_cutoff, cutoff)
stopifnot(identical(a[a$Date <= inner_cutoff, all_features], b[b$Date <= inner_cutoff, all_features]))
daily <- saved_daily
cat("PASS target-day/future observations cannot change target-day inputs; early-stop holdout temperature cannot change inner-training features\n")
