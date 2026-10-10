# 05–07 共用设施。工作目录必须为 R研究；默认 smoke，仅预测 2025-01-01..14。
# 不 source 02/04 的计算流程；只复用 01 数据接入及 模型函数.R 的纯函数。
phase_args <- function(id) {
  a <- commandArgs(trailingOnly = TRUE)
  get <- function(key, default) {
    x <- a[startsWith(a, paste0("--", key, "="))]
    if (length(x)) sub(paste0("^--", key, "="), "", tail(x, 1)) else default
  }
  mode <- get("mode", "smoke")
  stopifnot(mode %in% c("smoke", "full"))
  if (mode == "full" && Sys.getenv("USEP_FULL_APPROVED") != "yes")
    stop("全量运行尚需审核；审核后明确设置 USEP_FULL_APPROVED=yes。")
  workers <- as.integer(get("workers", "2"))
  stopifnot(is.finite(workers), workers >= 1L, workers <= 6L)
  run <- get("run", format(Sys.time(), "%Y%m%d_%H%M%S"))
  stopifnot(grepl("^[A-Za-z0-9_-]+$", run))
  out <- if (mode == "smoke") file.path("results", paste0("smoke_", run)) else "results"
  # 正式模式只把工作簿写进 results/；worker 日志与初筛/调参 CSV 写进 Git 忽略的中间结果目录。
  # 这样 results/ 顶层在运行期间没有不断变化的文件：三个脚本同时运行时互不触发哈希检查，续跑时指纹也不变。
  list(id = id, mode = mode, workers = workers, run = run, out = out,
       aux = if (mode == "smoke") out else file.path("results", "扩充_中间结果"))
}

phase_packages <- function() {
  lib <- Sys.getenv("USEP_R_LIBRARY")
  if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
  suppressPackageStartupMessages({library(readxl); library(dplyr); library(tidyr); library(writexl); library(forecast)})
  # 先设线程限制，再允许任何 worker 初始化 TensorFlow/BLAS。
  Sys.setenv(TF_NUM_INTRAOP_THREADS = "2", TF_NUM_INTEROP_THREADS = "2",
             OMP_NUM_THREADS = "2", OPENBLAS_NUM_THREADS = "2", TF_CPP_MIN_LOG_LEVEL = "2")
}

phase_load_data <- function() {
  # 接入方式与 04 相同，检查 hook 命中数；禁止执行 01 的任何结果写入。
  station_codes <- c(S24 = "Changi", S44 = "Jurong_West", S109 = "Ang_Mo_Kio", S104 = "Admiralty", S60 = "Sentosa_Island")
  september_temp <- Reduce(function(a, b) merge(a, b, by = "Date"), lapply(names(station_codes), function(code) {
    raw <- read.csv(file.path("raw_202609", paste0("DAILYDATA_", code, "_202609.csv")), fileEncoding = "UTF-8-BOM", colClasses = "character", check.names = FALSE)
    value <- trimws(raw[[grep("^Mean Temperature", names(raw))]])
    value[value %in% c("-", "—")] <- NA_character_
    x <- data.frame(Date = as.Date(sprintf("%s-%02d-%02d", trimws(raw$Year), as.integer(raw$Month), as.integer(raw$Day))), Value = as.numeric(value))
    # R 4.5 的 seq.Date 可能为 integer，as.Date(character) 为 double；比较日期值而非存储类型。
    stopifnot(identical(as.numeric(x$Date), as.numeric(seq(as.Date("2026-09-01"), as.Date("2026-09-30"), by = "day"))), identical(is.na(value), is.na(x$Value)))
    names(x)[2] <- station_codes[[code]]
    x
  }))
  hooks <- c(0L, 0L)
  for (e in as.list(parse("01_数据处理与变量生成.R", encoding = "UTF-8"))) {
    if (is.call(e) && as.character(e[[1]])[1] %in% c("write_xlsx", "saveRDS", "message")) next
    eval(e, globalenv())
    if (identical(e, quote(end_date <- as.Date("2026-08-31")))) {
      assign("end_date", as.Date("2026-09-30"), globalenv()); hooks[1] <- hooks[1] + 1L
    }
    if (identical(e, quote(temp$Date <- as.Date(temp$Date)))) {
      stopifnot(max(temp$Date) == as.Date("2026-08-31"))
      assign("temp", rbind(temp, september_temp[c("Date", stations)]), globalenv()); hooks[2] <- hooks[2] + 1L
    }
  }
  stopifnot(all(hooks == 1L))
  old <- readRDS("results/数据处理对象.rds")
  keep <- daily$Date <= as.Date("2026-08-31")
  stopifnot(isTRUE(all.equal(daily[keep, ], old$daily[old$daily$Date <= as.Date("2026-08-31"), ])))
  freeze <- readRDS("results/冻结的研究设置.rds")
  stopifnot(identical(temperature_lambda, freeze$temperature_lambda), refit_days == 7L)
  source("模型函数.R", encoding = "UTF-8")
  # 只传递数据和常量；函数在 worker 从只读源文件重新加载，避免序列化全局环境。
  objects <- c("daily", "temperature_lambda", "stations", "price_features", "external_features", "all_features", "warmup_date", "start_date", "train_end", "validation_end", "end_date", "refit_days", "holiday_dates")
  mget(objects, globalenv())
}

phase_worker_init <- function(payload, entry, wd, libs, context) {
  setwd(wd); .libPaths(libs)
  source("扩充公共函数.R", encoding = "UTF-8")
  phase_packages()
  list2env(readRDS(payload), globalenv())
  for (e in as.list(parse("01_数据处理与变量生成.R", encoding = "UTF-8")))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e, globalenv())
  source("模型函数.R", encoding = "UTF-8")
  source(entry, encoding = "UTF-8")
  assign(".phase", context, globalenv())
  assign(".feature_cache", new.env(parent = emptyenv()), globalenv())
  invisible(Sys.getpid())
}

phase_start <- function(id, entry) {
  phase_packages()
  opt <- phase_args(id)
  dir.create(opt$out, recursive = TRUE, showWarnings = FALSE)
  dir.create(opt$aux, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(opt$out, paste0("扩充_", id, ".xlsx"))
  if (file.exists(target)) stop("拒绝覆盖现有工作簿：", target, "；smoke 请使用新 --run。")
  protected <- c(list.files("results", full.names = TRUE), list.files(".", pattern = "\\.xlsx$", full.names = TRUE), list.files("raw_202609", full.names = TRUE), c("01_数据处理与变量生成.R", "02_模型验证与研究结果.R", "模型函数.R", "03_补充检验.R", "04_9月检验与实际预测.R"))
  protected <- protected[file_test("-f", protected)]
  hash <- tools::md5sum(protected)
  # 05–07 的正式输出不是彼此的输入，不计入指纹；否则一个脚本写出工作簿后，其余脚本续跑时缓存全部失效。
  inputs <- c(protected[!grepl("^扩充_[0-9]+\\.xlsx$", basename(protected))], "扩充公共函数.R", entry)
  packages <- c("forecast", "tseries", "ranger", "e1071", "prophet", "xgboost", "keras3", "tensorflow", "reticulate", "readxl", "dplyr", "tidyr", "writexl")
  versions <- setNames(vapply(packages, function(p) as.character(packageVersion(p)), character(1)), packages)
  if (id == "07") {
    python <- Sys.getenv("RETICULATE_PYTHON")
    if (!nzchar(python) || !file.exists(python)) stop("07 需要已安装后端的 RETICULATE_PYTHON。")
    command <- "import sys,importlib.metadata as m; print(sys.version.split()[0]); print(m.version('tensorflow')); print(m.version('keras'))"
    backend <- system2(python, c("-c", shQuote(command)), stdout = TRUE)
    stopifnot(length(backend) == 3L, is.null(attr(backend, "status")))
    versions <- c(versions, setNames(backend, c("Python", "Python_tensorflow", "Python_keras")))
  }
  fingerprint <- digest::digest(list(tools::md5sum(inputs), versions, R.version.string, Sys.getenv("RETICULATE_PYTHON"), opt$mode), algo = "sha256")
  cache <- file.path("results", "扩充_中间结果", id, opt$mode, fingerprint)
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  payload <- file.path(cache, "data.rds")
  if (!file.exists(payload)) saveRDS(phase_load_data(), payload)
  list2env(readRDS(payload), globalenv())
  source("模型函数.R", encoding = "UTF-8")
  # master 也可能需要训练期初筛。
  for (e in as.list(parse("01_数据处理与变量生成.R", encoding = "UTF-8")))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e, globalenv())
  context <- c(opt, list(cache = cache, fingerprint = fingerprint))
  assign(".phase", context, globalenv())
  assign(".feature_cache", new.env(parent = emptyenv()), globalenv())
  cl <- parallel::makePSOCKcluster(opt$workers, outfile = file.path(opt$aux, paste0(id, "_workers.log")))
  tryCatch(parallel::clusterCall(cl, phase_worker_init, normalizePath(payload, winslash = "/"), entry, getwd(), .libPaths(), context), error = function(e) {parallel::stopCluster(cl); stop(e)})
  list(opt = opt, cl = cl, hash = hash, target = target, versions = versions, fingerprint = fingerprint)
}

phase_map <- function(state, tasks, fun = phase_task) {
  if (!length(tasks)) return(list())
  parallel::parLapplyLB(state$cl, tasks, fun)
}

phase_features <- function(cutoff, through) {
  key <- paste(cutoff, through)
  if (!exists(key, .feature_cache, inherits = FALSE)) {
    assign(key, prepare_features(cutoff, through), .feature_cache)
  }
  get(key, .feature_cache, inherits = FALSE)
}

phase_dates <- function(mode, period = "Validation") {
  bounds <- switch(period, Validation = c("2025-01-01", "2025-12-31"), Test = c("2026-01-01", "2026-08-31"), September = c("2026-09-01", "2026-09-30"))
  dates <- seq(as.Date(bounds[1]), as.Date(bounds[2]), by = "day")
  if (mode == "smoke") {
    stopifnot(period == "Validation")
    dates <- head(dates, 14L)
  }
  dates
}

phase_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = 16))
phase_lambda <- function(config) if (identical(config$Transform, "Log")) 0 else NULL
phase_target <- function(dat, transform) switch(transform, Level = dat$DailyUSEP, Log = {stopifnot(all(dat$DailyUSEP > 0, na.rm = TRUE)); log(dat$DailyUSEP)}, Difference = dat$DailyUSEP - dat$USEP_lag1)
phase_inverse <- function(z, dat, rows, transform) switch(transform, Level = z, Log = exp(z), Difference = z + dat$USEP_lag1[rows])

phase_task <- function(job) {
  warnings <- character(); failure <- ""
  # Seconds 为各周拟合 + 预测耗时之和；每周耗时随周缓存保存，续跑时读回而不是重新计时。
  seconds <- 0; week_started <- NA_real_
  result <- tryCatch(withCallingHandlers({
    chunks <- lapply(seq(1L, length(job$dates), by = 7L), function(i) {
      dates <- job$dates[i:min(i + 6L, length(job$dates))]
      cutoff <- dates[1] - 1L
      key <- digest::digest(list(job$config, job$seed, dates), algo = "sha256")
      file <- file.path(.phase$cache, paste0("week_", key, ".rds"))
      if (file.exists(file)) {
        ans <- readRDS(file)
        stopifnot(length(attr(ans, "seconds")) == 1L, is.finite(attr(ans, "seconds")))
        seconds <<- seconds + attr(ans, "seconds")
        return(ans)
      }
      dat <- phase_features(cutoff, max(dates))
      # 所有新模型的拟合样本从统一预热终点开始；特征序列仍保留前面的上下文。
      rows <- which(dat$Date >= warmup_date & dat$Date <= cutoff)
      current <- match(dates, dat$Date)
      week_started <<- proc.time()[["elapsed"]]
      fit <- phase_fit(job$config, dat, rows, cutoff, job$seed)
      pred <- vapply(current, function(j) phase_predict(fit, job$config, dat, rows, j), numeric(1))
      stopifnot(length(pred) == length(dates), all(is.finite(pred)))
      ans <- data.frame(Date = dates, Prediction = pred, Actual = dat$DailyUSEP[current], Cutoff = cutoff, Seed = job$seed,
                        FitDetail = if (is.null(fit$detail)) "" else phase_json(fit$detail))
      attr(ans, "seconds") <- proc.time()[["elapsed"]] - week_started
      saveRDS(ans, file)
      seconds <<- seconds + attr(ans, "seconds"); week_started <<- NA_real_
      message(job$config$Model, " ", job$config$Candidate, " seed=", job$seed, " cutoff=", cutoff, " OK")
      ans
    })
    bind_rows(chunks)
  }, warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")}), error = function(e) {
    failure <<- conditionMessage(e)
    # 失败的那一周不缓存，但已花费的时间仍计入本次日志。
    if (is.finite(week_started)) seconds <<- seconds + proc.time()[["elapsed"]] - week_started
    NULL
  })
  rmse <- if (is.null(result)) NA_real_ else sqrt(mean((result$Actual - result$Prediction)^2))
  log <- data.frame(Model = job$config$Model, Candidate = job$config$Candidate, Transform = job$config$Transform,
                    Settings = phase_json(job$config), Seed = job$seed, Stage = job$stage, N = if (is.null(result)) 0L else nrow(result),
                    ValidationRMSE = if (job$period == "Validation") rmse else NA_real_, Seconds = seconds,
                    Failure = failure, Warnings = paste(unique(warnings), collapse = " | "), Mode = .phase$mode)
  if (nzchar(failure)) message("FAILED ", job$config$Candidate, ": ", failure)
  list(config = job$config, seed = job$seed, daily = result, log = log)
}

phase_jobs <- function(configs, dates, seeds = 2026L, stage = "Tuning", period = "Validation") {
  unlist(lapply(configs, function(config) lapply(seeds, function(seed) list(config = config, dates = dates, seed = seed, stage = stage, period = period))), recursive = FALSE)
}

phase_run <- function(state, configs, screening = data.frame()) {
  dates <- phase_dates(state$opt$mode)
  results <- phase_map(state, phase_jobs(configs, dates))
  tuning <- bind_rows(lapply(results, `[[`, "log"))
  write.csv(tuning, file.path(state$opt$aux, paste0(state$opt$id, "_tuning_initial.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  models <- unique(vapply(configs, `[[`, character(1), "Model"))
  neural_models <- c("LSTM_Improved", "GRU", "NNAR")
  finalists <- setNames(lapply(models, function(model) {
    candidates <- Filter(function(x) x$config$Model == model && !is.null(x$daily), results)
    if (!length(candidates)) stop("模型全部候选失败：", model, "；见 initial 调参日志。")
    candidates <- candidates[order(vapply(candidates, function(x) x$log$ValidationRMSE, numeric(1)))]
    head(candidates, if (model %in% c("LSTM_Improved", "GRU")) 3L else 1L)
  }), models)
  # 所有模型的种子确认任务放进同一次并行映射，不按模型依次等待。
  extras <- phase_map(state, unlist(lapply(intersect(models, neural_models), function(model)
    phase_jobs(lapply(finalists[[model]], `[[`, "config"), dates, c(2027L, 2028L), "SeedConfirmation")), recursive = FALSE))
  best <- setNames(lapply(models, function(model) {
    ranked <- lapply(finalists[[model]], function(x) {
      members <- c(list(x), Filter(function(y) y$config$Candidate == x$config$Candidate, extras))
      if (any(vapply(members, function(y) is.null(y$daily), logical(1)))) return(NULL)
      prediction <- rowMeans(do.call(cbind, lapply(members, function(y) y$daily$Prediction)))
      # 计划 P3：按各种子验证 RMSE 的算术平均定设置；同时保存均值预测 RMSE，避免混淆。
      list(config = x$config, members = members, prediction = prediction, selection = mean(vapply(members, function(y) y$log$ValidationRMSE, numeric(1))))
    })
    ranked <- Filter(Negate(is.null), ranked)
    if (!length(ranked)) stop("种子确认全部失败：", model)
    ranked[[which.min(vapply(ranked, `[[`, numeric(1), "selection"))]]
  }), models)
  # 所有模型的 Test 与 September 冻结评估同样合并为一次并行映射；结果顺序与任务顺序一致。
  periods <- if (state$opt$mode == "full") c("Test", "September") else character()
  final_jobs <- unlist(lapply(models, function(model) unlist(lapply(periods, function(period)
    phase_jobs(list(best[[model]]$config), phase_dates("full", period), if (model %in% neural_models) 2026:2028 else 2026L, "FrozenEvaluation", period)), recursive = FALSE)), recursive = FALSE)
  final_all <- phase_map(state, final_jobs)
  final_key <- vapply(final_jobs, function(job) paste(job$config$Model, job$period), character(1))
  frozen <- list(); all_daily <- list(); seeds_daily <- list()
  for (model in models) {
    neural <- model %in% neural_models
    chosen <- best[[model]]
    tuning <- bind_rows(tuning, bind_rows(lapply(Filter(function(y) y$config$Model == model, extras), `[[`, "log")))
    actual <- chosen$members[[1]]$daily$Actual
    frozen[[model]] <- data.frame(Model = model, Candidate = chosen$config$Candidate, Transform = chosen$config$Transform,
      Settings = phase_json(chosen$config), SelectionRMSE = chosen$selection, EnsembleValidationRMSE = sqrt(mean((actual - chosen$prediction)^2)),
      Seeds = if (neural) "2026,2027,2028" else "2026", IsFinal = state$opt$mode == "full", SelectionPeriod = if (state$opt$mode == "full") "2025 full year" else "SMOKE ONLY: 2025-01-01..14", Mode = state$opt$mode)
    all_daily[[paste0(model, "Validation")]] <- data.frame(Date = dates, Period = "Validation", Model = model, Prediction = chosen$prediction, Actual = actual)
    for (member in chosen$members) seeds_daily[[length(seeds_daily) + 1L]] <- cbind(Model = model, Period = "Validation", member$daily)
    for (period in periods) {
      final <- final_all[final_key == paste(model, period)]
      tuning <- bind_rows(tuning, bind_rows(lapply(final, `[[`, "log")))
      if (!length(final) || any(vapply(final, function(x) is.null(x$daily), logical(1)))) stop("冻结模型预测失败：", model, " / ", period)
      all_daily[[paste0(model, period)]] <- data.frame(Date = final[[1]]$daily$Date, Period = period, Model = model,
        Prediction = rowMeans(do.call(cbind, lapply(final, function(x) x$daily$Prediction))), Actual = final[[1]]$daily$Actual)
      for (member in final) seeds_daily[[length(seeds_daily) + 1L]] <- cbind(Model = model, Period = period, member$daily)
    }
  }
  # 原始 XGBoost/LSTM 作为只读对照，正式模式保留三个时期；不重新训练、不参与改进版调参。
  original <- switch(state$opt$id, `06` = "XGBoost", `07` = "LSTM", NULL)
  if (!is.null(original)) for (period in if (state$opt$mode == "smoke") "Validation" else c("Validation", "Test", "September")) {
    if (period == "September") {
      x <- as.data.frame(read_excel("results/9月检验与实际预测.xlsx", sheet = "Sept_Daily")); x <- x[x$Model == original, ]
      prediction <- x$Prediction
    } else {x <- as.data.frame(read_excel("results/逐日预测与模型变量.xlsx", sheet = period)); prediction <- x[[original]]}
    keep <- as.Date(x$Date) %in% phase_dates(state$opt$mode, period)
    all_daily[[paste0(original, period)]] <- data.frame(Date = as.Date(x$Date)[keep], Period = period, Model = paste0(original, "_Original"), Prediction = prediction[keep], Actual = x$Actual[keep])
  }
  daily_out <- bind_rows(all_daily)
  stopifnot(!anyNA(daily_out), !anyDuplicated(daily_out[c("Date", "Period", "Model")]), all(is.finite(daily_out$Prediction)))
  if (state$opt$mode == "smoke") stopifnot(all(daily_out$Period == "Validation"), min(daily_out$Date) == as.Date("2025-01-01"), max(daily_out$Date) == as.Date("2025-01-14"))
  meta <- data.frame(Item = c("Mode", "Fingerprint", "R", "Workers", names(state$versions)), Value = c(state$opt$mode, state$fingerprint, R.version.string, as.character(state$opt$workers), state$versions))
  sheets <- list(Daily = daily_out, Tuning = tuning, Frozen = bind_rows(frozen), SeedDaily = bind_rows(seeds_daily), Metadata = meta)
  if (nrow(screening)) sheets$Screening <- screening
  write_xlsx(sheets, state$target)
  # 读回核对输出约定；试跑和全量永不共享输出路径或缓存指纹。
  stopifnot(all(c("Daily", "Tuning", "Frozen") %in% excel_sheets(state$target)), nrow(read_excel(state$target, sheet = "Daily")) == nrow(daily_out))
  stopifnot(identical(tools::md5sum(names(state$hash)), state$hash))
  cat("PASS", state$opt$id, "mode=", state$opt$mode, "Daily=", nrow(daily_out), "Tuning=", nrow(tuning), "Frozen=", length(frozen), "\n")
  cat("Protected input/output hashes unchanged. Output:", state$target, "\n")
  print(sessionInfo())
  invisible(sheets)
}
