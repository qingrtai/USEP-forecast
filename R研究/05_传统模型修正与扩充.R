# P1: Rscript 05_传统模型修正与扩充.R --mode=smoke --workers=2 --run=<id>
# 默认只跑验证期前两周。完整网格与全量路径已实现；未经审核不运行 full。
source("扩充公共函数.R", encoding = "UTF-8")

p1_x <- function(config, dat) {
  if (config$Kind %in% c("ETS", "SARIMA")) return(NULL)
  if (config$Kind == "Prophet") return(as.matrix(dat[feature_names("TSLM", 4L)[grepl("_lag|_MA|_SD", feature_names("TSLM", 4L))]]))
  names <- feature_names("DynamicRegression", config$Step)
  if (config$Kind == "DHR") {
    # 周内用固定相位；删除星期哑变量以免与周 Fourier 共线，保留公众假期。
    names <- setdiff(names, c("Monday", "Tuesday", "Wednesday", "Thursday", "Saturday", "Weekend"))
    t <- as.numeric(dat$Date - start_date)
    fourier <- function(period, K, prefix) {
      x <- do.call(cbind, lapply(seq_len(K), function(k) cbind(sin(2*pi*k*t/period), cos(2*pi*k*t/period))))
      colnames(x) <- paste0(prefix, rep(seq_len(K), each = 2), rep(c("sin", "cos"), K)); x
    }
    return(cbind(as.matrix(dat[names]), fourier(365.25, config$K, "Year"), fourier(7, 3L, "Week")))
  }
  as.matrix(dat[names])
}

p1_order <- function(config, dat, rows) {
  y <- phase_target(dat, config$Transform)[rows]
  x <- p1_x(config, dat)
  # 回归误差的差分测试在初始训练期 OLS 残差上进行，不对 y 生硬套单变量差分。
  if (!is.null(x)) y <- residuals(lm(y ~ x[rows, , drop = FALSE]))
  D <- if (config$Kind == "DHR") 0L else forecast::nsdiffs(ts(y, frequency = 7), test = "seas", max.D = 1L)
  z <- if (D) diff(y, lag = 7L, differences = D) else y
  d <- forecast::ndiffs(z, test = "kpss", max.d = 2L)
  c(d = d, D = D)
}

p1_screen <- function(config) {
  start <- proc.time()[["elapsed"]]; failure <- ""; warning <- character()
  aicc <- tryCatch(withCallingHandlers({
    dat <- phase_features(train_end, train_end)
    rows <- which(dat$Date >= warmup_date)
    x <- p1_x(config, dat)
    model <- forecast::Arima(ts(dat$DailyUSEP[rows], frequency = 7), xreg = if (is.null(x)) NULL else x[rows, , drop = FALSE],
      order = config$Order, seasonal = list(order = config$Seasonal, period = 7), include.constant = TRUE,
      lambda = phase_lambda(config), biasadj = TRUE, method = "ML")
    if (!is.null(model$code) && model$code != 0L) stop("ARIMA optimizer code=", model$code)
    stopifnot(is.finite(model$aicc)); model$aicc
  }, warning = function(w) {warning <<- c(warning, conditionMessage(w)); invokeRestart("muffleWarning")}), error = function(e) {failure <<- conditionMessage(e); NA_real_})
  list(config = config, log = data.frame(Model = config$Model, Candidate = config$Candidate, Transform = config$Transform, Settings = phase_json(config),
    AICc = aicc, Seconds = proc.time()[["elapsed"]] - start, Failure = failure, Warnings = paste(unique(warning), collapse = " | "), Mode = .phase$mode))
}

p1_candidates <- function(state) {
  configs <- list(); screening <- list()
  add <- function(x) {configs[[length(configs) + 1L]] <<- x}
  dat <- phase_features(train_end, train_end); rows <- which(dat$Date >= warmup_date)
  for (model in c("SARIMA", paste0("DynamicRegression_M", 0:4), "DHR")) for (transform in c("Level", "Log")) {
    kind <- if (startsWith(model, "Dynamic")) "DynamicRegression" else model
    step <- if (kind == "DynamicRegression") as.integer(sub(".*_M", "", model)) else 4L
    base <- list(Model = model, Kind = kind, Step = step, Transform = transform, K = 5L)
    # DHR 固定共同 d 后比较 K，避免跨差分阶数比较 AICc；单位根检测使用 K=5 残差。
    diff <- p1_order(base, dat, rows)
    grid <- if (kind == "DHR") expand.grid(p = 0:2, q = 0:2, P = 0L, Q = 0L, K = 1:5) else expand.grid(p = 0:2, q = 0:2, P = 0:1, Q = 0:1, K = 0L)
    if (state$opt$mode == "smoke") grid <- if (kind == "DHR") grid[grid$p == 1L & grid$q == 0L & grid$K %in% c(1L, 2L), ] else grid[grid$p %in% 0:1 & grid$q == 0L & grid$P == 0L & grid$Q == 0L, ]
    jobs <- lapply(seq_len(nrow(grid)), function(i) {
      x <- base; x$K <- grid$K[i]; x$Order <- c(grid$p[i], unname(diff["d"]), grid$q[i]); x$Seasonal <- c(grid$P[i], unname(diff["D"]), grid$Q[i])
      x$Candidate <- paste(model, transform, paste(x$Order, collapse = ""), paste(x$Seasonal, collapse = ""), paste0("K", x$K), sep = "_"); x
    })
    fits <- phase_map(state, jobs, p1_screen)
    logs <- bind_rows(lapply(fits, `[[`, "log"))
    good <- which(is.finite(logs$AICc))
    if (!length(good)) stop("初筛全部失败：", model, " ", transform, " ", paste(logs$Failure, collapse = "; "))
    n <- if (state$opt$mode == "smoke" || kind == "DHR") 1L else 6L
    chosen <- head(good[order(logs$AICc[good])], n)
    logs$Kept <- seq_len(nrow(logs)) %in% chosen
    screening[[length(screening) + 1L]] <- logs
    for (i in chosen) add(fits[[i]]$config)
    # 初筛日志即时落盘，中断也可查失败原因；不是最终研究输出。
    write.csv(bind_rows(screening), file.path(state$opt$out, "05_screening.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  for (transform in c("Level", "Log")) {
    for (i in 1:3) add(list(Model = "ETS", Kind = "ETS", Step = 4L, Transform = transform,
      Candidate = paste("ETS", transform, i, sep = "_"), ETS = c("ANA", "AAA", "AAA")[i], Damped = i == 3L))
    for (cp in c(0.01, 0.05, 0.5)) add(list(Model = "Prophet", Kind = "Prophet", Step = 4L, Transform = transform,
      Candidate = paste("Prophet", transform, cp, sep = "_"), ChangepointPriorScale = cp))
  }
  list(configs = configs, screening = bind_rows(screening))
}

phase_fit <- function(config, dat, rows, cutoff, seed) {
  set.seed(seed)
  x <- p1_x(config, dat)
  scaler <- if (is.null(x)) NULL else fit_scaler(x[rows, , drop = FALSE])
  x <- if (is.null(x)) NULL else transform_x(x, scaler)
  y <- ts(dat$DailyUSEP[rows], frequency = 7)
  if (config$Kind == "ETS") {
    # forecast::ets 会在 lambda 非空时强制 additive.only；仍显式保留原三种结构。
    model <- forecast::ets(y, model = config$ETS, damped = config$Damped, lambda = phase_lambda(config), biasadj = TRUE)
  } else if (config$Kind == "Prophet") {
    target <- phase_target(dat, config$Transform)
    holidays <- data.frame(holiday = "SG_public_holiday", ds = holiday_dates, lower_window = 0L, upper_window = 0L)
    model <- prophet::prophet(growth = "linear", yearly.seasonality = TRUE, weekly.seasonality = TRUE,
      daily.seasonality = FALSE, holidays = holidays, changepoint.prior.scale = config$ChangepointPriorScale,
      uncertainty.samples = 0L, fit = FALSE)
    for (name in colnames(x)) model <- prophet::add_regressor(model, name, standardize = TRUE)
    frame <- data.frame(ds = dat$Date[rows], y = target[rows], x[rows, , drop = FALSE])
    model <- prophet::fit.prophet(model, frame, seed = seed)
    # Prophet 无原生 Box-Cox biasadj；训练期对数残差的 smearing 还原均值，只用当前训练期。
    smear <- if (config$Transform == "Log") mean(exp(target[rows] - predict(model, frame)$yhat)) else 1
    return(list(model = model, scaler = scaler, smear = smear, detail = list(TrainEnd = as.character(cutoff))))
  } else {
    model <- forecast::Arima(y, xreg = if (is.null(x)) NULL else x[rows, , drop = FALSE], order = config$Order,
      seasonal = list(order = config$Seasonal, period = 7), include.constant = TRUE, method = "ML", lambda = phase_lambda(config), biasadj = TRUE)
    if (!is.null(model$code) && model$code != 0L) stop("ARIMA optimizer code=", model$code)
  }
  list(model = model, scaler = scaler, detail = list(TrainEnd = as.character(cutoff)))
}

phase_predict <- function(fit, config, dat, rows, current) {
  past <- which(dat$Date >= warmup_date & seq_len(nrow(dat)) < current)
  history <- ts(dat$DailyUSEP[past], frequency = 7)
  if (config$Kind == "ETS") {
    updated <- forecast::ets(history, model = fit$model, use.initial.values = TRUE, lambda = phase_lambda(config), biasadj = TRUE)
    return(as.numeric(forecast::forecast(updated, h = 1L, biasadj = TRUE)$mean))
  }
  x <- p1_x(config, dat)
  if (!is.null(x)) x <- transform_x(x, fit$scaler)
  if (config$Kind == "Prophet") {
    z <- predict(fit$model, data.frame(ds = dat$Date[current], x[current, , drop = FALSE]))$yhat
    return(as.numeric(if (config$Transform == "Log") exp(z) * fit$smear else z))
  }
  updated <- forecast::Arima(history, xreg = if (is.null(x)) NULL else x[past, , drop = FALSE], model = fit$model, lambda = phase_lambda(config), biasadj = TRUE)
  as.numeric(forecast::forecast(updated, xreg = if (is.null(x)) NULL else x[current, , drop = FALSE], h = 1L, biasadj = TRUE)$mean)
}

p1_main <- function() {
  state <- phase_start("05", "05_传统模型修正与扩充.R")
  on.exit(parallel::stopCluster(state$cl), add = TRUE)
  candidates <- p1_candidates(state)
  phase_run(state, candidates$configs, candidates$screening)
}
if (sys.nframe() == 0L) p1_main()
