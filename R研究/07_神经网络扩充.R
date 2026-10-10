# P3: Rscript 07_神经网络扩充.R --mode=smoke --workers=2 --run=<id>
# 必须先设置 RETICULATE_PYTHON。TensorFlow 两线程/进程；最多六个 worker。
source("扩充公共函数.R", encoding = "UTF-8")

p3_candidates <- function(mode) {
  set.seed(2026)
  grid <- expand.grid(sequence = c(7L, 14L, 28L), units = c(16L, 32L), dropout = c(0.1, 0.2), learning_rate = c(0.001, 0.0005))
  # P3 明确重新 set.seed(2026)，不继承 02 抽取 XGBoost 后的 RNG 状态。
  grid <- grid[sample(nrow(grid), 12L), ]
  configs <- list()
  for (model in c("LSTM_Improved", "GRU")) for (transform in c("Level", "Log", "Difference")) {
    use <- if (mode == "smoke") head(grid, 1L) else grid
    for (i in seq_len(nrow(use))) configs[[length(configs) + 1L]] <- list(Model = model, Transform = transform,
      Candidate = paste(model, transform, i, sep = "_"), Parameters = as.list(use[i, ]))
  }
  for (transform in c("Level", "Log")) for (xreg in c(FALSE, TRUE)) configs[[length(configs) + 1L]] <- list(Model = "NNAR", Transform = transform,
    Candidate = paste("NNAR", transform, xreg, sep = "_"), XReg = xreg, Repeats = 20L)
  configs
}

p3_backend <- function() {
  if (!exists(".backend_ready", globalenv(), inherits = FALSE)) {
    if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) stop("请明确设置 RETICULATE_PYTHON，避免自动下载/切换 Python。")
    Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")
    reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
    tensorflow::tf$config$threading$set_intra_op_parallelism_threads(2L)
    tensorflow::tf$config$threading$set_inter_op_parallelism_threads(2L)
    tensorflow::tf$config$experimental$enable_op_determinism()
    assign(".backend_ready", TRUE, globalenv())
    message("Backend: Python=", reticulate::py_config()$python, "; TensorFlow=", tensorflow::tf$`__version__`, "; Keras=", reticulate::import("keras")$`__version__`)
  }
}

p3_network <- function(config, n_features, seed) {
  p3_backend()
  keras3::clear_session(); keras3::set_random_seed(seed)
  p <- config$Parameters
  model <- keras3::keras_model_sequential(input_shape = c(p$sequence, n_features))
  model <- if (config$Model == "GRU") keras3::layer_gru(model, units = p$units) else keras3::layer_lstm(model, units = p$units)
  model <- keras3::layer_dropout(model, rate = p$dropout)
  model <- keras3::layer_dense(model, units = 1L)
  keras3::compile(model, optimizer = keras3::optimizer_adam(learning_rate = p$learning_rate), loss = "mse")
  model
}

phase_fit <- function(config, dat, rows, cutoff, seed) {
  set.seed(seed)
  features <- feature_names("XGBoost", 4L)
  x <- as.matrix(dat[features])
  if (config$Model == "NNAR") {
    scaler <- fit_scaler(x[rows, , drop = FALSE])
    plain <- transform_x(x, scaler, TRUE)
    model <- forecast::nnetar(ts(dat$DailyUSEP[rows], frequency = 7),
      xreg = if (config$XReg) plain[rows, , drop = FALSE] else NULL, lambda = phase_lambda(config), repeats = 20L, MaxNWts = 100000L)
    return(list(model = model, scaler = scaler, detail = list(TrainEnd = as.character(cutoff), p = model$p, P = model$P, size = model$size, repeats = 20L)))
  }
  p <- config$Parameters
  y <- phase_target(dat, config$Transform)
  inner <- head(rows, -28L); holdout <- tail(rows, 28L)
  # A1 S2：内层早停用独立温度补值模型，不复用含留出段的补值系数。
  inner_data <- phase_features(dat$Date[max(inner)], cutoff)
  inner_x <- as.matrix(inner_data[features])
  inner_scaler <- fit_scaler(inner_x[seq_len(max(inner)), , drop = FALSE])
  inner_x <- transform_x(inner_x, inner_scaler, TRUE)
  inner_center <- mean(y[inner]); inner_scale <- sd(y[inner])
  stopifnot(is.finite(inner_scale), inner_scale > 0)
  early <- p3_network(config, ncol(x), seed)
  history <- keras3::fit(early, make_sequences(inner_x, inner, p$sequence), (y[inner] - inner_center) / inner_scale,
    validation_data = list(make_sequences(inner_x, holdout, p$sequence), (y[holdout] - inner_center) / inner_scale),
    epochs = 100L, batch_size = 32L, shuffle = TRUE,
    callbacks = list(keras3::callback_early_stopping(monitor = "val_loss", patience = 10L)), verbose = 0L)
  epochs <- which.min(history$metrics$val_loss)
  stopifnot(length(epochs) == 1L, epochs >= 1L)
  scaler <- fit_scaler(x[seq_len(max(rows)), , drop = FALSE])
  scaled <- transform_x(x, scaler, TRUE)
  center <- mean(y[rows]); scale <- sd(y[rows])
  model <- p3_network(config, ncol(x), seed)
  keras3::fit(model, make_sequences(scaled, rows, p$sequence), (y[rows] - center) / scale,
    epochs = epochs, batch_size = 32L, shuffle = TRUE, verbose = 0L)
  list(model = model, scaler = scaler, center = center, scale = scale,
       detail = list(TrainEnd = as.character(cutoff), InnerCutoff = as.character(dat$Date[max(inner)]), Epochs = epochs, Shuffle = TRUE))
}

phase_predict <- function(fit, config, dat, rows, current) {
  x <- transform_x(as.matrix(dat[feature_names("XGBoost", 4L)]), fit$scaler, TRUE)
  if (config$Model == "NNAR") {
    past <- which(dat$Date >= warmup_date & seq_len(nrow(dat)) < current)
    updated <- forecast::nnetar(ts(dat$DailyUSEP[past], frequency = 7),
      xreg = if (config$XReg) x[past, , drop = FALSE] else NULL, model = fit$model)
    # nnetar 的 lambda 由 model 继承，周内只更新输入/状态，不重估网络。
    return(as.numeric(forecast::forecast(updated, h = 1L, xreg = if (config$XReg) x[current, , drop = FALSE] else NULL, PI = FALSE)$mean))
  }
  z <- as.numeric(predict(fit$model, make_sequences(x, current, config$Parameters$sequence), verbose = 0L)) * fit$scale + fit$center
  as.numeric(phase_inverse(z, dat, current, config$Transform))
}

p3_main <- function() {
  state <- phase_start("07", "07_神经网络扩充.R")
  on.exit(parallel::stopCluster(state$cl), add = TRUE)
  phase_run(state, p3_candidates(state$opt$mode))
}
if (sys.nframe() == 0L) p3_main()
