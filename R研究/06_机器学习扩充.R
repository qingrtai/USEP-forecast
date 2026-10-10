# P2: Rscript 06_机器学习扩充.R --mode=smoke --workers=2 --run=<id>
source("扩充公共函数.R", encoding = "UTF-8")

p2_candidates <- function(mode) {
  set.seed(2026)
  grid <- expand.grid(max_depth = c(2L, 3L, 4L), eta = c(0.03, 0.1), min_child_weight = c(1, 5), subsample = c(0.8, 1), colsample_bytree = c(0.8, 1), lambda = c(1, 5))
  # 顺序、RNG 及抽样与 02 的原 12 组完全相同。
  grids <- list(RandomForest = expand.grid(mtry = c(3L, 6L, 9L), min.node.size = c(5L, 10L, 20L)),
    SVR = expand.grid(cost = c(1, 10, 100), GammaMultiplier = c(0.5, 1, 2)), XGBoost_Transformed = grid[sample(nrow(grid), 12L), ])
  configs <- list()
  for (model in names(grids)) for (transform in c("Level", "Log", "Difference")) {
    g <- grids[[model]]
    if (mode == "smoke") g <- head(g, 1L)
    for (i in seq_len(nrow(g))) configs[[length(configs) + 1L]] <- list(Model = model, Transform = transform,
      Candidate = paste(model, transform, i, sep = "_"), Parameters = as.list(g[i, ]))
  }
  configs
}

phase_fit <- function(config, dat, rows, cutoff, seed) {
  set.seed(seed)
  features <- feature_names("XGBoost", 4L)
  x <- as.matrix(dat[features]); y <- phase_target(dat, config$Transform)
  scaler <- fit_scaler(x[rows, , drop = FALSE])
  plain <- transform_x(x, scaler, config$Model == "SVR")
  # SVR epsilon=0.1 在训练期标准化目标尺度定义，避免不同目标量纲造成网格不可比。
  center <- if (config$Model == "SVR") mean(y[rows]) else 0
  scale <- if (config$Model == "SVR") sd(y[rows]) else 1
  stopifnot(is.finite(scale), scale > 0)
  target <- (y - center) / scale
  p <- config$Parameters
  detail <- list(TrainEnd = as.character(cutoff))
  if (config$Model == "RandomForest") {
    model <- ranger::ranger(x = plain[rows, , drop = FALSE], y = target[rows], num.trees = 500L,
      mtry = p$mtry, min.node.size = p$min.node.size, seed = seed, num.threads = 2L)
  } else if (config$Model == "SVR") {
    model <- e1071::svm(x = plain[rows, , drop = FALSE], y = target[rows], type = "eps-regression", kernel = "radial",
      cost = p$cost, gamma = p$GammaMultiplier / ncol(plain), epsilon = 0.1, scale = FALSE)
  } else {
    # A1 S2：先切 inner/holdout，再单独拟合温度补值和输入缺值处理。
    inner <- head(rows, -28L); holdout <- tail(rows, 28L)
    inner_data <- phase_features(dat$Date[max(inner)], cutoff)
    inner_x <- as.matrix(inner_data[features]); inner_scaler <- fit_scaler(inner_x[inner, , drop = FALSE])
    inner_x <- transform_x(inner_x, inner_scaler)
    params <- c(list(objective = "reg:squarederror", eval_metric = "rmse", nthread = 2L, seed = seed), p)
    early <- xgboost::xgb.train(params = params, data = xgboost::xgb.DMatrix(inner_x[inner, , drop = FALSE], label = target[inner]),
      nrounds = 600L, evals = list(validation = xgboost::xgb.DMatrix(inner_x[holdout, , drop = FALSE], label = target[holdout])), early_stopping_rounds = 20L, verbose = 0L)
    rounds <- which.min(as.numeric(attr(early, "evaluation_log")[[2]]))
    stopifnot(length(rounds) == 1L, rounds >= 1L)
    model <- xgboost::xgb.train(params = params, data = xgboost::xgb.DMatrix(plain[rows, , drop = FALSE], label = target[rows]), nrounds = rounds, verbose = 0L)
    detail$InnerCutoff <- as.character(dat$Date[max(inner)]); detail$Rounds <- rounds
  }
  list(model = model, scaler = scaler, center = center, scale = scale, detail = detail)
}

phase_predict <- function(fit, config, dat, rows, current) {
  x <- transform_x(as.matrix(dat[feature_names("XGBoost", 4L)]), fit$scaler, config$Model == "SVR")[current, , drop = FALSE]
  z <- if (config$Model == "RandomForest") predict(fit$model, data = x, num.threads = 2L)$predictions else if (config$Model == "SVR") predict(fit$model, x) else predict(fit$model, xgboost::xgb.DMatrix(x))
  # 对数模型直接 exp 回原尺度（未声称 bias-adjusted mean）；差分加实际前日价格。
  as.numeric(phase_inverse(as.numeric(z) * fit$scale + fit$center, dat, current, config$Transform))
}

p2_main <- function() {
  state <- phase_start("06", "06_机器学习扩充.R")
  on.exit(parallel::stopCluster(state$cl), add = TRUE)
  phase_run(state, p2_candidates(state$opt$mode))
}
if (sys.nframe() == 0L) p2_main()
