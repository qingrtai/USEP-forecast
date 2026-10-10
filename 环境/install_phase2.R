# Rscript 环境/install_phase2.R；使用用户库，不更改冻结研究结果。
lib <- Sys.getenv("USEP_R_LIBRARY", unset = file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")))
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600)
install.packages(c("tseries", "ranger", "e1071", "prophet"), lib = lib)
# 已有脚本所需依赖；只补缺失项。
required <- c("readxl", "dplyr", "tidyr", "writexl", "forecast", "xgboost", "keras3", "tensorflow", "reticulate", "jsonlite", "digest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, lib = lib)
stopifnot(all(vapply(c("tseries", "ranger", "e1071", "prophet", required), requireNamespace, logical(1), quietly = TRUE)))
print(sessionInfo())
