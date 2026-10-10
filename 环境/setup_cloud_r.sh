#!/usr/bin/env bash
# 云端（Ubuntu 24.04）一次性安装 R 运行环境，约 10–15 分钟。
# CRAN 官网在云端被网络策略拦截，因此：R 与多数包用 Ubuntu 源，
# 较新的包（xgboost 3.x、keras3 及其依赖）从 GitHub 上的 CRAN 镜像 github.com/cran/<包名> 源码安装。
# 用法：bash 环境/setup_cloud_r.sh
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq || true
apt-get install -y --no-install-recommends \
  r-base-core r-base-dev cmake python3-venv \
  r-cran-forecast r-cran-readxl r-cran-dplyr r-cran-tidyr r-cran-writexl \
  r-cran-urca r-cran-tseries r-cran-ggplot2 r-cran-reticulate r-cran-jsonlite \
  r-cran-data.table r-cran-r6 r-cran-rlang r-cran-glue r-cran-cli r-cran-magrittr \
  r-cran-fastmap r-cran-generics r-cran-png r-cran-rcpptoml r-cran-here r-cran-withr \
  r-cran-rappdirs r-cran-processx r-cran-yaml r-cran-whisker r-cran-base64enc \
  r-cran-tidyselect r-cran-rstudioapi r-cran-zeallot r-cran-backports r-cran-rcpp \
  r-cran-matrix r-cran-later r-cran-codetools
# 与本地一致的 TensorFlow 2.21.0 / Keras 3.15.1
python3 -m venv /opt/tfenv
/opt/tfenv/bin/pip install -q tensorflow==2.21.0 keras==3.15.1 numpy
# GitHub CRAN 镜像源码安装
BUILD=$(mktemp -d)
cd "$BUILD"
for p in reticulate config tfruns tfautograph tensorflow dotty keras3 xgboost; do
  git clone --depth 1 -q "https://github.com/cran/$p.git"
  MAKEFLAGS=-j2 R CMD INSTALL "$p"
done
echo "完成。运行 R 脚本前请设置："
echo "  export LANG=C.UTF-8 LC_ALL=C.UTF-8 RETICULATE_PYTHON=/opt/tfenv/bin/python"
