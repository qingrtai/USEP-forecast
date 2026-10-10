# P1–P3 代码与试跑交接

本批仅供代码审核。没有启动全年验证、2026 年测试期或九月预测，不能将两周 RMSE 或 `Frozen` 中的临时选择当作研究结论。

## 文件与执行方式

- `../05_传统模型修正与扩充.R`：ETS、重选 SARIMA、动态回归 M0–M4、DHR、Prophet。
- `../06_机器学习扩充.R`：随机森林、SVR、XGBoost 目标变换版；保留原 XGBoost 冻结预测作为对照。
- `../07_神经网络扩充.R`：改进 LSTM、GRU、NNAR；保留原 LSTM 冻结预测作为对照。
- `../扩充公共函数.R`：数据接入、独立 Rscript/PSOCK worker、逐周断点、调参、种子确认、统一输出与旧文件哈希检查。
- `../试跑校验.R`：输出读回校验、完整网格数量、原 XGBoost 网格同一性、目标变换逆运算和未来观测扰动检查。
- `../../环境/install_phase2.R`：先按要求安装 tseries/ranger/e1071/prophet，再补缺失的 R 依赖到用户库。
- `../../环境/run_phase2_smoke.ps1`：依次启动三个脚本，固定 `--mode=smoke`；每次使用新的 `Run`，拒绝覆盖旧工作簿及日志。

实际本地环境为 R 4.5.3，而非计划中记载的 R 4.6.1。R 包版本进入每个工作簿 `Metadata`；安装日志及 `sessionInfo()` 随本目录提交。首次将 Python 后端放在 `环境/.venv-phase2/`，但 Windows reticulate 对中文路径转换失败；最终改用 `%LOCALAPPDATA%/USEP-phase2/python/` 中的独立 Python 3.11 环境，TensorFlow 2.21.0、Keras 3.15.1，不修改其他 Python 环境。07 显式禁用自动创建 managed venv。

在仓库根目录安装、试跑的命令示例：

```powershell
$env:LC_ALL = 'English_United States.utf8'
& 'E:/R-4.5.3/bin/Rscript.exe' '环境/install_phase2.R'
# 首次缺少后端时，在独立目录创建 Python 3.11 venv：
$pythonDir = Join-Path $env:LOCALAPPDATA 'USEP-phase2/python'
& 'C:/Users/Lenovo/AppData/Local/Programs/Python/Python311/python.exe' -m venv $pythonDir
& (Join-Path $pythonDir 'Scripts/python.exe') -m pip install 'tensorflow==2.21.0' 'keras==3.15.1'
# Bypass 仅针对这次 PowerShell 进程，不更改系统执行策略。
powershell -NoProfile -ExecutionPolicy Bypass -File '环境/run_phase2_smoke.ps1' -Run '<新的英文或数字ID>' -Workers 2
```

也可以在 `R研究/` 目录直接调用 Rscript，先将 `RETICULATE_PYTHON` 指向上述 venv 的绝对路径。每个脚本默认 smoke；正式模式须审核后显式设置 `USEP_FULL_APPROVED=yes` 并传 `--mode=full`，本次未执行。不要同时启动多份 07，确保 TensorFlow 进程总数不超过六个。

## 试跑范围与输出约定

每个候选的预测日期仅为 **2025-01-01 至 2025-01-14**；重估截止点仅为 **2024-12-31、2025-01-07**。保留完整历史训练、真实早停规则（末 28 天留出；XGBoost 上限 600 轮、patience=20；神经网络上限 100 epochs、patience=10）、500 棵树和 NNAR repeats=20；没有用缩短训练历史或一轮网络来冒充正常训练。

为了检查分支而不进行全网格调参，smoke 使用预先固定的代表候选：

| 脚本 | 初始试跑候选 | 范围 |
|---|---:|---|
| 05 | 26 | SARIMA 和动态回归 M0–M4 每种水平/对数先筛两个低阶规格取一个；DHR 每种变换先筛 K=1/2 取一个；ETS 三规格×两变换；Prophet 三 changepoint prior×两变换 |
| 06 | 9 | 三模型×三目标，每个目标取既定网格第一组，选择不依据试跑成绩 |
| 07 | 10 | LSTM/GRU 各三目标，每个目标取抽样网格第一组；NNAR 单变量/外部变量×水平/对数 |

07 再实际执行种子确认：LSTM/GRU 各三个试跑候选用 2027/2028 重跑；NNAR 临时最佳候选用另两种子重跑。包括初始种子在内，07 的 Tuning 共应为 24 行。LSTM/GRU 最终选择指标为三个种子的验证 RMSE 的算术平均，同时另存三种子均值预测的 RMSE；两者不可混用。正式模式仍按计划先跑 36 个候选，再确认前三名；NNAR 四候选先用 2026 选定，再取三种子均值。

试跑工作簿存为 `results/smoke_<Run>/扩充_05.xlsx`、`扩充_06.xlsx`、`扩充_07.xlsx`；正式模式才写 `results/扩充_05.xlsx` 等。三个表至少含：

- `Daily`：Date、Period、Model、Prediction、Actual；本次 Period 只有 Validation。
- `Tuning`：Model、Candidate、Transform、完整 Settings JSON、Seed、Stage、N、ValidationRMSE、Seconds、Failure、Warnings、Mode。
- `Frozen`：临时选择及全部设置、选择指标、种子、`IsFinal=FALSE`、`SelectionPeriod=SMOKE ONLY`。
- `SeedDaily`：被选候选每个种子的预测、实际值、Cutoff、训练轮数等 FitDetail。
- `Metadata`：版本、mode、指纹和 worker 数；05 另有 `Screening` 保存 AICc 初筛及失败信息。

中间结果位于 Git 忽略的 `results/扩充_中间结果/`，每个候选/种子/重估周单独缓存。指纹包含代码、输入文件、冻结数据、R/包版本、运行模式及 07 的 Python/TensorFlow/Keras 版本；试跑不会被全量运行误读。已完成周可续用，失败周不缓存。正式工作簿存在时拒绝覆盖。

## 审核时请注意的实现选择

1. **S1 修正**：SARIMA 先在初始训练期用 `nsdiffs(test="seas")` 取 D，再在季节差分后用 KPSS `ndiffs` 取 d；动态回归对同一训练期 OLS 残差执行该流程。差分阶数在整个验证/测试流程固定；水平和对数的 AICc 不混排。正式模式在每种变换中按 AICc 留六个，再用全年验证 RMSE 选择。
2. **DHR**：保留动态回归价格滚动统计、滞后外部变量和公众假期，周 Fourier K=3 替换星期哑变量；年度周期 365.25，K=1..5。先用 K=5 的训练残差固定共同 d、D=0，再在该差分形式内按 AICc 联合选择 K 与非季节 p/q；变换按验证 RMSE 选择。这样不会在不同 d 之间比较 K 的 AICc。
3. **S2 修正**：XGBoost/LSTM/GRU 早停前，以内层最后一天重新拟合温度补值和输入处理；确定轮数后才用完整外层历史重新训练。预测特征的 lag/滚动定义及站点 Ridge 算法仍来自只读 01；没有改动旧共享函数。
4. **统一样本**：新增模型拟合样本均舍弃最初 28 天；LSTM/GRU 的序列可以使用此前的输入上下文，标准化不使用外层未来数据。旧 ETS/SARIMA 冻结值原本使用完整价格历史，仍原样保留，不能声称新旧训练样本完全相同。
5. **Prophet**：额外回归量为 TSLM 的非日历特征（价格滞后/滚动统计和外部变量滞后），周/年季节及 01 的新加坡公众假期由 Prophet 处理；不重复加入星期哑变量。水平/对数均参评。Prophet 无 forecast 的 `biasadj` 接口，对数预测以当前训练期 log 残差 smearing 还原；ETS/ARIMA/DHR 的 λ=0 使用 `biasadj=TRUE`。
6. **SVR**：输入和目标都按当前训练期标准化，epsilon=0.1 在标准化目标尺度上定义；预测先还原标准化，再还原目标变换。机器学习、LSTM/GRU 对数目标使用直接 exp；差分预测加实际前日价格。NNAR 的 Box-Cox 使用 nnetar 默认还原；不宣称这些是有偏差修正的条件均值。
7. **TPC 与随机性**：沿用冻结设置中未保留 TPC 的决定；新模型不把测试/九月成绩用于选择。XGBoost 12 组与 02 完全相同；P3 按新计划独立 `set.seed(2026)` 抽 12 组，因此不继承 02 在 XGBoost 抽样后的 RNG 状态。每个 worker 内 TensorFlow 两线程，ranger 两线程；NNAR 周内 `nnetar(model=...)` 仅更新输入。带 xreg 的 NNAR 默认结构需 1276 个权重，超过 nnet 默认上限 1000，故设置 `MaxNWts=100000`；这只放宽实现上限，不改变默认 p/P/k 或 repeats=20。
8. **审核边界**：本次验证可以证明代码分支与表结构跑通，不能证明全年网格所有高阶 ARIMA 均收敛，也不能替代全量结果审核；任何候选失败都会保留日志，模型无成功候选时停止。实际本地 R 与文档不符需在正式计算前确认。

初轮日志 `06_attempt1.log` 记录了日期类型断言失败：R 4.5 的 `seq.Date` 为 integer，而字符转 Date 为 double；新增接入代码改为对日期值逐项严格比较，未放宽日期范围或更改 CSV。启动器还修正了 PowerShell 的退出码读取方式（使用 Start-Process -Wait）。07 首轮日志记录 Python 路径识别和 NNAR 权重上限失败，之后按上述方式修正。失败日志保留，只有通过完整校验的工作簿才作为试跑交接文件。

## 07 试跑完成情况（本地 Claude Code 接手，2026-10-10 19:28）

Codex 额度用完时 07 的重试进程（19:04 启动，Run 仍为 `p1p3_20261010_final2`）还在运行；接手后没有重启，等它自行结束。

- 07 本次成功运行的日志是 `07_retry.stdout.log`、`07_retry.stderr.log`（stderr 只有 TensorFlow 的提示信息）和 `../results/smoke_p1p3_20261010_final2/07_workers.log`。`p1p3_20261010_final2_07.*` 与 `07_attempt1_*` 是同一 Run 名下首次失败的记录。
- `扩充_07.xlsx`：Daily 56 行（LSTM_Improved、GRU、NNAR、LSTM_Original 各 14 天），Tuning 24 行（10 个初始候选 + 14 个种子确认，无失败），Frozen 3 行。
- `试跑校验.R` 对三个工作簿全部 PASS，输出在 `p1p3_20261010_final2_validation.stdout.log`。
- 运行前后 01、02、`模型函数.R`、03、04、原始数据和 `results/` 已有文件与 HEAD 一致（`git diff HEAD` 只有 `.gitignore`）。

审核时另请注意：

1. **05 工作簿的 Tuning.Seconds 不是拟合耗时。** `final2` 与此前的 `final` 指纹相同，05 各周结果直接读自缓存，所以 Seconds 接近 0；首轮的实际耗时见 `05_firstpass_tuning.csv`。`phase_task()` 只统计本次调用的耗时，全量运行若中断续跑，调参日志里的耗时同样会偏小。建议全量前把每周耗时写进周缓存再汇总。本次未改代码。
2. Metadata 中的 `tensorflow 2.20.0` 是 R 包版本；Python 端 TensorFlow 2.21.0、Keras 3.15.1 见 07 工作簿的 `Python_tensorflow`、`Python_keras` 两行。
3. 07 试跑时机器上另有一个占用 14 个进程的无关任务，Seconds 偏大。两周耗时：LSTM/GRU 每个候选每种子约 15–125 秒，NNAR 带外部变量约 90–530 秒。

## 审核通过后的改动与 07 复跑（2026-10-10 19:54，Run `p1p3_20261010_final3`）

全量运行前改了 `扩充公共函数.R`，只重跑 07 的 smoke（5 个 worker）：

1. `phase_run`：所有模型的种子确认任务合并为一次 `phase_map`，所有模型的 Test、September 冻结评估合并为一次 `phase_map`。选择逻辑、种子、各工作表的列和行顺序不变。
2. `phase_task`：每周的拟合 + 预测耗时作为 `seconds` 属性存进周缓存，Tuning 的 Seconds 是各周耗时之和，续跑时读回缓存中的数值。上文第 1 条的问题由此解决。
3. 指纹不再包含 `results/扩充_05.xlsx` 等 05–07 的正式输出。原来一个脚本写出工作簿后，其余脚本续跑时指纹会变、缓存全部失效。这一条不在审核意见里，是本地另加的。
4. `试跑校验.R` 可在目录后加脚本编号，只检查部分工作簿（本次为 `07`）。

结果：`扩充_07.xlsx` 的 Daily、Frozen、SeedDaily 与 `final2` 完全相同，Tuning 除 Seconds 外完全相同；校验全部 PASS（`p1p3_20261010_final3_validation.stdout.log`）。Test/September 的合并路径 smoke 不会执行，另用合成任务比对过新旧 `phase_run` 在 full 模式下的四张表，结果一致。

接口依据：[forecast nnetar 的 model 更新方式](https://pkg.robjhyndman.com/forecast/reference/nnetar.html)、[Keras GRU 接口](https://keras3.posit.co/reference/layer_gru.html)、[Prophet 官方文档](https://facebook.github.io/prophet/docs/additional_topics.html)。
