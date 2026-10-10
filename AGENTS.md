# AGENTS.md

本仓库的项目规则写在 `CLAUDE.md`，工作清单写在 `修订执行计划.md`。开始任何工作前先读这两个文件，其中的规则同样适用于 Codex 等其他代码助手：

- 不重跑 02，不覆盖 `R研究/results/` 中已有文件，不修改 01、02、`模型函数.R` 和原始数据。
- 新脚本先小规模试跑并 push，经审核后再全量运行。
- 允许 commit 并 push 到 `main`；push 前先 `git pull --rebase`；提交信息以任务编号开头（如 `P1:`、`Audit:`）。
- 上级目录中其他项目的说明文件（如 `d:\Desktop\CLAUDE.md`）不适用于本仓库。
