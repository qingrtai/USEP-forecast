2026 年 9 月五站日度气象原始数据（任务 L1）
==========================================

来源：新加坡气象署（Meteorological Service Singapore）Historical Daily Records
      https://www.weather.gov.sg/climate-historical-daily/
下载时间：2026-10-10 14:36（UTC+8），用 curl 直接下载，HTTP 状态均为 200
文件内容：下载后未做任何修改（未改编码、换行、列名、数值）

1. 文件清单
-----------
站点代码取自文件名/网址。“TempSG.xlsx 列名”是 01 脚本使用的站点键；
“文件内 Station 列”是 CSV 第一列的实际取值。

代码  TempSG.xlsx 列名  文件内 Station 列            字节数  网址
S24   Changi            Changi Met. Station          3580    https://www.weather.gov.sg/files/dailydata/DAILYDATA_S24_202609.csv
S44   Jurong_West       Nanyang Avenue               3430    https://www.weather.gov.sg/files/dailydata/DAILYDATA_S44_202609.csv
S109  Ang_Mo_Kio        Ang Mo Kio Avenue 5          3468    https://www.weather.gov.sg/files/dailydata/DAILYDATA_S109_202609.csv
S104  Admiralty         Woodlands Avenue 9           3550    https://www.weather.gov.sg/files/dailydata/DAILYDATA_S104_202609.csv
S60   Sentosa_Island    Artillery Avenue (Sentosa)   3748    https://www.weather.gov.sg/files/dailydata/DAILYDATA_S60_202609.csv

SHA-256：
d5951dbf97538ca9c761dc671a7833a0d339efed9f7f892e406982fc04f45e3b  DAILYDATA_S24_202609.csv
1999036ad0206b56cdbebe2a558e5dcd9ec97f844a6cc8723bf7c5fbd410d01b  DAILYDATA_S44_202609.csv
e5fbb6ceae39a096d780fc2ba7499e350a05cb90cac1ffbf5442b0b07ae2b86a  DAILYDATA_S109_202609.csv
b0113a0cdc4355f3e773d52c5b4763f1683fc7f331e125e0a190d6c1b99dd6e9  DAILYDATA_S104_202609.csv
d5e7be6fc1fd0fb5af87121147258550739b6e9c849e6599ae5b91f9091f14fe  DAILYDATA_S60_202609.csv

2. 检查结果
-----------
五个文件均满足：
- 1 行表头 + 30 行数据，每行 13 列，无残缺行。
- 日期（Year, Month, Day 三列）完整覆盖 2026-09-01 至 2026-09-30，无重复、无缺日。
- 表头完全相同，含 “Mean Temperature (°C)” 列（第 9 列）。
- 平均气温有值的各行均满足 最低气温 <= 平均气温 <= 最高气温。

平均气温缺测（影响研究）：
- S60 Sentosa：2026-09-29、2026-09-30 两天缺测（最高、最低气温同日也缺测）。
- 其余四站 30 天齐全。五站合计 150 个站点日中 148 个有平均气温。
- Changi（S24）无缺测，满足 01 脚本中 !anyNA(temp$Changi) 的要求。
- 按 修订执行计划.md C2 第 1 步，S60 这两天需用 01 中同一 Ridge 方法补值。

其他列缺测（研究未使用，仅记录）：
- S109 Ang Mo Kio：Mean Wind Speed、Max Wind Speed 在 2026-09-11 至 2026-09-18 共 8 天缺测；气温齐全。
- S24、S44、S104：所有列均无缺测。

平均气温概况（°C）：
代码  有效天数  最小值  最大值  均值
S24   30        26.7    30.1    29.28
S44   30        25.3    30.1    28.78
S109  30        26.3    30.5    29.26
S104  30        25.8    30.2    29.06
S60   28        26.4    30.0    29.02

与现有数据的衔接：TempSG.xlsx 的 TempSG_Five_Stations 表最后一天是 2026-08-31，
9 月数据正好接续，无重叠、无间隔。TempSG.xlsx 未修改。

3. 读取时的注意事项
-------------------
- 编码为 UTF-8 带 BOM；不处理 BOM 时第一列名会变成 “﻿Station”。
- “°” 是 U+00B0（字节 C2 B0）。建议按 “Mean Temperature” 模糊匹配列名。
- 缺测标记是 ASCII 连字符 “-”（U+002D），前后没有空格；本批文件中没有出现 “—”。
- 数值单元格左侧用空格补齐（如 “    29.7”），转数值前需去空格。
- 换行符为 LF，文件末尾没有换行符（所以 wc -l 显示 30，实际是 31 行）。
- Station 列的写法与 TempSG.xlsx 中的站名（Changi、Jurong (West)、Ang Mo Kio、
  Admiralty、Sentosa Island）不同，匹配站点请用文件名中的站点代码，不要用 Station 列。

4. 关于 .gitattributes
----------------------
本目录的 .gitattributes 把 *.csv 设为 -text，使 git 在任何系统上检出时都不转换换行符，
保证文件与下载时逐字节一致（可用上面的 SHA-256 核对）。
