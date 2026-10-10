param(
    [string]$Rscript = 'E:/R-4.5.3/bin/Rscript.exe',
    [string]$Run = (Get-Date -Format 'yyyyMMdd_HHmmss'),
    [int]$Workers = 2,
    [string]$Python = (Join-Path $env:LOCALAPPDATA 'USEP-phase2/python/Scripts/python.exe')
)
$ErrorActionPreference = 'Stop'
if ($Workers -lt 1 -or $Workers -gt 6) { throw 'Workers must be between 1 and 6.' }
$repo = Split-Path -Parent $PSScriptRoot
$study = Join-Path $repo 'R研究'
$env:LC_ALL = 'English_United States.utf8'
$env:RETICULATE_PYTHON = $Python
if (!(Test-Path -LiteralPath $env:RETICULATE_PYTHON)) { throw 'Install the phase2 Python backend first; see smoke_logs/README.md.' }
$logs = Join-Path $study 'smoke_logs'
New-Item -ItemType Directory -Force -Path $logs | Out-Null
$scripts = @('05_传统模型修正与扩充.R', '06_机器学习扩充.R', '07_神经网络扩充.R')
foreach ($script in $scripts) {
    $id = $script.Substring(0, 2)
    $stdout = Join-Path $logs ($Run + '_' + $id + '.stdout.log')
    $stderr = Join-Path $logs ($Run + '_' + $id + '.stderr.log')
    if ((Test-Path -LiteralPath $stdout) -or (Test-Path -LiteralPath $stderr)) { throw 'Choose a new Run; existing logs are preserved.' }
    $process = Start-Process -FilePath $Rscript -ArgumentList @($script, '--mode=smoke', "--workers=$Workers", "--run=$Run") -WorkingDirectory $study -WindowStyle Hidden -Wait -PassThru -RedirectStandardOutput $stdout -RedirectStandardError $stderr
    if ($process.ExitCode -ne 0) { throw "$id failed; see $stdout and $stderr" }
}
$check = Start-Process -FilePath $Rscript -ArgumentList @('试跑校验.R', "results/smoke_$Run") -WorkingDirectory $study -WindowStyle Hidden -Wait -PassThru -RedirectStandardOutput (Join-Path $logs ($Run + '_validation.stdout.log')) -RedirectStandardError (Join-Path $logs ($Run + '_validation.stderr.log'))
if ($check.ExitCode -ne 0) { throw 'Workbook validation failed.' }
Write-Output "Smoke complete: R研究/results/smoke_$Run. Stop here for review; no full run was started."
