@echo off
setlocal

REM ============================================================
REM Run lintr checks on the project
REM Installs lintr if not already installed
REM ============================================================

set "R_PATH="

for /f "delims=" %%i in ('dir "%ProgramFiles%\R" /ad /b /o-n') do (
    if not defined R_PATH set "R_PATH=%ProgramFiles%\R\%%i"
)

echo R is located at: "%R_PATH%"

if not exist "%R_PATH%\bin\Rscript.exe" (
    echo Error: Rscript.exe not found in "%R_PATH%\bin"
    exit /b 1
)

REM Install lintr if not already installed
echo Checking for lintr package...
"%R_PATH%\bin\Rscript.exe" -e "if (!requireNamespace('lintr', quietly = TRUE)) install.packages('lintr', repos='https://cloud.r-project.org')"

echo Running lintr checks with custom settings...

"%R_PATH%\bin\Rscript.exe" -e "lintr::lint_dir('../')

IF %ERRORLEVEL% NEQ 0 (
    echo Lintr found issues.
    exit /b %ERRORLEVEL%
) ELSE (
    echo No lint issues found.
)
