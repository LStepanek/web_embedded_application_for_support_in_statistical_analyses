@echo off
setlocal

set "R_PATH="

for /f "delims=" %%i in ('dir "%ProgramFiles%\R" /ad /b /o-n') do (
    if not defined R_PATH set "R_PATH=%ProgramFiles%\R\%%i"
)

echo R is located at: "%R_PATH%"

if not exist "%R_PATH%\bin\Rscript.exe" (
    echo Error: Rscript.exe not found in "%R_PATH%\bin"
    exit /b 1
)

"%R_PATH%\bin\Rscript.exe" -e "shiny::runApp('../', launch.browser = TRUE)"
