@echo off

set R_PATH=

for /f "delims=" %%i in ('dir "%ProgramFiles%\R" /ad /b') do set R_PATH=%ProgramFiles%\R\%%i

echo %R_PATH%

"%R_PATH%\bin\Rscript" -e "shiny::runApp('../', launch.browser = TRUE)"

pause
