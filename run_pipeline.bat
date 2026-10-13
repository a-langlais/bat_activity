@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "DATA_DIR=%CD%"
set "REPO_DIR=%~dp0"
set "PIPELINE_STATUS=0"

if "%~2"=="" goto usage

set "SOFTWARE=%~1"
set "MILIEU=%~2"

if /I not "%SOFTWARE%"=="SonoChiro" if /I not "%SOFTWARE%"=="Tadarida" goto usage
if /I not "%MILIEU%"=="ouvert" if /I not "%MILIEU%"=="ferme" goto usage

shift
shift
set "INDICATOR_ARGS="

:collect_args
if "%~1"=="" goto find_rscript
set "INDICATOR_ARGS=!INDICATOR_ARGS! "%~1""
shift
goto collect_args

:find_rscript
if defined RSCRIPT if exist "%RSCRIPT%" goto run_pipeline

set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"

if not defined RSCRIPT (
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
  )
)

if not defined RSCRIPT (
  echo ERROR: Rscript.exe was not found. Install R or add Rscript.exe to PATH.
  exit /b 1
)

:run_pipeline
if not exist "%REPO_DIR%batactivity\R\standardize.R" (
  echo ERROR: The batch file must remain at the repository root.
  exit /b 1
)

echo Data directory: %DATA_DIR%
echo Rscript: %RSCRIPT%
echo.

pushd "%REPO_DIR%" || exit /b 1

echo [1/3] Standardizing %SOFTWARE% CSV files...
"%RSCRIPT%" "scripts\03_standardiser_lot.R" "%SOFTWARE%" --dossier "%DATA_DIR%"
if errorlevel 1 (
  set "PIPELINE_STATUS=!ERRORLEVEL!"
  goto pipeline_failed
)

echo.
echo [2/3] Calculating passive indicators...
"%RSCRIPT%" "scripts\04_indicateurs_passifs_lot.R" !INDICATOR_ARGS! --dossier "%DATA_DIR%"
if errorlevel 1 (
  set "PIPELINE_STATUS=!ERRORLEVEL!"
  goto pipeline_failed
)

echo.
echo [3/3] Applying detectability correction for a %MILIEU% environment...
"%RSCRIPT%" "scripts\05_correction_detectabilite.R" --milieu "%MILIEU%" --dossier "%DATA_DIR%"
if errorlevel 1 (
  set "PIPELINE_STATUS=!ERRORLEVEL!"
  goto pipeline_failed
)

popd
echo.
echo Pipeline completed successfully.
exit /b 0

:pipeline_failed
popd
echo.
echo ERROR: The pipeline stopped because one step failed.
exit /b !PIPELINE_STATUS!

:usage
echo Usage:
echo   run_passive_pipeline.bat ^<SonoChiro^|Tadarida^> ^<ouvert^|ferme^> [indicator options]
echo.
echo Run this command from the directory containing the *_clean.csv files.
echo Remaining options are passed to 04_indicateurs_passifs_lot.R.
echo.
echo Examples:
echo   C:\path\to\bat_activity\run_passive_pipeline.bat SonoChiro ouvert
echo   C:\path\to\bat_activity\run_passive_pipeline.bat Tadarida ferme --ville Bordeaux --ephemerides 30 30
exit /b 2
