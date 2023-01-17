@echo off

echo Start build...
if EXIST build del /Q build\*.*
if NOT EXIST build mkdir build

echo Assembling code...
bin\vasmarm_std_win32.exe -L build\fiq.txt -m250 -Fbin -opt-adr -o build\fiq_code.bin fiq_code.asm

if %ERRORLEVEL% neq 0 (
	echo Failed to assemble code.
	exit /b 1
)

bin\vasmarm_std_win32.exe -L build\compile.txt -m250 -Fbin -opt-adr -o build\rasterman.bin rasterman.asm

if %ERRORLEVEL% neq 0 (
	echo Failed to assemble code.
	exit /b 1
)

if %ERRORLEVEL% neq 0 (
	echo Failed to link code.
	exit /b 1
)

echo Copying files...
set HOSTFS=..\arculator\hostfs
copy "build\rasterman.bin" "%HOSTFS%\RMv030,ffa"
