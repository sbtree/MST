del *.7z
cd Source
del *.dcu;*.zip;*.7z;*.hpp;*.identcache;*.local;*.rsm;*.bak;*.o /q /s
del __history\*.* /q /s
rd __history
cd ..\Demos
ren cleanup.exe cleanup.ex_
del *.dcu;*.zip;*.7z;*.hpp;*.identcache;*.local;*.rsm;*.bak;*.exe;*.o /q /s
ren cleanup.ex_ cleanup.exe
cleanup
cd ..
if "%1"=="-np" goto end
pause
:end