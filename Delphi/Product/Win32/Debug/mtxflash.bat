mtxflash.exe stmi.ini

if ERRORLEVEL 1 goto error
exit

:error
@echo.
@echo +----------------------------------------------------------------+
@echo +                                                                +
@echo +  !! ERROR !!                                                   +
@echo +  Fehler beim Update Bootloader / Firmware                      +
@echo +  Ueberpruefen Sie die Anschluesse und Versuchen es noch einmal.+
@echo +                                                                +
@echo +----------------------------------------------------------------+
@echo.

