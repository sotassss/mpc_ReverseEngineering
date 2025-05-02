@if not "%~0"=="%~dp0.\%~nx0" start /min cmd /c,"%~dp0.\%~nx0" %* & goto :eof
@echo off


powershell -Command "get-content C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT1.json | Set-Content -Encoding UTF8 C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT.json"


copy /y C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT1.json C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT3.json
