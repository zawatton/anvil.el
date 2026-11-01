@echo off
rem Native Windows MCP entry point; Python supervises validated image restore.
python "%~dp0anvil-standalone-image.py" %*
