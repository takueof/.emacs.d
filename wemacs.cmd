@echo off
rem -*- mode: Bat; coding: utf-8-unix; -*-
setlocal
rem ===========================================================================
rem GNU Emacs client `emacsclientw' wrapper
rem Copyright (C) 2014-2023 Taku Watabe
rem Time-stamp: <2023-12-11T10:04:30+09:00>
rem ===========================================================================
rem This program is free software; you can redistribute it and/or modify
rem it under the terms of the GNU General Public License as published by
rem the Free Software Foundation, either version 3 of the License, or
rem (at your option) any later version.
rem
rem This program is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem GNU General Public License for more details.
rem
rem You should have received a copy of the GNU General Public License
rem along with this program.  If not, see <https://www.gnu.org/licenses/>.
rem ===========================================================================
set EMACS_DIR="C:\programs\emacs\bin"
set EMACS_BIN="runemacs.exe"
set EMACS_CLIENT="emacsclientw.exe"

rem Client Server File
rem
rem WARNING: In Windows environment, it is necessary to specify the server file
rem          path to the environment variable EMACS_SERVER_FILE when executing
rem          EMACS_CLIENT.
rem          It is unclear why it is necessary.
set EMACS_SERVER_FILE="%USERPROFILE%\.emacs.server\server"

rem Execute
"%EMACS_DIR%\%EMACS_CLIENT%" -n -a "%EMACS_DIR%\%EMACS_BIN%" %*

rem ===========================================================================
endlocal
