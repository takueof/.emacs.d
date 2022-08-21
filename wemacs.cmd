@echo off
setlocal
rem ---------------------------------------------------------------------------
rem GNU Emacs クライアント `emacsclientw' ラッパー
rem Copyright (C) 2014-2022 Taku Watabe
rem Time-stamp: <2022-08-21T11:47:57+09:00>
rem ---------------------------------------------------------------------------
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
rem ---------------------------------------------------------------------------
set EMACS_DIR=C:\programs\emacs\bin
set EMACS_BIN=runemacs.exe
set EMACS_CLIENT=emacsclientw.exe

rem クライアントサーバファイル
rem
rem WARNING: Windows 環境は EMACS_CLIENT 実行時に環境変数 EMACS_SERVER_FILE へ
rem          サーバファイルのパスを明示する必要がある
rem          なぜ必要かは不明
set EMACS_SERVER_FILE=%APPDATA%\.emacs.server\server

rem 実行
"%EMACS_DIR%\%EMACS_CLIENT%" -n -a "%EMACS_DIR%\%EMACS_BIN%" %*

rem ---------------------------------------------------------------------------
endlocal
rem Local Variables:
rem coding: japanese-cp932-dos
rem End
