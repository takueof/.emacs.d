;;; early-init.el --- "GNU Emacs" early initialize -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Taku WATABE
;; Time-stamp: <2026-05-24T13:16:45+09:00>

;; Author: Taku Watabe <taku.eof@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This config file is for "GNU Emacs" ONLY.
;; Unsupported other "emacsen" ("XEmacs" and others).

;; WARNING: MUST use English ONLY because `set-language-environment' not set.

;;; Code:

;;
;; Don't edit and create `custom-file'.
;;
(setopt custom-file null-device)
;;
;; Don't generate lockfiles.
;;
(setopt create-lockfiles nil)
;;
;; Don't generate auto backup files.
;;
(setopt auto-save-default nil)
(setopt make-backup-files nil)
(setopt auto-save-list-file-prefix "~/.emacs-auto-save-list/.saves-")
;;
;; Precompute activation actions to speed up startup.
;;
(setopt package-quickstart t)
;;
;; Don't use "Native compile" because that caused crash and delay.
;;
(setopt no-native-compile t)
;;
;; Don't show `*Warnings*' buffer when asynchronous native compiling.
;;
(setopt native-comp-async-report-warnings-errors nil)
;;
;; Prevents garbage collection that occurs at startup.
;;
;; WARNING: Must set the values really need at the end of `init.el'.
;;
(setopt gc-cons-threshold most-positive-fixnum)
;;
;; Increase subprocess main memory.
;;
(setopt read-process-output-max 1073741824) ; 1GB
;;
;; Prohibit frame resizing, which is affected by font changes.
;;
(setopt frame-inhibit-implied-resize t)
;;
;; Enable frame resizing in px units.
;;
(setopt frame-resize-pixelwise t)
;;
;; Disable UI elements
;;
(blink-cursor-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tab-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
