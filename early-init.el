;;; early-init.el --- "GNU Emacs" early initialize -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Taku WATABE
;; Time-stamp: <2026-06-26T20:08:27+09:00>

;; Author: Taku WATABE <taku.eof@gmail.com>

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
(setopt custom-file (if (member system-type '(ms-dos windows-nt))
                        (concat temporary-file-directory "custom.el")
                      null-device))
;;
;; Don't generate lockfiles.
;;
(setopt create-lockfiles nil)
;;
;; Don't generate auto backup files.
;;
(setopt auto-save-default nil)
(setopt make-backup-files nil)
;;
;; Save locally ONLY.
;;
(setopt auto-save-list-file-prefix "~/.emacs-auto-save-list.d/.saves-")
(setopt bookmark-default-file "~/.emacs-bookmark.eld")
(setopt ido-save-directory-list-file "~/.emacs-ido-save-directory-list.eld")
(setopt nsm-settings-file "~/.emacs-network-security.eld")
(setopt recentf-save-file "~/.emacs-recentf.eld")
(setopt save-place-file "~/.emacs-saveplace.eld")
(setopt savehist-file "~/.emacs-savehist.eld")
(setopt server-auth-dir "~/.emacs-server.d")
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
(setopt gc-cons-threshold 1073741824) ; 1GB
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
;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
