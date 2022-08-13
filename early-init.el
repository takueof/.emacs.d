;;; early-init.el --- "GNU Emacs" early initialize config file -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Taku Watabe
;; Time-stamp: <2022-08-14T00:05:04+09:00>

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

;; This config file can use "GNU Emacs" ONLY,
;; unsupported other "emacsen" ("XEmacs" and others).

;; WARNING: MUST USE English ONLY because `set-language-environment' not set.

;;; Code:


;; ============================================================================
;; Initialize settings
;; ============================================================================
;; Don't use `custom-set-variables' and `add-to-list'.
;; Because those functions introduces configuration delays.
;; ============================================================================
;;
;; Don't allow additional notes for `custom-set-variables' and
;; `custom-set-faces' to `user-init-file'.
;; Automatic saving is done in another file.
;;
;; WARNING: If you don't write this setting at the beginning,
;;          `custom.el' will not be generated.
;;
(setq custom-file (locate-user-emacs-file "custom.el"))
;;
;; Prevents garbage collection that occurs at startup.
;;
(setq gc-cons-threshold most-positive-fixnum)
;;
;; Silence extra message I/O.
;;
(setq inhibit-message t)
;;
;; Faster font display.
;;
(setq inhibit-compacting-font-caches t)
;;
;; Increase subprocess main memory.
;;
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
;;
;; Package initialization occurs before `user-init-file' is loaded
;; but after `early-init-file'.
;;
(setq package-enable-at-startup nil)
;;
;; Prohibit frame resizing, which is affected by font changes.
;; Because we easily halve startup times.
;;
(setq frame-inhibit-implied-resize t)
;;
;; Disable UI elements to prevent the appearance of unstyled frames
;;
(setq default-frame-alist (append default-frame-alist
                                  '((menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (vertical-scroll-bars))))


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
