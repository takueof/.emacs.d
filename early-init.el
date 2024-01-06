;;; early-init.el --- "GNU Emacs" early initialize config file -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Taku Watabe
;; Time-stamp: <2024-01-06T22:41:10+09:00>

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

;; This config file can use "GNU Emacs" ONLY.
;; Unsupported other "emacsen" ("XEmacs" and others).

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
;; Don't use "Native Compile".
;;
;; Because, I have problems with slower execution than "Byte Compile" on both
;; my macOS and Windows environments.
;;
(setq no-native-compile t)
;;
;; Don't show `*Warnings*' buffer when asynchronous native compiling.
;;
(setq native-comp-async-report-warnings-errors nil)
;;
;; Prevents garbage collection that occurs at startup.
;;
;; WARNING: Must set the values really need at the end of `init.el'.
;;
(setq gc-cons-threshold most-positive-fixnum)
;;
;; Silence extra message I/O.
;;
;; WARNING: Must set the values really need at the end of `init.el'.
;;
(setq inhibit-message t)
;;
;; Faster font display.
;;
(setq inhibit-compacting-font-caches t)
;;
;; Increase subprocess main memory.
;;
(setq read-process-output-max (* 512 1024 1024)) ; 512MB
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
;; Enable frame resizing in px units.
;;
(setq frame-resize-pixelwise t)
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
;;
;; For `frame'
;;
;; See also:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions
  ;; `lambda' is intentionally used.
  ;; Because this function isn't reused until a frame is created.
  #'(lambda (&optional frame)
      "Initialize `frameset' with `after-make-frame-functions' in FRAME."
      ;; Set all items that don't need to be saved to `desktop' to `:never'.
      (when (listp frameset-filter-alist)
        (let ((availables '(;; List only the items you want to save in `desktop'.
                             width
                             height
                             top
                             left
                             GUI:width
                             GUI:height
                             GUI:top
                             GUI:left)))
          (dolist (item frameset-filter-alist)
            (unless (member (car item) availables)
              ;; Set all other items to `:never'.
              (setcdr item :never)))))))


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
