;;; early-init.el --- "GNU Emacs" early initialize config file -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Taku Watabe
;; Time-stamp: <2021-02-06T23:57:41+09:00>

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
(custom-set-variables
 ;;
 ;; Don't allow additional notes for `custom-set-variables' and
 ;; `custom-set-faces' to `user-init-file'.
 ;; Automatic saving is done in another file.
 ;;
 '(custom-file (locate-user-emacs-file "custom.el"))
 ;;
 ;; Package initialization occurs before `user-init-file' is loaded
 ;; but after `early-init-file'.
 ;;
 '(package-enable-at-startup nil)
 ;;
 ;; Prohibit frame resizing, which is affected by font changes.
 ;; Because we easily halve startup times.
 ;;
 '(frame-inhibit-implied-resize t))


;; ============================================================================
;; Disable UI elements to prevent the appearance of unstyled frames
;; ============================================================================
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
