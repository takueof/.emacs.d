;;; early-init.el --- "GNU Emacs" early initialize -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Taku WATABE
;; Time-stamp: <2026-07-22T07:40:45+09:00>

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

;; This config file support "GNU Emacs" ONLY.
;; Other "emacsen" are unsupported (e.g. "XEmacs").

;; WARNING: This file MUST ONLY use English.
;;          Because `set-language-environment' has not been executed.

;;; Code:

;;
;; Prevents garbage collection that occurs at startup.
;;
(setopt gc-cons-threshold 5368709120) ; 5GB
;;
;; Increase subprocess main memory.
;;
(setopt read-process-output-max 1073741824) ; 1GB
;;
;; Increase log buffer maximum number of lines.
;;
(setopt message-log-max 2000)
;;
;; Increase `undo' limit.
;;
(setopt undo-limit 600000)
(setopt undo-strong-limit 900000) ; (= 1.5 (/ undo-strong-limit undo-limit))
;;
;; Don't generate lockfiles.
;;
(setopt create-lockfiles nil)
;;
;; Use system’s trash can.
;;
(setopt delete-by-moving-to-trash t)
;;
;; Use right fringe that indicate buffer boundaries and scrolling.
;;
(setopt indicate-buffer-boundaries 'right)
;;
;; Use fringe that indicate empty lines.
;;
(setopt indicate-empty-lines t)
;;
;; Don't compact font caches during garbage collection.
;;
(setopt inhibit-compacting-font-caches t)
;;
;; Don't use the default face's font for symbols and punctuation.
;; Reverting to less than GNU Emacs v25.0 behavior.
;;
(setopt use-default-font-for-symbols nil)
;;
;; Make mouse pointer invisible while typing.
;;
(setopt make-pointer-invisible t)
;;
;; Use short answers (to use "y/n").
;;
(setopt use-short-answers t)
;;
;; Start scrolling at 10 line from the top and bottom of the window.
;;
(setopt next-screen-context-lines 10)
(setopt scroll-margin 10)
;;
;; Respect the value of ‘truncate-lines’.
;;
(setopt truncate-partial-width-windows nil)
;;
;; Use ignore case when searching and matching.
;;
(setopt case-fold-search t)
;;
;; Use ignore case when reading a buffer name completion.
;;
(setopt read-buffer-completion-ignore-case t)
;;
;; Use custom end-of-line format string in mode line.
;;
(setopt eol-mnemonic-dos "[RN]")
(setopt eol-mnemonic-mac "[CR]")
(setopt eol-mnemonic-undecided "[??]")
(setopt eol-mnemonic-unix "[LF]")
;;
;; Prohibit frame resizing, which is affected by font changes.
;;
(setopt frame-inhibit-implied-resize t)
;;
;; Enable frame resizing in px units.
;;
(setopt frame-resize-pixelwise t)
;;
;; Use simplest frame title.
;;
(setopt frame-title-format "GNU Emacs")
;;
;; Don't use bell sound.
;;
(setopt ring-bell-function 'ignore)
;;
;; Don't use system caret (Windows ONLY).
;;
;; See:
;; https://mementomori.social/@tml/116416045226298692
;;
(setopt w32-use-visible-system-caret nil)
;;
;; Don't send <altgr> keycode when left <alt> + right <ctrl> key down (Windows ONLY).
;;
;; See:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Keyboard.html
;;
(setopt w32-recognize-altgr nil)
;;
;; Add processes that intentionally output CRLF end-of-line (Windows ONLY).
;;
;; See:
;; https://www.emacswiki.org/emacs/ShellMode#h5o-1
;;
(if (member system-type '(cygwin windows-nt ms-dos))
    (add-to-list 'process-coding-system-alist
                 '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix))))
;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; early-init.el ends here
