;;; windows-00-keybinds.el --- 設定 - Windows - キーバインド -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:21:07+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 右 <Alt> + 左 <Ctrl> の組み合わせで <AltGr> が発送されないようにする
 ;; <AltGr> は独自のキーコードであり、C-M- であるとみなされない
 ;;
 ;; see also:
 ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Keyboard.html
 '(w32-recognize-altgr nil))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-00-keybinds.el ends here
