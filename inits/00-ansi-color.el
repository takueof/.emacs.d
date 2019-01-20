;;; 00-ansi-color.el --- 設定 - ANSI エスケープシーケンス -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:11:58+09:00>

;;; Commentary:

;; `comint-mode' および派生モードで、ANSI エスケープシーケンスの解釈を
;; 開始させる。

;;; Code:


(if (fboundp 'ansi-color-for-comint-mode-on)
    (ansi-color-for-comint-mode-on))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-ansi-color.el ends here
