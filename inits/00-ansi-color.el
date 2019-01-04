;;; 00-ansi-color.el --- 設定 - ANSI エスケープシーケンス

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-13T00:47:02+09:00>

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
