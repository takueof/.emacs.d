;;; 10-minor-tooltip.el --- 設定 - マイナーモード - ツールチップ -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-22T21:09:01+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(eval-after-load 'tooltip
  '(if (fboundp 'tooltip-mode)
       ;; 利用しない
       (tooltip-mode -1)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-tooltip.el ends here
