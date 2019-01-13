;;; 10-minor-flymake.el --- 設定 - マイナーモード - 自動静的解析ランナー -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-13T20:31:24+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(flymake-run-in-place nil))

(eval-after-load 'my-utils
  '(eval-after-load 'flymake
     '(if (fboundp 'flymake-mode)
          (my-change-lighter flymake-mode nil))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-flymake.el ends here
