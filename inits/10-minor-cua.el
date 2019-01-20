;;; 10-minor-cua.el --- 設定 - マイナーモード - 矩形選択 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:13:03+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'cua-selection-mode)
    ;; 特殊キーバインドは使わない
    (cua-selection-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-cua.el ends here
