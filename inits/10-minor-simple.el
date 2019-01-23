;;; 10-minor-simple.el --- 設定 - マイナーモード - 基礎編集コマンド集 -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-23T10:51:24+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; 暫定マークを使用
(if (fboundp 'transient-mark-mode)
    (transient-mark-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-simple.el ends here
