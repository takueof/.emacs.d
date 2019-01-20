;;; 10-minor-saveplace.el --- 設定 - マイナーモード - ファイルごとにカーソル位置を保存 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:15:30+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; ローカル環境にのみ保存
 '(save-place-file (convert-standard-filename "~/.emacs.saveplace.el")))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (require 'saveplace nil :noerror) ; 未 `autoload'
    (setq-default save-place t))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-saveplace.el ends here
