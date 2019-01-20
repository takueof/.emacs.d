;;; 10-minor-recentf.el --- 設定 - マイナーモード - ファイル履歴保存 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:15:19+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; すべての履歴を保存
 '(recentf-max-saved-items nil)
 ;; ローカル環境にのみ保存
 '(recentf-save-file (convert-standard-filename "~/.emacs.recentf.el")))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-recentf.el ends here
