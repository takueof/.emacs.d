;;; 10-minor-nsm.el --- 設定 - マイナーモード - Network Security Manager -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:14:54+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; ローカル環境にのみ保存
 '(nsm-settings-file (convert-standard-filename
                      "~/.emacs.network-security.data")))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-nsm.el ends here
