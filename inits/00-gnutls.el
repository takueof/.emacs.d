;;; 00-gnutls.el --- 設定 - GnuTLS -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-13T12:18:52+09:00>

;;; Commentary:

;; GNU/Linux ならびに UNIX 以外の環境でパスが認識されず、フリーズする問題を回避する。
;;
;; see also:
;; https://twitter.com/robario/statuses/364694728299659264

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(gnutls-trustfiles (mapcar 'expand-file-name gnutls-trustfiles)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-gnutls.el ends here
