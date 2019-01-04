;;; 00-gnutls.el --- 設定 - GnuTLS

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-13T00:47:43+09:00>

;;; Commentary:

;; GNU Linux/UNIX 以外の環境でパスが認識されず、フリーズする問題を回避する。
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
