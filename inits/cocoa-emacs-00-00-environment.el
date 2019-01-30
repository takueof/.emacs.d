;;; cocoa-emacs-00-00-environment.el --- 設定 - macOS (Cocoa) - 環境 -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-30T23:01:48+09:00>

;;; Commentary:

;; `feature' 名 `my-environment'
;;
;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; GnuTLS trustfiles 追加
(when (gnutls-available-p)
  ;; グローバル変数未定義の場合に備える
  (if (not (boundp 'gnutls-trustfiles))
      (defvar gnutls-trustfiles nil))
  ;; 追加開始
  ;;
  ;; see also:
  ;; https://emacs.stackexchange.com/a/18070
  (let ((trust-files '("/usr/local/etc/libressl/cert.pem"
                       "/usr/local/etc/openssl/cert.pem"
                       "/etc/ssl/cert.pem")))
    (dolist (trust-file trust-files)
      (setq trust-file (convert-standard-filename trust-file))
      (if (file-exists-p trust-file)
          (add-to-list 'gnutls-trustfiles trust-file t)))))


(provide 'my-environment)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; cocoa-emacs-00-00-environment.el ends here
