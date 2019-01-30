;;; windows-00-00-environment.el --- 設定 - Windows - 環境 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-30T23:01:44+09:00>

;;; Commentary:

;; `feature' 名 `my-environment'
;;
;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; 環境変数 PATH では不足している分の追加
(let* ((program-files-dir-x86 (or (getenv "PROGRAMFILES\(X86\)")
                                  (getenv "PROGRAMFILES")))
       (paths `(,(concat program-files-dir-x86 "/Aspell/bin")
                "C:/programs/cmigemo/bin"
                "C:/programs/cygwin/bin")))
  (dolist (path paths)
    (setq path (convert-standard-filename path))
    (if (and (file-exists-p path)
             (file-directory-p path))
        (add-to-list 'exec-path path))))


;; GnuTLS trustfiles 追加
(when (gnutls-available-p)
  ;; グローバル変数未定義の場合に備える
  (if (not (boundp 'gnutls-trustfiles))
      (defvar gnutls-trustfiles nil))
  ;; 追加開始
  (let ((trust-files '("C:/programs/cygwin/usr/ssl/certs/ca-bundle.crt")))
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

;;; windows-00-00-environment.el ends here
