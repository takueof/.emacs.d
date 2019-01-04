;;; 20-major-apache.el --- 設定 - メジャーモード - Apache

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:41+09:00>

;;; Commentary:

;;; Code:

(package-install 'apache-mode)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-apache-mode-initialize ()
  "Initialize `apache-mode' before file load."
  ;; EMPTY
  )

(add-hook 'apache-mode-hook #'my-apache-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'apache-mode)
    (add-to-list 'auto-mode-alist '("\\.conf\\'" . apache-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-apache.el ends here
