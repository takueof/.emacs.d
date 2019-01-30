;;; 20-major-apache.el --- 設定 - メジャーモード - Apache -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:30:54+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'apache-mode))
    (package-install 'apache-mode))


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
