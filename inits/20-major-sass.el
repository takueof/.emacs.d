;;; 20-major-sass.el --- 設定 - メジャーモード - Sass (Type: .sass)

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:22+09:00>

;;; Commentary:

;;; Code:

(package-install 'sass-mode)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-sass-mode-initialize ()
  "Initialize `sass-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil))

(add-hook 'sass-mode-hook #'my-sass-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'sass-mode)
    (add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-sass.el ends here
