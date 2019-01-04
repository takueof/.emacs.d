;;; 20-major-yaml.el --- 設定 - メジャーモード - YAML

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-03-09T10:56:33+09:00>

;;; Commentary:

;;; Code:

(package-install 'yaml-mode)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(yaml-indent-offset 2))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-yaml-mode-initialize ()
  "Initialize `css-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil))

(add-hook 'yaml-mode-hook #'my-yaml-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . yaml-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-yaml.el ends here
