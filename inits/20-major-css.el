;;; 20-major-css.el --- 設定 - メジャーモード - CSS

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:56:52+09:00>

;;; Commentary:

;;; Code:

(package-install 'css-eldoc)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(css-indent-offset 2))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-css-mode-initialize ()
  "Initialize `css-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil))

(add-hook 'css-mode-hook #'my-css-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'css-eldoc-enable)
    (css-eldoc-enable))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-css.el ends here
