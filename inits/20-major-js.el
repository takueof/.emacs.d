;;; 20-major-js.el --- 設定 - メジャーモード - JavaScript

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:03+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(js-indent-level 4))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-js-mode-initialize ()
  "Initialize `js-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

  ;; 一部機能を `js2-mode' から借用
  (when (require 'js2-mode nil :noerror)
    (if (fboundp 'js2-indent-line)
        (setq-local indent-line-function #'js2-indent-line))
    (if (fboundp 'js2-indent-region)
        (setq-local indent-region-function #'js2-indent-region))
    (if (fboundp 'js2-line-break)
        (setq-local comment-line-break-function #'js2-line-break))
    (if (fboundp 'js2-beginning-of-defun)
        (setq-local beginning-of-defun-function #'js2-beginning-of-defun))
    (if (fboundp 'js2-end-of-defun)
        (setq-local end-of-defun-function #'js2-end-of-defun))))

(add-hook 'js-mode-hook #'my-js-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; (when (fboundp 'js-mode)
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
;;   (add-to-list 'auto-mode-alist '("\\.pac\\'" . js-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-js.el ends here
