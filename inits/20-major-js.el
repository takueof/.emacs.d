;;; 20-major-js.el --- 設定 - メジャーモード - JavaScript -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:19:45+09:00>

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
        (setq-local end-of-defun-function #'js2-end-of-defun)))

  ;; EditorConfig 対応
  (eval-after-load 'editorconfig
    '(if (hash-table-p editorconfig-properties-hash)
         (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                (indent-style (equal indent-style-data "tab"))
                (insert-final-newline-data (gethash 'insert_final_newline editorconfig-properties-hash))
                (insert-final-newline (equal insert-final-newline-data "true")))
           (if (not (equal indent-tabs-mode indent-style))
               (setq-local indent-tabs-mode indent-style))
           (if (not (equal require-final-newline insert-final-newline))
               (setq-local require-final-newline insert-final-newline))))))

(add-hook 'js-mode-hook #'my-js-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; WARNING: `js2-mode' を利用するため起動させない
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
