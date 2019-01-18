;;; 20-major-haml.el --- 設定 - メジャーモード - Haml -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-18T11:47:48+09:00>

;;; Commentary:

;;; Code:

(package-install 'haml-mode)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-haml-mode-initialize ()
  "Initialize `haml-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

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

(add-hook 'haml-mode-hook #'my-haml-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-haml.el ends here
