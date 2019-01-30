;;; 20-major-yaml.el --- 設定 - メジャーモード - YAML -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:34:58+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'yaml-mode))
    (package-install 'yaml-mode))


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
