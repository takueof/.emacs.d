;;; 20-major-json.el --- 設定 - メジャーモード - JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

(package-install 'json-mode)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-json-mode-initialize ()
  "Initialize `json-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)
  (setq-local js-indent-level 2)
  (setq-local tab-width 2)

  ;; EditorConfig 対応
  (eval-after-load 'editorconfig
    '(if (boundp 'editorconfig-properties-hash)
         (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                (indent-style (equal indent-style-data "tab"))
                (insert-final-newline-data (gethash 'insert_final_newline editorconfig-properties-hash))
                (insert-final-newline (equal insert-final-newline-data "true")))
           (if (not (equal indent-tabs-mode indent-style))
               (setq-local indent-tabs-mode indent-style))
           (if (not (equal require-final-newline insert-final-newline))
               (setq-local require-final-newline insert-final-newline))))))

(add-hook 'json-mode-hook #'my-json-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.bowerrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.ftppass\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.htmlhintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.htmllintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jscsrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.stylelintrc\\'" . json-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-json.el ends here
