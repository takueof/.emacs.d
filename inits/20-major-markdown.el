;;; 20-major-markdown.el --- 設定 - メジャーモード - Markdown ドキュメント -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:33:21+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'markdown-mode))
    (package-install 'markdown-mode))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(markdown-command (or (executable-find "github-markup")
                        (executable-find "markdown")
                        "markdown"))
 '(markdown-command-needs-filename (equal markdown-command
                                          (executable-find "github-markup")))
 '(markdown-coding-system 'utf-8-unix)
 ;; プレビュー用に生成した実 HTML ファイルの残存を防ぐ
 '(markdown-live-preview-delete-export 'delete-on-export))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-markdown-mode-initialize ()
  "Initialize `markdown-mode' before file load."
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

(add-hook 'markdown-mode-hook #'my-markdown-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; プレーンテキストファイルは除外
(setq auto-mode-alist
      (delete '("\\.text\\'" . markdown-mode) auto-mode-alist))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-markdown.el ends here
