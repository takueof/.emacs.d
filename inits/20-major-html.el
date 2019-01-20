;;; 20-major-html.el --- 設定 - メジャーモード - (X)HTML -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:19:33+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
;; `sgml-mode.el' ロード時のみ評価されるため、その前に定義する必要がある
(setq-default html-quick-keys t)

;; Dreamweaver のような、終了タグ前コメント挿入を実装
(eval-after-load 'sgml-mode
  '(when (and (fboundp 'sgml-close-tag)
              (require 'close-comment-tag nil :noerror)
              (fboundp 'close-comment-tag)
              (boundp 'sgml-mode-map))
     (defun my-sgml-close-tag-with-comment ()
       (concat (documentation 'sgml-close-tag) "\n"
               "Then, if close tag is element having id or class attribute, insert special comment before close tag.")
       (interactive)
       ;; コメント挿入時、カーソルは終了タグ開始直前に位置していなければならない
       (let ((sgml-close-tag-result (save-excursion (sgml-close-tag))))
         (indent-according-to-mode)
         (close-comment-tag)
         sgml-close-tag-result))

     (define-key sgml-mode-map (kbd "C-c C-c e") 'my-sgml-close-tag-with-comment)
     (define-key sgml-mode-map (kbd "C-c C-c C-e") 'my-sgml-close-tag-with-comment)
     (define-key sgml-mode-map (kbd "C-c e") 'close-comment-tag)))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-html-mode-initialize ()
  "Initialize `html-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

  (when (featurep 'sgml-electric-tag-pair-mode)
    (declare-function sgml-electric-tag-pair-mode "sgml-mode")
    (sgml-electric-tag-pair-mode +1))

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

(add-hook 'html-mode-hook #'my-html-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'html-mode)
    (add-to-list 'auto-mode-alist '("\\.[sx]?html?\\'" . html-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-html.el ends here
