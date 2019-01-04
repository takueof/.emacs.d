;;; 20-major-html.el --- 設定 - メジャーモード - (X)HTML

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:01+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
;; `sgml-mode.el' ロード時のみ評価されるため、その前に定義する必要がある
(setq-default html-quick-keys t)

;; Dreamweaver のような、終了タグ前コメント挿入を実装
(eval-after-load 'sgml-mode
  '(progn
     (when (and (fboundp 'sgml-close-tag)
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
       (define-key sgml-mode-map (kbd "C-c e") 'close-comment-tag))))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-html-mode-initialize ()
  "Initialize `html-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

  (when (featurep 'sgml-electric-tag-pair-mode)
    (declare-function sgml-electric-tag-pair-mode "sgml-mode")
    (sgml-electric-tag-pair-mode +1)))

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
