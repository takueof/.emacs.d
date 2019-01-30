;;; 20-major-lisp.el --- 設定 - メジャーモード - 各種 Lisp 方言 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:33:06+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'elisp-slime-nav))
    (package-install 'elisp-slime-nav))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'my-utils
  '(eval-after-load 'elisp-slime-nav
     '(if (fboundp 'elisp-slime-nav-mode)
          (my-change-lighter elisp-slime-nav-mode nil))))


;; ----------------------------------------------------------------------------
;; 初期化: `lisp-mode'
;; ----------------------------------------------------------------------------
(defun my-lisp-mode-initialize ()
  "Common initialize \"Lisp\" major mode before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)
  (setq-local tab-width 8)

  (smartparens-mode +1)
  (elisp-slime-nav-mode +1)

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

(add-hook 'lisp-mode-hook #'my-lisp-mode-initialize)
(add-hook 'emacs-lisp-mode-hook #'my-lisp-mode-initialize)
(add-hook 'lisp-interaction-mode-hook #'my-lisp-mode-initialize)
(add-hook 'ielm-mode-hook #'my-lisp-mode-initialize)


;; ----------------------------------------------------------------------------
;; 初期化: `lisp-interaction-mode'
;; ----------------------------------------------------------------------------
(defun my-lisp-interaction-mode-initialize ()
  "Initialize `lisp-interaction-mode-initialize' before file load."
  ;; `whitespace-mode' 無効化
  (eval-after-load 'whitespace
    '(progn
       (if (fboundp 'whitespace-mode)
           (whitespace-mode -1))
       (when (boundp 'whitespace-style)
         (setq-local whitespace-style (copy-tree whitespace-style))
         (delete 'empty whitespace-style)))))

(add-hook 'lisp-interaction-mode-hook #'my-lisp-interaction-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-lisp.el ends here
