;;; 20-major-nxml-mode.el --- 設定 - メジャーモード - 汎用 XML 文書 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 要素インデント幅
 '(nxml-child-indent 2)
 ;; 属性インデント幅
 '(nxml-attribute-indent 0)
 ;; スラッシュ入力で終了タグを自動補完
 '(nxml-slash-auto-complete-flag t)
 '(nxml-bind-meta-tab-to-complete-flag t)
 ;; C-M-k で下位を含む要素全体を kill
 '(nxml-sexp-element-flag t)
 ;; 文字および実体参照のグリフを表示
 '(nxml-char-ref-display-glyph-flag t))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-nxml-mode-initialize ()
  "Initialize `nxml-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

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

(add-hook 'nxml-mode-hook #'my-nxml-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; (if (fboundp 'nxml-mode)
;;     (add-to-list 'auto-mode-alist '("\\.[sx]?html?\\'" . nxml-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-nxml-mode.el ends here
