;;; 20-major-scss.el --- 設定 - メジャーモード - Sass (Type: .scss) -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:34:25+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'scss-mode))
    (package-install 'scss-mode))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; インデントは css-indent-offset に準ずる
 ;; 共通設定は `20-major-css.el' 参照
 ;; '(css-indent-offset 2)
 ;;
 ;; コンパイルは常に手動（保存時は何もしない）
 ;; 各種ツール経由でコンパイルされうるため
 '(scss-compile-at-save nil))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-scss-mode-initialize ()
  "Initialize `scss-mode' before file load."
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

(add-hook 'scss-mode-hook #'my-scss-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-scss.el ends here
