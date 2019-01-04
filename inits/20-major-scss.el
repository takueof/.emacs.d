;;; 20-major-scss.el --- 設定 - メジャーモード - Sass (Type: .scss)

;; Copyright (C) 2013-2018 Taku Watabe
;; Time-stamp: <2018-06-12T14:20:35+09:00>

;;; Commentary:

;;; Code:

(package-install 'scss-mode)


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
  (setq-local require-final-newline nil))

(add-hook 'scss-mode-hook #'my-scss-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-scss.el ends here
