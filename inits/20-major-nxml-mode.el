;;; 20-major-nxml-mode.el --- 設定 - メジャーモード - 汎用 XML 文書

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:17+09:00>

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
  (setq-local require-final-newline nil))

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
