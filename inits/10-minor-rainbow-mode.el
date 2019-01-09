;;; 10-minor-rainbow-mode.el --- 設定 - マイナーモード - 自動カラー表示

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:36:01+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'rainbow-mode
  '(when (boundp 'rainbow-html-colors-major-mode-list)
     ;; (X)HTML & CSS カラー
     (add-to-list 'rainbow-html-colors-major-mode-list 'sass-mode)
     (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode)
     (add-to-list 'rainbow-html-colors-major-mode-list 'less-mode)))

(eval-after-load 'my-utils
  '(eval-after-load 'rainbow-mode
     '(my-change-lighter rainbow-mode nil)))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'rainbow-mode)
    (rainbow-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-rainbow-mode.el ends here
