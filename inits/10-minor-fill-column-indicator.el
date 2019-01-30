;;; 10-minor-fill-column-indicator.el --- 設定 - マイナーモード - デフォルト行文字数の位置にインジケータを表示 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-30T15:23:31+09:00>

;;; Commentary:

;; see also:
;; `fill-column'

;;; Code:

(if (not (package-installed-p 'fill-column-indicator))
    (package-install 'fill-column-indicator))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; FIXME: なぜか赤色になる
 ;; '(fci-rule-color (face-attribute 'font-lock-comment-face :foreground))
 '(fci-rule-color (face-attribute 'default :foreground))
 '(fci-rule-use-dashes t)
 '(fci-dash-pattern 0.5)
 ;; HACK: `fci-mode' を有効にした後、
 ;;       `toggle-truncate-lines' で折り返し表示を有効にすると
 ;;       `line-move-visual' が強制的に `nil' になる問題を回避
 '(fci-handle-line-move-visual nil))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'fci-mode)
    (global-set-key (kbd "C-c q") #'fci-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-fill-column-indicator.el ends here
