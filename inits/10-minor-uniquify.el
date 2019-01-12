;;; 10-minor-uniquify.el --- 設定 - マイナーモード - ファイル名を元に、より唯一性の高いバッファ名を生成 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(uniquify-buffer-name-style 'forward)
 '(uniquify-ignore-buffers-re "^*[^*]+*\\-"))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(require 'uniquify nil :noerror) ; 未 `autoload'


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-uniquify.el ends here
