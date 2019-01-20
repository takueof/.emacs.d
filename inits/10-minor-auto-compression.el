;;; 10-minor-auto-compression.el --- 設定 - マイナーモード - アーカイブファイルを直接編集 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:12:25+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(when (require 'jka-cmpr-hook nil :noerror) ; 未 `autoload'
  (auto-compression-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-auto-compression.el ends here
