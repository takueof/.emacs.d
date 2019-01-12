;;; 10-minor-bookmark+.el --- 設定 - マイナーモード - `bookmark' 拡張 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;;; Code:

(package-install 'bookmark+)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(bookmark-version-control t)
 ;; ローカル環境にのみ保存
 '(bookmark-default-file (convert-standard-filename "~/.emacs.bookmark.el")))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(require 'bookmark+ nil :noerror)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-bookmark+.el ends here
