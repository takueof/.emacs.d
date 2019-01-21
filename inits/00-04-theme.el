;;; 00-04-theme.el --- 設定 - カラーテーマ -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-21T22:54:50+09:00>

;;; Commentary:

;; WARNING: `00-02-coding.el' より後に設定すること
;;          `00-03-fonts.el' より後に設定すること

;;; Code:


;; 利用可能なカラーテーマを設定
(let ((required-themes '(;; 利用したいカラーテーマの一覧
                         ;; 優先度が高い順に降順ソートしておくこと
                         my-default
                         wheatgrass))
      (availabled-themes (custom-available-themes)))
  ;; 利用したいカラーテーマが見つからなければ何もしない
  (catch 'required-theme-found
    (dolist (theme required-themes)
      (when (member theme availabled-themes)
        (load-theme theme t)
        (throw 'required-theme-found theme)))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-04-theme.el ends here
