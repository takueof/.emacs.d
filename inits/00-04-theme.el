;;; 00-04-theme.el --- 設定 - カラーテーマ

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-19T22:27:18+09:00>

;;; Commentary:

;; WARNING: 文字セット・コーディングシステム設定よりも後で設定すること。
;;          フォント設定よりも後で設定すること。

;;; Code:

;; 利用可能なカラーテーマを設定
(let ((required-themes '(;; 利用したいカラーテーマの一覧
                         ;; 優先度が高い順に降順ソートしておくこと
                         my-default
                         wheatgrass
                         ))
      (availabled-themes (custom-available-themes)))
  ;; 利用したいカラーテーマが見つからなければ、何もしない
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
