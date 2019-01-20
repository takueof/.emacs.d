;;; windows-10-minor-migemo.el --- 設定 - Windows - マイナーモード - ローマ字入力から日本語をインクリメンタル検索 -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-19T22:04:54+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(let ((cmd (executable-find "cmigemo"))
      (dict (convert-standard-filename "C:/programs/cmigemo/share/migemo/utf-8/migemo-dict")))
  (if (and (file-exists-p cmd)
           (file-exists-p dict))
      (custom-set-variables
       ;; コマンド設定
       `(migemo-command ,cmd)
       '(migemo-options '("-q" "--emacs"))
       ;; 辞書ファイルはデフォルトのものを利用
       `(migemo-dictionary ,dict)
       '(migemo-user-dictionary nil)
       '(migemo-regex-dictionary nil))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-10-minor-migemo.el ends here
