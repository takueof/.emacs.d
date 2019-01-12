;;; cocoa-emacs-10-minor-migemo.el --- 設定 - macOS (Cocoa) - マイナーモード - ローマ字入力から日本語をインクリメンタル検索 -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(let ((cmd "cmigemo")
      (dict (convert-standard-filename "/usr/local/share/migemo/utf-8/migemo-dict")))
  (if (and (executable-find cmd)
           (file-exists-p dict))
      (custom-set-variables
       ;; コマンド設定
       '(migemo-command cmd)
       '(migemo-options '("-q" "--emacs"))
       ;; 辞書ファイルはデフォルトのものを利用
       '(migemo-dictionary dict)
       '(migemo-user-dictionary nil)
       '(migemo-regex-dictionary nil))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; cocoa-emacs-10-minor-migemo.el ends here
