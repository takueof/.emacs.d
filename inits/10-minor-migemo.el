;;; 10-minor-migemo.el --- 設定 - マイナーモード - ローマ字入力から日本語をインクリメンタル検索

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-06T10:12:32+09:00>

;;; Commentary:

;;; Code:

(package-install 'migemo)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 空白文字と認識させる対象を広げる
 '(migemo-white-space-regexp "[[:space:]\s-]*")
 ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
 '(migemo-directory (locate-user-emacs-file "migemo"))
 ;; キャッシュを使わせる
 '(migemo-use-pattern-alist t)
 '(migemo-pattern-alist-length 1024)
 '(migemo-use-frequent-pattern-alist t)
 ;; キャッシュはユーザ別基礎ディレクトリにまとめる
 '(migemo-pattern-alist-file
   (expand-file-name ".migemo-patterns" migemo-directory))
 '(migemo-frequent-pattern-alist-file
   (expand-file-name ".migemo-frequent-patterns" migemo-directory))
 ;; 辞書エンコーディングを明示
 '(migemo-coding-system 'utf-8-unix))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (and (require 'migemo nil :noerror)
         (fboundp 'migemo-init))
    (migemo-init))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-migemo.el ends here
