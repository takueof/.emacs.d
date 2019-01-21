;;; 10-minor-migemo.el --- 設定 - マイナーモード - ローマ字入力から日本語をインクリメンタル検索 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-21T10:23:17+09:00>

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
 ;; `migemo' 側で定義されている `isearch' 関連キーバインドを使わせない
 ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
 '(migemo-use-default-isearch-keybinding nil)
 ;; キャッシュを使わせる
 '(migemo-use-pattern-alist t)
 '(migemo-use-frequent-pattern-alist t)
 '(migemo-pattern-alist-length 1024)
 ;; キャッシュはユーザ別基礎ディレクトリにまとめる
 '(migemo-pattern-alist-file (expand-file-name ".migemo-patterns" migemo-directory))
 '(migemo-frequent-pattern-alist-file (expand-file-name ".migemo-frequent-patterns" migemo-directory))
 ;; 辞書エンコーディングを明示
 '(migemo-coding-system 'utf-8-unix))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (and (require 'migemo nil :noerror)
         (boundp 'isearch-mode-map)
         (fboundp 'migemo-isearch-toggle-migemo))
    ;; 関数 `migemo-init' の実行は、各環境ごとの設定ファイル内で行わせる
    ;; 各環境ごとの設定ファイルでも `custom-set-variables' を実行しているため
    ;;
    ;; ここではキーバインドのみ設定
    (define-key isearch-mode-map (kbd "C-c C-s") #'migemo-isearch-toggle-migemo))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-migemo.el ends here
