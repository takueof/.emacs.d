;;; 10-minor-undo-tree.el --- 設定 - マイナーモード - `undo' 拡張、`redo' 機能追加ならびに分岐履歴実装 -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

(package-install 'undo-tree)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(undo-tree-mode-lighter ""))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; `require' 必須（既存関数 `undo' を `undo-tree-undo' に置換しているため）
(when (and (require 'undo-tree nil :noerror)
           (fboundp 'undo-tree-redo)
           (fboundp 'global-undo-tree-mode))
  ;; `redo' 追加
  (global-set-key (kbd "C-.") #'undo-tree-redo)
  ;; 起動
  (global-undo-tree-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-undo-tree.el ends here
