;;; 10-minor-undo-tree.el --- 設定 - マイナーモード - `undo' 拡張、`redo' 機能追加ならびに分岐履歴実装

;; Copyright (C) 2017 Taku Watabe
;; Time-stamp: <2017-09-25T17:14:01+09:00>

;;; Commentary:

;;; Code:

(package-install 'undo-tree)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(undo-tree-mode-lighter " UT"))


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
