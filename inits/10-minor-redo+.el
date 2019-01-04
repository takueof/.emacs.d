;;; 10-minor-redo+.el --- 設定 - マイナーモード - `redo' 実装および `undo' 拡張

;; Copyright (C) 2013-2017 Taku Watabe
;; Time-stamp: <2017-09-26T15:03:23+09:00>

;;; Commentary:

;; undo-tree を利用するため停止中。

;; see also:
;; `10-minor-undo-tree.el'

;;; Code:

;; (package-install 'redo+)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; `require' 必須（既存関数 `undo' を置換しているため）
(if (and (require 'redo+ nil :noerror)
         (fboundp 'redo))
    (global-set-key (kbd "C-.") #'redo))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-redo+.el ends here
