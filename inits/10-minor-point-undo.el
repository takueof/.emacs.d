;;; 10-minor-point-undo.el --- 設定 - マイナーモード - カーソルの移動履歴

;; Copyright (C) 2017 Taku Watabe
;; Time-stamp: <2017-09-25T17:39:05+09:00>

;;; Commentary:

;;; Code:

(package-install 'point-undo)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; `require' 必須（各関数が `autoload' ではないため）
(when (and (require 'point-undo nil :noerror)
           (fboundp 'point-undo)
           (fboundp 'point-redo))
  (global-set-key (kbd "M-]") #'point-undo)
  (global-set-key (kbd "M-[") #'point-redo))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-point-undo.el ends here
