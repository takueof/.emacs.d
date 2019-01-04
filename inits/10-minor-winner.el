;;; 10-minor-winner.el --- 設定 - マイナーモード - ウインドウの状態履歴を undo/redo

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T14:07:54+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'winner-mode)
    (winner-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-winner.el ends here
