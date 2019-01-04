;;; 10-minor-frame.el --- 設定 - マイナーモード - フレーム

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T13:55:22+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(eval-after-load 'frame
  '(progn
     ;; カーソルは点滅させない
     (blink-cursor-mode -1)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-frame.el ends here
