;;; 10-minor-hippie.el --- 設定 - マイナーモード - 拡張補完・展開

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T13:55:58+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'hippie-expand)
    (global-set-key (kbd "M-/") #'hippie-expand))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-hippie.el ends here
