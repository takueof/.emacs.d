;;; 10-minor-autorevert.el --- 設定 - マイナーモード - 自動バッファ再読込

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T13:47:51+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'global-auto-revert-mode)
    (global-auto-revert-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-autorevert.el ends here
