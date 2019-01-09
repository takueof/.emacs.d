;;; 10-minor-tramp.el --- 設定 - マイナーモード - TRAMP (Transparent Remote Access, Multiple Protocols)

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-09T15:29:54+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'tramp
  '(custom-set-variables
    '(tramp-persistency-file-name (convert-standard-filename "~/.emacs.tramp"))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-tramp.el ends here
