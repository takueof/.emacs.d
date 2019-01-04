;;; 10-minor-editorconfig.el --- 設定 - マイナーモード - EditorConfig

;; Copyright (C) 2018 Taku Watabe
;; Time-stamp: <2018-11-22T19:57:50+09:00>

;;; Commentary:

;;; Code:

(package-install 'editorconfig)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'editorconfig-mode)
    (editorconfig-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-editorconfig.el ends here
