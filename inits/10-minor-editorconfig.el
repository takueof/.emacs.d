;;; 10-minor-editorconfig.el --- 設定 - マイナーモード - EditorConfig -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

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
