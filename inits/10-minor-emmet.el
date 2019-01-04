;;; 10-minor-emmet.el --- 設定 - マイナーモード - Emmet サポート

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-12T13:54:15+09:00>

;;; Commentary:

;; see also:
;; http://docs.emmet.io/

;;; Code:

(package-install 'emmet-mode)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(emmet-indentation 2)
 '(emmet-move-cursor-between-quotes t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-emmet.el ends here
