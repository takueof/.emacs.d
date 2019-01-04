;;; 10-minor-smartparens.el --- 設定 - マイナーモード - 各種カッコ関連機能拡張

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-03-26T14:50:04+09:00>

;;; Commentary:

;;; Code:

(package-install 'smartparens)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; insert custom
 '(sp-autoinsert-quote-if-followed-by-closing-pair t)
 ;; delete custom
 '(sp-undo-pairs-separately t)
 ;; show-smartparens-mode
 '(sp-show-pair-from-inside t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(when (require 'smartparens-config nil :noerror)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-smartparens.el ends here
