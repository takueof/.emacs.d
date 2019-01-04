;;; 10-minor-savehist.el --- 設定 - マイナーモード - ミニバッファの履歴を残す

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-12T14:01:25+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(history-length t) ; 無限
 ;; ローカル環境にのみ保存
 '(savehist-file (convert-standard-filename "~/.emacs.savehist.el")))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'savehist-mode)
    (savehist-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-savehist.el ends here
