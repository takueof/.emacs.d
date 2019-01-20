;;; 10-minor-savehist.el --- 設定 - マイナーモード - ミニバッファの履歴を残す -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:15:25+09:00>

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
