;;; windows-10-minor-grep.el --- 設定 - Windows - マイナーモード - grep -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-19T13:21:45+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; 例外が出るため NUL デバイスは使わせない
 '(grep-use-null-device nil))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(when (require 'grep nil :noerror)
  ;; PATH は通っていないが、`exec-path' は通っている場合を想定
  ;;
  ;; すべて `defvar' 定義なので、 `autoload' 前後での `custom-set-variables'
  ;; による設定は不可能
  ;; 明示的ロード後～関数実行前までに設定しなければならない
  (setq grep-program
        (purecopy (or (executable-find "grep")
                      "grep")))
  (setq find-program
        (purecopy (or (executable-find "find")
                      "find")))
  (setq xargs-program
        (purecopy (or (executable-find "xargs")
                      "xargs"))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-10-minor-grep.el ends here
