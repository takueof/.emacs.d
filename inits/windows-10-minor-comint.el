;;; windows-10-minor-comint.el --- 設定 - Windows - 共通コマンドインタプリタ -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:23+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'comint
  '(progn
     (custom-set-variables
      '(comint-scroll-to-bottom-on-input 'all)
      '(comint-move-point-for-output 'all)
      '(comint-buffer-maximum-size 5000)
      '(comint-process-echoes t)
      '(comint-eol-on-send t))))

;; ------------------------------------
;; プロセスごとのコーディングシステム変換表
;;
;; see also:
;; http://www.emacswiki.org/emacs/ShellMode#toc1
;; ------------------------------------
(add-to-list 'process-coding-system-alist
             '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix)))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-comint-mode-initialize ()
  "Initialize `comint-mode' before file load."
  (if (boundp 'comint-input-sender-no-newline)
      (setq-local comint-input-sender-no-newline t)))

(add-hook 'comint-mode-hook #'my-comint-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-10-minor-comint.el ends here
