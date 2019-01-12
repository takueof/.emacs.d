;;; 10-minor-exec-path-from-shell.el --- 設定 - マイナーモード - GNU/Linux, Unix, macOS 環境変数 $PATH 自動取得・設定 -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;;; Code:

(package-install 'exec-path-from-shell)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (member window-system '(mac ns x))
    (eval-after-load 'exec-path-from-shell
      '(if (fboundp 'exec-path-from-shell-initialize)
           (exec-path-from-shell-initialize))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-exec-path-from-shell.el ends here
