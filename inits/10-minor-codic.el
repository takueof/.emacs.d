;;; 10-minor-codic.el --- 設定 - マイナーモード - プログラマ向けネーミング辞書 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-30T12:52:37+09:00>

;;; Commentary:

;; see also:
;; http://codic.jp/about.html

;;; Code:

(if (not (package-installed-p 'codic))
    (package-install 'codic))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'codic
  ;; 専用バッファを kill する関数が定義されていないなら、追加
  '(unless (fboundp 'codic-view-kill)
     (defun codic-quit ()
       "Quit `codic' window and bury its buffer."
       (interactive)
       (with-current-buffer (current-buffer)
         (quit-window t)))

     ;; 専用バッファでキーバインドを有効にするため、アドバイスを利用
     ;; 専用 hook がないため
     (defun codic-local-set-key (items)
       "Set `local-set-key' for `codic' result buffer."
       (with-current-buffer "*Codic Result*"
         (if (fboundp 'codic-quit)
             (local-set-key (kbd "q") #'codic-quit))))

     (if (fboundp 'codic--view)
         (advice-add 'codic--view
                     :after
                     #'codic-local-set-key))))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'codic)
    (global-set-key (kbd "C-c C-q") #'codic))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-codic.el ends here
