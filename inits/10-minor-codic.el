;;; 10-minor-codic.el --- 設定 - マイナーモード - プログラマ向けネーミング辞書 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;; see also:
;; http://codic.jp/about.html

;;; Code:

(package-install 'codic)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'codic
  '(progn
     ;; 専用バッファを kill する関数が定義されていないなら、追加
     (if (not (fboundp 'codic-view-kill))
         (defun codic-quit ()
           "Quit `codic' window and bury its buffer."
           (interactive)
           (with-current-buffer (current-buffer)
             (quit-window t))))

     ;; 専用バッファで各種キーバインドを有効にする
     (defadvice codic--view (after
                             codic-view-local-set-key
                             activate)
       (with-current-buffer "*Codic Result*"
         (local-set-key (kbd "q") #'codic-quit)))))


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
