;;; 20-major-eww.el --- 設定 - メジャーモード - EWW (Emacs Web Wowser) -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-27T18:00:58+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(eww-search-prefix "https://www.google.co.jp/search?&q=")
 '(eww-restore-desktop t)
 '(eww-history-limit 100))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-eww-initialize ()
  "Initialize `eww' after rendered."
  ;; EMPTY
  )

(add-hook 'eww-after-render-hook #'my-eww-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(global-set-key (kbd "C-c C-e") #'eww)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-eww.el ends here
