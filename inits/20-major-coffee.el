;;; 20-major-coffee.el --- 設定 - メジャーモード - CoffeeScript

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:56:47+09:00>

;;; Commentary:

;;; Code:

(package-install 'coffee-mode)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'coffee-mode
  '(progn
     (if (boundp 'coffee-mode-map)
         ;; バッティング解除
         (define-key coffee-mode-map (kbd "C-c C-l") nil))))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-coffee-mode-initialize ()
  "Initialize `coffee-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil))

(add-hook 'coffee-mode-hook #'my-coffee-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-coffee.el ends here
