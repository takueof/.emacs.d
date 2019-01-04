;;; 20-major-php.el --- 設定 - メジャーモード - PHP

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:19+09:00>

;;; Commentary:

;;; Code:

(package-install 'php-mode)
(package-install 'php-eldoc)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-php-mode-initialize ()
  "Initialize `php-mode' major mode before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

  (if (fboundp 'php-eldoc-enable)
      (php-eldoc-enable)))

(add-hook 'php-mode-hook #'my-php-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-php.el ends here
