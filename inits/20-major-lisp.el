;;; 20-major-lisp.el --- 設定 - メジャーモード - 各種 Lisp 方言

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:14+09:00>

;;; Commentary:

;;; Code:

(package-install 'elisp-slime-nav)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(eval-after-load 'my-utils
  '(eval-after-load 'elisp-slime-nav
     '(my-change-lighter elisp-slime-nav-mode nil)))


;; ----------------------------------------------------------------------------
;; 初期化: `lisp-mode'
;; ----------------------------------------------------------------------------
(defun my-lisp-mode-initialize ()
  "Common initialize \"Lisp\" major mode before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 8)

  (smartparens-mode +1)
  (elisp-slime-nav-mode +1))

(add-hook 'lisp-mode-hook #'my-lisp-mode-initialize)
(add-hook 'emacs-lisp-mode-hook #'my-lisp-mode-initialize)
(add-hook 'lisp-interaction-mode-hook #'my-lisp-mode-initialize)
(add-hook 'ielm-mode-hook #'my-lisp-mode-initialize)


;; ----------------------------------------------------------------------------
;; 初期化: `lisp-interaction-mode'
;; ----------------------------------------------------------------------------
(defun my-lisp-interaction-mode-initialize ()
  "Initialize `lisp-interaction-mode-initialize' before file load."
  ;; `whitespace-mode' 無効化
  (eval-after-load 'whitespace
    '(progn
       (if (fboundp 'whitespace-mode)
           (whitespace-mode -1))
       (when (boundp 'whitespace-style)
         (setq-local whitespace-style (copy-tree whitespace-style))
         (delete 'empty whitespace-style)))))

(add-hook 'lisp-interaction-mode-hook #'my-lisp-interaction-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-lisp.el ends here
