;;; 20-major-json.el --- 設定 - メジャーモード - JSON

;; Copyright (C) 2013-2018 Taku Watabe
;; Time-stamp: <2018-06-01T10:35:12+09:00>

;;; Commentary:

;;; Code:

(package-install 'json-mode)


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-json-mode-initialize ()
  "Initialize `json-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local js-indent-level 2)
  (setq-local tab-width 2))

(add-hook 'json-mode-hook #'my-json-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.bowerrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.ftppass\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.htmlhintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.htmllintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jscsrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.stylelintrc\\'" . json-mode))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-json.el ends here
