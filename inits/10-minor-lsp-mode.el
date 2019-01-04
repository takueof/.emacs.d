;;; 10-minor-lsp-mode.el --- 設定 - マイナーモード - LSP クライアント

;; Copyright (C) 2018 Taku Watabe
;; Time-stamp: <2018-12-17T18:19:33+09:00>

;;; Commentary:

;;; Code:

(package-install 'lsp-mode)
(package-install 'lsp-ui)
(package-install 'company-lsp)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 ;; Defaults
 '(lsp-auto-guess-root t)
 '(lsp-restart nil)
 ;; With `lsp-ui-flycheck'
 '(lsp-ui-flycheck-list-position 'right)
 ;; With `company-mode'
 '(company-lsp-enable-recompletion t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'lsp)
    (add-hook 'prog-mode-hook #'lsp))

(eval-after-load 'lsp
  '(require 'lsp-clients nil :noerror))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-lsp-mode.el ends here
