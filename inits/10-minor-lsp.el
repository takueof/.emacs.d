;;; 10-minor-lsp.el --- 設定 - マイナーモード - LSP クライアント -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-30T22:41:42+09:00>

;;; Commentary:

;;; Code:

(if (not (package-installed-p 'lsp-mode))
    (package-install 'lsp-mode))
(if (not (package-installed-p 'lsp-ui))
    (package-install 'lsp-ui))
(if (not (package-installed-p 'company-lsp))
    (package-install 'company-lsp))


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(lsp-auto-guess-root t)
 '(lsp-restart 'ignore)
 '(lsp-prefer-flymake t)
 ;; ローカル環境にのみ保存
 '(lsp-session-file (convert-standard-filename "~/.emacs.lsp-session"))
 ;; With `lsp-ui'
 '(lsp-ui-flycheck-list-position 'right)
 ;; With `company-lsp'
 '(company-lsp-enable-recompletion t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(when (fboundp 'lsp)
  (dolist (mode-hook '(;; 対象モードの hook を列挙（降順ソート済）
                       css-mode-hook
                       html-mode-hook
                       js-mode-hook
                       js2-mode-hook
                       json-mode-hook
                       php-mode-hook
                       sass-mode-hook
                       scss-mode-hook
                       sh-mode-hook
                       vue-mode-hook))
    (add-hook mode-hook #'lsp))

  (if (and (require 'lsp nil :noerror)
           (boundp 'lsp-after-open-hook)
           (require 'lsp-ui-flycheck nil :noerror)
           (fboundp 'lsp-ui-flycheck-enable))
      (add-hook 'lsp-after-open-hook #'lsp-ui-flycheck-enable)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-lsp.el ends here
