;;; 10-minor-lsp-mode.el --- 設定 - マイナーモード - LSP クライアント

;; Copyright (C) 2018-2019 Taku Watabe
;; Time-stamp: <2019-01-12T13:35:42+09:00>

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
 '(lsp-restart 'ignore)
 ;; ローカル環境にのみ保存
 '(lsp-session-file (convert-standard-filename "~/.emacs.lsp-session"))
 ;; With `lsp-ui'
 '(lsp-ui-flycheck-list-position 'right)
 ;; With `company-lsp'
 '(company-lsp-enable-recompletion t))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'lsp)
    (add-hook 'prog-mode-hook #'lsp))

(eval-after-load 'lsp
  '(progn
     (require 'lsp-clients nil :noerror)

     (if (and (boundp 'lsp-after-open-hook)
              (require 'lsp-ui-flycheck nil :noerror)
              (fboundp 'lsp-ui-flycheck-enable))
         (add-hook 'lsp-after-open-hook #'lsp-ui-flycheck-enable))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-lsp-mode.el ends here
