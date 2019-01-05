;;; 20-major-php.el --- 設定 - メジャーモード - PHP

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-05T17:17:49+09:00>

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
      (php-eldoc-enable))

  ;; EditorConfig 対応
  (eval-after-load 'editorconfig
    '(if (boundp 'editorconfig-properties-hash)
         (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                (indent-style (equal indent-style-data "tab"))
                (insert-final-newline-data (gethash 'insert_final_newline editorconfig-properties-hash))
                (insert-final-newline (equal insert-final-newline-data "true")))
           (if (not (equal indent-tabs-mode indent-style))
               (setq-local indent-tabs-mode indent-style))
           (if (not (equal require-final-newline insert-final-newline))
               (setq-local require-final-newline insert-final-newline))))))

(add-hook 'php-mode-hook #'my-php-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-php.el ends here
