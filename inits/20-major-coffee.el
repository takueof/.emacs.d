;;; 20-major-coffee.el --- 設定 - メジャーモード - CoffeeScript

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-05T19:09:13+09:00>

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
  (setq-local require-final-newline nil)

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

(add-hook 'coffee-mode-hook #'my-coffee-mode-initialize)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-coffee.el ends here
