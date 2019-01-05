;;; 20-major-js2.el --- 設定 - メジャーモード - JavaScript

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-05T17:27:10+09:00>

;;; Commentary:

;;; Code:

(package-install 'js2-mode)
(package-install 'js2-refactor)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(js2-highlight-level 3) ; すべての構文強調を有効化
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-pretty-multiline-declarations t)
 '(js2-indent-switch-body nil) ; case 文はインデントしない
 '(js2-idle-timer-delay 0.2)
 '(js2-dynamic-idle-timer-adjust 0)
 '(js2-concat-multiline-strings t)
 ;; 文法チェック関連
 ;; 他ツールに任せるため、すべて無効化
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(js2-missing-semi-one-line-override nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(js2-strict-var-hides-function-arg-warning nil)
 ;; その他
 '(js2-skip-preprocessor-directives t)
 '(js2-language-version 200)
 '(js2-instanceof-has-side-effects nil)
 '(js2-move-point-on-right-click nil) ; 使わない
 '(js2-allow-rhino-new-expr-initializer nil) ; 使わない
 '(js2-allow-member-expr-as-function-name nil)
 '(js2-include-browser-externs t)
 '(js2-include-rhino-externs nil)
 '(js2-include-node-externs nil)
 '(js2-mode-indent-inhibit-undo nil)
 '(js2-mode-indent-ignore-first-tab nil)
 '(js2-highlight-external-variables t)
 ;; JSLint
 '(js2-include-jslint-globals nil)) ; 他ツールに任せるため無効化


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-js2-mode-initialize ()
  "Initialize `js2-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil)

  (require 'js2-refactor nil :noerror)

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

(add-hook 'js2-mode-hook #'my-js2-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(when (fboundp 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.pac\\'" . js2-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-js2.el ends here
