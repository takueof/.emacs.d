;;; 00-05-im.el --- 設定 - インプットメソッド -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-30T22:40:05+09:00>

;;; Commentary:

;; WARNING: `00-02-coding.el' より後に設定すること
;;          `00-03-fonts.el' より後に設定すること
;;          `00-04-theme.el' より後に設定すること

;;; Code:


;; 各種シンボル（`current-input-method' など）が `mule-cmds' で定義されている
;; 例外を出さず、確実に初期化する
(eval-after-load "mule-cmds" ; 未 `provide'
  '(progn
     ;; -----------------------------------------------------------------------
     ;; インプットメソッド切替時に、フェイス `cursor' を変更
     ;; -----------------------------------------------------------------------
     (defface my-cursor-default nil
       "`cursor' face for `current-input-method' is nil."
       :group 'customize)
     (copy-face 'cursor 'my-cursor-default)

     (defface my-cursor-input-method-activated '((t
                                                  :background "red"))
       "`cursor' face for `current-input-method' is non-nil."
       :group 'customize)

     (defun my-change-cursor-faces-by-current-input-method ()
       "Change cursor color with `current-input-method'."
       (set-cursor-color
        (face-attribute (if current-input-method
                            'my-cursor-input-method-activated
                          'my-cursor-default)
                        :background)))

     (defun my-change-cursor-faces-by-current-input-method-advice (window &optional norecord)
       "Change cursor color with `current-input-method' for `advice-add'."
       (my-change-cursor-faces-by-current-input-method))


     ;; -----------------------------------------------------------------------
     ;; 有効化
     ;; -----------------------------------------------------------------------
     (add-hook 'input-method-activate-hook
               #'my-change-cursor-faces-by-current-input-method)
     (add-hook 'input-method-deactivate-hook
               #'my-change-cursor-faces-by-current-input-method)

     ;; ウインドウ選択後、input-method の状態に応じてフェイス `cursor' を変更
     ;;
     ;; `cursor' はフレーム単位
     ;; しかし、`current-input-method' はバッファローカル変数
     ;; よって、バッファ間で `current-input-method' 値が異なれば、
     ;; `cursor' が意図せぬ状態になる
     ;;
     ;; ゆえに、ウインドウ切替のタイミングでの `cursor' 明示変更が必要
     ;;
     ;; バッファ切替時は、特に何もしない
     ;;
     ;; `select-window' の実行後に起動するフックが存在しないため、
     ;; アドバイスでしのぐ
     (if (fboundp 'select-window)
         (advice-add 'select-window
                     :after
                     #'my-change-cursor-faces-by-current-input-method-advice))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-05-im.el ends here
