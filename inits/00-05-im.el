;;; 00-05-im.el --- 設定 - インプットメソッド

;; Copyright (C) 2013-2015 Taku Watabe
;; Time-stamp: <2015-02-19T22:28:01+09:00>

;;; Commentary:

;; WARNING: 文字セット・コーディングシステム設定よりも後で設定すること。
;;          フォント設定よりも後で設定すること。
;;          カラーテーマ設定よりも後で設定すること。

;;; Code:

;; 各種シンボル（`current-input-method' など）が `mule-cmds' で定義されている
;; 例外を出さず、確実に初期化する
(eval-after-load "mule-cmds" ; 未 `provide'
  '(progn
     ;; -----------------------------------------------------------------------
     ;; インプットメソッド切替時に、フェイス `cursor' を変更
     ;; -----------------------------------------------------------------------
     (defface my-cursor-default nil
       "`current-input-method' が non-nil の場合に用いる `cursor' フェイス。"
       :group 'customize)
     (copy-face 'cursor 'my-cursor-default)

     (defface my-cursor-input-method-activated '((t
                                                  :background "red"))
       "`current-input-method' が non-nil の場合に用いる `cursor' フェイス。"
       :group 'customize)

     (defun my-change-cursor-faces-by-current-input-method ()
       "`current-input-method' の評価値に応じ、カーソル色を変更する。"
       (set-cursor-color
        (face-attribute (if current-input-method
                            'my-cursor-input-method-activated
                          'my-cursor-default)
                        :background)))


     ;; -----------------------------------------------------------------------
     ;; 有効化
     ;; -----------------------------------------------------------------------
     (add-hook 'input-method-activate-hook
               #'my-change-cursor-faces-by-current-input-method)
     (add-hook 'input-method-deactivate-hook
               #'my-change-cursor-faces-by-current-input-method)

     (defadvice select-window (after
                               my-change-cursor-faces
                               activate)
       "ウインドウ選択後、input-method の状態に応じてフェイス `cursor' を変更する。

`cursor' はフレーム単位。
しかし、`current-input-method' はバッファローカル変数。
ゆえに、バッファ間で `current-input-method' 値が異なれば、`cursor' が意図せぬ状態になる。

ゆえに、ウインドウ切替のタイミングで、`cursor' を明示的に変更しなければならない。

バッファ切替時は、特に何もしない。
ウインドウ切替時と異なり、ユーザに対する明示的なカーソル表示が発生しないため。

`select-window' の実行後に起動するフックが存在しないため、`defadvice' でしのいでいる。"
       (my-change-cursor-faces-by-current-input-method))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-05-im.el ends here
