;;; my-im.el --- 設定 - Input Method (IM) -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Taku Watabe
;; Time-stamp: <2022-08-14T00:00:37+09:00>

;; Author: Taku Watabe <taku.eof@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 独自定義した Input Method (IM) 設定の集合
;; `feature' 名 `my-im'
;;
;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; ============================================================================
;; Input Method (IM) 切替時に `cursor' の face を変更
;; ============================================================================
;; WARNING: `window-system' 外の環境（例：ターミナルエミュレータ）では例外発生
;;          `window-system' 外の環境は除外することで回避
;; ============================================================================
(when window-system
  ;; 各種シンボル（`current-input-method' など）が `mule-cmds' で定義されている
  ;; 例外を出さず、確実に初期化する
  (with-eval-after-load "mule-cmds" ; 未 `provide'
    (defface my-cursor-default nil
      "`cursor' face for `current-input-method' is nil."
      :group 'customize)
    (copy-face 'cursor 'my-cursor-default)

    (defface my-cursor-input-method-activated '((t
                                                 :background "gold"))
      "`cursor' face for `current-input-method' is non-nil."
      :group 'customize)

    (defun my-change-cursor-faces-by-current-input-method ()
      "Change cursor color with `current-input-method'."
      (let* ((current-input-method (if (fboundp #'mac-input-source)
                                       (let ((input-source (mac-input-source)))
                                         (if (numberp (string-match "\\.US\\'" input-source))
                                             nil
                                           input-source))
                                     current-input-method))
             (cursor-face (if current-input-method
                              'my-cursor-input-method-activated
                            'my-cursor-default)))
        (set-cursor-color (face-attribute cursor-face :background))))

    ;; ------------------------------------------------------------------------
    ;; 有効化
    ;; ------------------------------------------------------------------------
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
    ;; `select-window' 実行後に起動するフック `buffer-list-update-hook' を利用
    (add-hook 'buffer-list-update-hook
              #'my-change-cursor-faces-by-current-input-method)
    ;; input-method の activate/deactivate と連動させる
    (add-hook 'input-method-activate-hook
              #'my-change-cursor-faces-by-current-input-method)
    (add-hook 'input-method-deactivate-hook
              #'my-change-cursor-faces-by-current-input-method)
    (add-hook 'mac-selected-keyboard-input-source-change-hook ;; macOS ONLY
              #'my-change-cursor-faces-by-current-input-method)
    (add-hook 'mac-enabled-keyboard-input-sources-change-hook ;; macOS ONLY
              #'my-change-cursor-faces-by-current-input-method)))


(provide 'my-im)


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; my-im.el ends here
