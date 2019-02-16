;;; my-keybinds.el --- 設定 - グローバルキーバインド -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-02-17T06:04:45+09:00>

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

;; 独自定義したグローバルキーバインド設定の集合
;; `feature' 名 `my-keybinds'
;;
;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; Backspace と DEL を 交換
(keyboard-translate ?\C-h ?\C-?)

;; DEL を C-d にする
(keyboard-translate ?\C-? ?\C-d)

;; `ido-undo-merge-work-directory' 実行のため C-z を押しすぎた場合、
;; `suspend-frame' が起動しないよう配慮
(global-unset-key (kbd "C-z"))

;; ヘルプ表示を割り当てなおす
(if (fboundp 'help-command)
    (global-set-key (kbd "C-x ?") #'help-command))

;; ウインドウ中央表示はもっともシンプルなものを用いる
;; `recenter-top-bottom' は使わない
(if (fboundp 'recenter)
    (global-set-key (kbd "C-l") #'recenter))

;; リージョン範囲をソート
(if (fboundp 'sort-lines)
    (global-set-key (kbd "C-c C-c C-s") #'sort-lines))

;; 1つ前のエラーを表示
(if (fboundp 'previous-error)
    (global-set-key (kbd "C-x \\") #'previous-error))


;; ============================================================================
;; 独自定義
;; ============================================================================
(eval-after-load 'my-utils
  '(progn
     ;; 行頭移動は物理行
     (global-set-key (kbd "C-a") #'my-beginning-of-smart-indented-line)

     ;; 前のウインドウに移動
     (global-set-key (kbd "C-x p") #'my-other-window-reverse)

     ;; 前のフレームに移動
     (global-set-key (kbd "C-x 5 p") #'my-other-frame-reverse)

     ;; 折り返し表示を強制切替
     (global-set-key (kbd "C-x w") #'my-toggle-truncate-lines-force)

     ;; カーソル位置にファイル名を挿入
     (global-set-key (kbd "C-c i f") #'my-insert-file-name)

     ;; カーソル位置にファイルパスを挿入
     (global-set-key (kbd "C-c i p") #'my-insert-file-path)

     ;; 一括エンコーディング変換
     (global-set-key (kbd "C-c RET f") #'my-change-files-coding-system)))


(provide 'my-keybinds)


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; my-keybinds.el ends here
