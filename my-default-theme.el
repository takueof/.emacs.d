;;; my-default-theme.el --- My original defaut themes -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-21T22:59:57+09:00>

;; Author: Taku Watabe <taku.eof@gmail.com>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 自分用 GNU Emacs テーマ（黒背景）
;; 作成方針は次の通り：
;;
;;   * 基本フェイスのみ変更する
;;   * 各メジャー＆マイナーモードの独自フェイスは「なるべく」変更しない
;;       * ただし (background dark) 用の定義がない場合を除く

;;; Code:


(deftheme my-default
  "My original default faces based on \"black\" (#000000) background.")

(custom-theme-set-faces
 'my-default

 ;; ---------------------------------
 ;; C-x C-e テスト用:
 ;; 利用可能カラー一覧: (list-colors-display)
 ;; ---------------------------------
 ;; (tooltip-show "aaああ")
 ;; (face-attribute 'cursor :inverse-video)
 ;; (face-all-attributes 'cursor)


 ;; ---------------------------------
 ;; 基本 (by `faces')
 ;; ---------------------------------
 '(default
    ((((class color))
      (:background "black" :foreground "white"))))
 '(cursor
   ((((class color))
     (:background "white"))))
 '(error
   ((((class color))
     (:foreground "red"))))
 '(warning
   ((((class color))
     (:foreground "orange"))))
 '(success
   ((((class color))
     (:foreground "green"))))
 ;;
 ;; フォント
 ;;
 '(fixed-pitch
   ((t
     nil))) ; :font-family 除去
 '(variable-pitch
   ((t
     nil))) ; :font-family 除去
 ;;
 ;; ハイライト
 ;;
 '(highlight
   ((((class color))
     (:foreground "white" :background "dark green"))))
 '(region
   ((((class color))
     (:foreground "white" :background "dark green"))))
 '(secondary-selection
   ((((class color))
     (:background "dark slate gray"))))
 ;;
 ;; モードライン
 ;;
 ;; TODO: まだ未定義
 ;;
 ;; mode-line
 ;; mode-line-inactive
 ;; mode-line-highlight


 ;; ---------------------------------
 ;; 基本フォントロック (by `font-lock')
 ;; ---------------------------------
 '(font-lock-comment-face
   ((((class color))
     (:foreground "SpringGreen3"))))
 '(font-lock-comment-delimiter-face
   ((((class color))
     ;; ほんの少し目立たせる
     (:foreground "SpringGreen1" :bold t))))
 '(font-lock-string-face
   ((((class color))
     (:foreground "dark khaki"))))
 '(font-lock-doc-face
   ((((class color))
     (:foreground "light salmon"))))
 '(font-lock-keyword-face
   ((((class color))
     (:foreground "cyan"))))
 '(font-lock-builtin-face ; 調整の余地あり
   ((((class color))
     (:foreground "LightSteelBlue"))))
 '(font-lock-function-name-face ; 調整の余地あり
   ((((class color))
     (:foreground "pale green" :bold t))))
 '(font-lock-variable-name-face ; 調整の余地あり
   ((((class color))
     (:foreground "pale green"))))
 '(font-lock-type-face ; 調整の余地あり
   ((((class color))
     (:foreground "aquamarine"))))
 '(font-lock-constant-face
   ((((class color))
     (:foreground "violet"))))
 '(font-lock-warning-face
   ((((class color))
     (:foreground "orange" :bold t))))
 '(font-lock-negation-char-face ; 調整の余地あり
   ((((class color))
     (:foreground "orange" :bold t))))
 '(font-lock-preprocessor-face ; 調整の余地あり
   ((((class color))
     (:foreground "cornflower blue" :bold t))))


 ;; ---------------------------------
 ;; 非カレントウインドウ (by `auto-dim-other-buffers')
 ;; ---------------------------------
 '(auto-dim-other-buffers-face
   ((((class color))
     (:background "gray20"))))


 ;; ---------------------------------
 ;; Flycheck モードライン (by `flycheck-color-mode-line')
 ;; ---------------------------------
 '(flycheck-color-mode-line-error-face
   ((((class color))
     :foreground "white" :background "red4")))
 '(flycheck-color-mode-line-warning-face
   ((((class color))
     :foreground "white" :background "orange4")))
 '(flycheck-color-mode-line-info-face
   ((((class color))
     :foreground "white" :background "RoyalBlue4")))
 '(flycheck-color-mode-line-success-face
   ((t))) ; モードラインデフォルトの face を利用
 '(flycheck-color-mode-line-running-face
   ((((class color))
     :foreground "white" :background "green4")))


 ;; ---------------------------------
 ;; ツールチップ (by `tooltip')
 ;; ---------------------------------
 '(tooltip
   ((((class color))
     ;; 配色は各ウインドウシステムのデフォルトに合わせる
     ;; フォントは別途定義したいので、あえて未指定のまま
     (:background "SystemInfoWindow" :foreground "SystemInfoText"))))


 ;; ---------------------------------
 ;; `anzu'
 ;; ---------------------------------
 '(anzu-mode-line
   ((((class color))
     (:inherit 'error))))


 ;; ---------------------------------
 ;; ツールチップ (by `company')
 ;; ---------------------------------
 ;;
 ;; TODO: まだ未定義
 ;;
 ;; company-tooltip
 ;; company-tooltip-selection
 ;; company-tooltip-search
 ;; company-tooltip-mouse
 ;; company-tooltip-common
 ;; company-tooltip-common-selection
 ;; company-tooltip-annotation
 ;; company-scrollbar-fg
 ;; company-scrollbar-bg
 ;; company-preview
 ;; company-preview-common
 ;; company-preview-search
 ;; company-echo
 ;; company-echo-common


 ;; ---------------------------------
 ;; ボタン (by `button')
 ;; ---------------------------------
 '(button
   ((t
     ;; link フェイスを継承させない
     nil))))


(provide-theme 'my-default)


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; my-default-theme.el ends here
