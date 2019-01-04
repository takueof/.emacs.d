;;; 20-major-js3.el --- 設定 - メジャーモード - JavaScript

;; Copyright (C) 2014-2015 Taku Watabe
;; Time-stamp: <2015-02-18T17:57:08+09:00>

;;; Commentary:

;;; Code:

;; (package-install 'js3-mode)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
;; TODO: case のインデントが好みでない
;;       switch と case は同一インデント、case 内は1インデントがいい
(custom-set-variables
 ;; 構文内のムダな部分を省く
 ;; TODO: 効果を見極めてそれぞれ変更するかどうか決める
 '(js3-compact t)
 '(js3-compact-while t)
 '(js3-compact-for t)
 '(js3-compact-if t)
 '(js3-compact-infix t)
 '(js3-compact-expr t)
 '(js3-compact-list t)
 '(js3-compact-case t)
 ;; 行内文字数の上限
 '(js3-max-columns 80)
 ;; インデントは半角スペース (U+0020)
 '(js3-indent-tabs-mode nil)
 ;; TODO: なんだこれ？
 '(js3-continued-expr-mult 2)
 ;; var 宣言で特殊整形（インデント）を用いる
 '(js3-pretty-vars t)
 '(js3-pretty-lazy-vars t)
 ;; 保存前の不要空白チェック・削除は行わない
 ;; 運用フェイズにあるファイルの過剰 diff を防ぐため
 '(js3-cleanup-whitespace nil)
 ;; 右クリックによるポイント移動は使わない
 '(js3-move-point-on-right-click nil)
 ;; 開閉のある構文の補完を用いる
 '(js3-mirror-mode t)
 ;; 自動インデント有効
 '(js3-auto-indent-p t)
 ;; TODO: なんだこれ？
 '(js3-consistent-level-indent-inner-bracket t)
 ;; TODO: なんだこれ？
 '(js3-boring-indentation t)
 ;; 手動インデントは使わない
 '(js3-manual-indentation nil)
 ;; Enter キー単独でのインデントを許可
 '(js3-indent-on-enter-key t)
 ;; 改行時に自動インデント
 '(js3-enter-indents-newline t)
 ;; 行末・行頭ジャンプに用いる関数はそのままにする
 ;; `js3-mode' 独自関数は使わない
 '(js3-rebind-eol-bol-keys nil)
 ;; 自動パース間隔
 '(js3-idle-timer-delay 0.2)
 ;; ファイルサイズが巨大なら、自動パース間隔を延ばす
 ;; 計算式：
 ;; (* js3-idle-timer-delay
 ;;    (/ (point-max) js3-dynamic-idle-timer-adjust))
 '(js3-dynamic-idle-timer-adjust 5)
 ;; パース実行時に、パース状態・間隔をミニバッファヘ表示
 '(js3-mode-verbose-parse-p t)
 ;; 自動クォートエスケープ有効
 '(js3-mode-escape-quotes t)
 ;; コメント内での自動空白追加を使う
 '(js3-mode-squeeze-spaces t)
 ;; 文法チェックは他に任せる（すべて無効）
 '(js3-mode-show-parse-errors nil)
 '(js3-mode-show-strict-warnings nil)
 '(js3-strict-trailing-comma-warning nil)
 '(js3-strict-missing-semi-warning nil)
 '(js3-missing-semi-one-line-override nil)
 '(js3-strict-inconsistent-return-warning nil)
 '(js3-strict-cond-assign-warning nil)
 '(js3-strict-var-redeclaration-warning nil)
 '(js3-strict-var-hides-function-arg-warning nil)
 '(js3-skip-preprocessor-directives nil)
 ;; JavaScript 1.8 以上として認識させる
 '(js3-language-version 180)
 ;; プロパティ名キーワードリストを利用
 '(js3-allow-keywords-as-property-names t)
 ;; instanceof の扱いを特に変えたりはしない
 '(js3-instanceof-has-side-effects nil)
 ;; Rhino の JavaScript 1.7 向け new 構文は使わない
 '(js3-allow-rhino-new-expr-initializer nil)
 ;; ブラウザ用キーワードのみデフォルト有効
 '(js3-include-browser-externs t)
 '(js3-include-rhino-externs nil)
 '(js3-include-gears-externs nil)
 ;; 各種ハイライトはすべて有効
 '(js3-highlight-external-variables t)
 '(js3-auto-insert-catch-block t)
 ;; 通常インデント
 '(js3-indent-level 4)
 ;; 記号別インデント
 '(js3-expr-indent-offset 0)   ; 演算子
 '(js3-paren-indent-offset 0)  ; ()
 '(js3-square-indent-offset 0) ; []
 '(js3-curly-indent-offset 0)  ; {}
 ;; インデント時には再パース
 '(js3-reparse-on-indent t)
 ;; 先頭カンマを使う（node.js 風）
 '(js3-lazy-commas t)
 ;; 先頭セミコロンを使う
 '(js3-lazy-semicolons t)
 ;; 先頭演算子を使う
 '(js3-lazy-operators t)
 ;; 過去行のピリオドと位置あわせ
 ;; 通常インデントは優先させない
 '(js3-lazy-dots nil)
 '(js3-indent-dots t)
 ;; `next-error' キーバインドに `js3-next-error' をマッピング
 ;; つまりオーバーライドする
 '(js3-dont-rebind-backtick nil))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-js3-mode-initialize ()
  "Initialize `js3-mode' before file load."
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline nil))

(add-hook 'js3-mode-hook #'my-js3-mode-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
;; (when (fboundp 'js3-mode)
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
;;   (add-to-list 'auto-mode-alist '("\\.pac\\'" . js3-mode)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 20-major-js3.el ends here
