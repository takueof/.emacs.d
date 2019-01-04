;;; 00-01-default.el --- 設定 - 共通

;; Copyright (C) 2013-2018 Taku Watabe
;; Time-stamp: <2018-11-02T12:22:40+09:00>

;;; Commentary:

;;; Code:

(custom-set-variables
 ;;
 ;; フレームタイトルはカレントバッファ名を基準にする
 ;;
 '(frame-title-format (format "%%b - emacs %s" emacs-version))
 ;;
 ;; スタートアップ表示は一切させない
 ;;
 ;; see also:
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
 ;;
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message t)
 ;;
 ;; *scratch* バッファのデフォルトメッセージは表示しない
 ;;
 '(initial-scratch-message nil)
 ;;
 ;; ベルは視覚のみ・音なし
 ;;
 '(visible-bell t)
 '(ring-bell-function 'ignore)
 ;;
 ;; タイプ時にマウスポインタを自動で隠す
 ;;
 '(make-pointer-invisible t)
 ;;
 ;; 空行をフリンジに表示
 ;;
 '(indicate-empty-lines t)
 ;;
 ;; ファイル先頭・末尾の状態表示をフリンジに表示
 ;;
 '(indicate-buffer-boundaries 'right)
 ;;
 ;; `kill-line' で改行も含めて削除
 ;;
 '(kill-whole-line t)
 ;;
 ;; `undo' 時に `redo' 履歴は無視する
 ;;
 '(undo-no-redo t)
 ;;
 ;; クリップボードと `kill-ring' を同期させる
 ;;
 '(x-select-enable-clipboard t)
 ;; 同一（重複）文字列は `kill-ring' に保存しない
 '(kill-do-not-save-duplicates t)
 ;;
 ;; ページ単位スクロール時に重複させる行数を少し増やす
 ;;
 '(next-screen-context-lines 3)
 ;;
 ;; カーソル移動によるスクロール時の挙動を、次の仕様に変更する:
 ;;
 ;;   * ウインドウ上下から10行目でスクロール開始
 ;;
 ;; see also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
 '(scroll-margin 10)
 ;;
 ;; 行間調整は行わない
 ;;
 '(line-spacing nil)
 ;;
 ;; 行間移動に論理行を用いる
 ;;
 '(line-move-visual t)
 ;;
 ;; デフォルトの行表示は折り返し「なし」
 ;;
 '(truncate-lines t)
 '(truncate-partial-width-windows t)
 '(default-truncate-lines t)
 ;;
 ;; デフォルトの行文字数を、端末エミュレータのデファクトスタンダードにあわせる
 ;;
 '(fill-column 80)
 ;;
 ;; デフォルトのインデント利用文字は、常に半角空白 (U+0020) のみ
 ;; 必要なら各メジャーモードごとに設定しなおす
 ;;
 '(indent-tabs-mode nil)
 ;;
 ;; 自分用デフォルトタブ文字表示幅
 ;; 必要なら各メジャーモードごとに設定しなおす
 ;;
 '(tab-width 4)
 ;;
 ;; 大文字・小文字は区別しない
 ;;
 '(case-fold-search t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 ;;
 ;; 新規ファイル・バッファ作成時の確認は省略
 ;;
 '(confirm-nonexistent-file-or-buffer nil)
 ;;
 ;; 最終行への改行（空行）挿入は、任意とする
 ;;
 ;; 他人の書いたファイルが最終行に改行のないファイルだった場合、
 ;; 保存すると改行が入って差分となり、いちいち指摘が入りうる問題を回避
 ;;
 '(require-final-newline nil)
 '(mode-require-final-newline nil)
 ;;
 ;; `undo' 上限を引き上げ
 ;;
 '(undo-limit 600000)
 '(undo-strong-limit 900000) ; (= 1.5 (/ undo-strong-limit undo-limit)) を踏襲
 ;;
 ;; 自動バックアップは不要
 ;;
 '(auto-save-default nil)
 '(make-backup-files nil)
 '(auto-save-list-file-prefix "~/.emacs.auto-save-list/.saves-") ; ローカル環境化
 ;;
 ;; `eval-expression' 時の出力を省略させない
 ;;
 '(eval-expression-print-level nil)
 '(eval-expression-print-length nil)
 ;;
 ;; 補完表示は循環させる
 ;;
 '(completion-cycle-threshold t)
 ;;
 ;; 補完表示は縦にする
 ;;
 '(completions-format 'vertical))

;; YES/NO 選択を簡略化
(fset 'yes-or-no-p 'y-or-n-p)

;; ベル音はなし
(if (fboundp 'set-message-beep)
    (set-message-beep 'silent))

;; リージョンの大文字・小文字変換で、実行の是非を問わせない
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; 暫定マークを使用
(if (fboundp 'transient-mark-mode)
    (transient-mark-mode +1))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-01-default.el ends here
