;;; 00-01-default.el --- 設定 - 共通 -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-23T19:13:21+09:00>

;;; Commentary:

;;; Code:


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
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
 ;; 読取専用バッファにおける `kill-line' 実行時、
 ;; エコーエリアに関連メッセージが表示されるようにする
 ;;
 '(kill-read-only-ok t)
 ;;
 ;; 同一（重複）文字列は `kill-ring' に保存しない
 ;;
 '(kill-do-not-save-duplicates t)
 ;;
 ;; `undo' 時に `redo' 履歴は無視する
 ;;
 '(undo-no-redo t)
 ;;
 ;; クリップボードと `kill-ring' を同期させる
 ;;
 '(x-select-enable-clipboard t) ; NOTE: obsolete sinse v25.1
 '(select-enable-clipboard t)
 ;;
 ;; スクロール時、自動スクロールをアグレッシブにする
 ;;
 ;; see also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Scrolling.html
 '(scroll-conservatively 0) ; default
 '(scroll-step 0) ; default
 '(scroll-up-aggressively nil) ; default
 '(scroll-down-aggressively nil) ; default
 ;;
 ;; なるべくウインドウ上下から10行目でスクロール開始
 ;;
 '(scroll-margin 10)
 '(maximum-scroll-margin 10)
 ;;
 ;; ページ単位スクロール時に行を重複させない
 ;;
 '(next-screen-context-lines 0)
 ;;
 ;; スクロール時、なるべく先頭ないし最後の文字にポイントを移動させる
 ;;
 '(scroll-error-top-bottom t)
 ;;
 ;; スクロール時、なるべくポイントを同一スクリーン位置に留まらせる
 ;;
 '(scroll-preserve-screen-position t)
 ;;
 ;; 行間調整はしない
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
 '(auto-save-list-file-prefix (convert-standard-filename "~/.emacs.auto-save-list/.saves-")) ; ローカル環境化
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
 '(completions-format 'vertical)
 ;;
 ;; エコーエリアの最大行数を増やす
 ;;
 '(message-log-max 2000))

;; YES/NO 選択を簡略化
(fset 'yes-or-no-p 'y-or-n-p)

;; リージョンの大文字・小文字変換で、実行の是非を問わせない
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Windows: ベル音はなし
(if (fboundp 'set-message-beep)
    (set-message-beep 'silent))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 00-01-default.el ends here
