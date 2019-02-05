;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-02-05T19:21:49+09:00>

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This config file can use "GNU Emacs" ONLY,
;; not compatible with "XEmacs" and other emacsens.
;;
;; This file is VERY LONG.
;; So, I DARE USE file local variables in the FIRST LINE.

;;; Code:


;; ============================================================================
;; Use "Japanese" environment
;; ============================================================================
(set-language-environment "Japanese")


;; ============================================================================
;; コーディングシステム
;; ============================================================================
;; いくつかのデフォルトだけ決める（他は変えない）
;;
;; `prefer-coding-system' は絶対に使わないこと！
;; 例：(prefer-coding-system 'utf-8-unix)
;; システムごとに最適化された、自動設定のデフォルト定義を破壊するため
(set-coding-system-priority 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; macOS ONLY
(when (member system-type '(darwin))
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (setq-default default-process-coding-system '(utf-8 . utf-8)))

;; 「UTF-8（BOM 有）」のエイリアスを作成
;;
;; デフォルト名は長いため
;;
;; see also:
;; `mule-conf.el'
(define-coding-system-alias 'utf-8-bom 'utf-8-with-signature)

;; `cp932' を `shift_jis' として強制認識
;;
;; MIME を用いた自動エンコーディング判定を行うコード（`sgml-mode' など）でも
;; 例外が出ないようにする
(coding-system-put 'japanese-cp932
                   :mime-charset 'shift_jis)

;; `japanese-shift-jis' を Microsoft Code Page 932 相当にする
;;
;; GNU Emacs における Shift_JIS 実装 `japanese-shift-jis' は、
;; JIS X 0208 の附属書1にある定義を厳格に実装したもの
;; ゆえに、一部文字（例：「～」(U+FF5E)）が未定義であったりするなど、実用上の問題が発生
;;
;; そこで、タイムスタンプ時点で最も普及している Microsoft の Shift_JIS 実装
;;  `japanese-cp932' を、デフォルトの Shift_JIS として認識させる
;;
;; see also:
;; `japanese.el'
(define-coding-system-alias 'japanese-shift-jis 'japanese-cp932)
(define-coding-system-alias 'shift_jis 'japanese-cp932)
(define-coding-system-alias 'sjis 'japanese-cp932)

;; 「〜」(U+301C) → 「～」(U+FF5E) 自動変換
(coding-system-put 'japanese-cp932 ; Shift_JIS
                   :encode-translation-table (get 'japanese-ucs-jis-to-cp932-map 'translation-table))


;; 「～」(U+FF5E) → 「〜」(U+301C) 自動変換
(coding-system-put 'japanese-iso-8bit ; EUC-JP
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'iso-2022-jp ; JIS
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))


;; ============================================================================
;; ロードパス追加
;; ============================================================================
(add-to-list 'load-path (locate-user-emacs-file "utils"))
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))


;; ============================================================================
;; ユーティリティ
;; ============================================================================
(require 'my-utils nil :noerror)


;; ============================================================================
;; フォント
;; ============================================================================
(require 'my-fonts nil :noerror)


;; ============================================================================
;; モードライン
;; ============================================================================
(require 'my-modeline nil :noerror)


;; ============================================================================
;; グローバルキーバインド
;; ============================================================================
(require 'my-keybinds nil :noerror)


;; ============================================================================
;; ANSI エスケープシーケンス
;; ============================================================================
;; `comint-mode' および派生モードで、ANSI エスケープシーケンスの解釈を
;; 開始させる。
(if (fboundp 'ansi-color-for-comint-mode-on)
    (ansi-color-for-comint-mode-on))


;; ============================================================================
;; カラーテーマ
;; ============================================================================
;; 利用可能なカラーテーマを設定
(let ((required-themes '(;; 利用したいカラーテーマの一覧
                         ;; 優先度が高い順に降順ソートしておくこと
                         my-default
                         wheatgrass))
      (availabled-themes (custom-available-themes)))
  ;; 利用したいカラーテーマが見つからなければ何もしない
  (catch 'required-theme-found
    (dolist (theme required-themes)
      (when (member theme availabled-themes)
        (load-theme theme t)
        (throw 'required-theme-found theme)))))


;; ============================================================================
;; インプットメソッド
;; ============================================================================
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


;; ============================================================================
;; YES/NO 選択を簡略化
;; ============================================================================
(fset 'yes-or-no-p 'y-or-n-p)


;; ============================================================================
;; リージョンの大文字・小文字変換で、実行の是非を問わせない
;; ============================================================================
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ============================================================================
;; ベル音 (Windows ONLY)
;; ============================================================================
(if (fboundp 'set-message-beep)
    ;; なし
    (set-message-beep 'silent))


;; ============================================================================
;; デフォルト値
;; ============================================================================
(custom-set-variables
 ;;
 ;; `custom-set-variables' と `custom-set-faces' に `user-init-file' への
 ;; 追記を許さない
 ;; 自動保存は別ファイルに行わせる
 ;;
 '(custom-file (locate-user-emacs-file "custom.el"))
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
 ;; ページ単位スクロール時に行を重複させる
 ;;
 '(next-screen-context-lines 2)
 ;;
 ;; スクロール時、なるべく先頭ないし最後の文字にポイントを移動させる
 ;;
 '(scroll-error-top-bottom t)
 ;;
 ;; スクロール時、ポイントを同一スクリーン位置に留まらせなくてもよい
 ;; non-nil にするとスクロールが不安定になりがちなため、nil とする
 ;;
 '(scroll-preserve-screen-position nil)
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
 ;; タブは常にインデントのみ実施
 ;;
 '(tab-always-indent t)
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
 ;; 最終行への改行（空行）挿入を強制
 ;;
 ;; 不要なら各メジャーモードごとに設定させる
 ;;
 '(require-final-newline t)
 '(mode-require-final-newline t)
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
 `(auto-save-list-file-prefix ,(convert-standard-filename "~/.emacs.auto-save-list/.saves-")) ; ローカル環境化
 ;;
 ;; ロックファイルは生成させる
 ;;
 '(create-lockfiles t)
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
 '(message-log-max 2000)
 ;;
 ;; ミニバッファで各種コマンドを利用できるようにする
 ;;
 '(enable-recursive-minibuffers t)
 ;;
 ;; Trash（「ごみ箱」など）が使える場合はそちらへ廃棄
 ;;
 '(delete-by-moving-to-trash t)
 ;;
 ;; 人為的に italic/bold フォントを選択 (Windows ONLY)
 ;;
 '(w32-enable-synthesized-fonts t)
 ;;
 ;; インプットメソッドを明示 (Windows ONLY)
 ;;
 ;; 識別名 "W32-IME"' は、IME パッチが適用されていなければ使えない
 ;; 他環境ではデフォルトのままとする
 ;;
 `(default-input-method ,(cond ((member system-type '(ms-dos windows-nt))
                                "W32-IME")
                               (t
                                default-input-method)))
 ;;
 ;; バッファごとのインプットメソッド状態モードライン表示 (Windows ONLY)
 ;;
 '(w32-ime-mode-line-state-indicator "[Aa]")
 ;;
 ;; インプットメソッド状態モードライン表示の一覧 (Windows ONLY)
 ;;
 ;; 順に「IM OFF」「IM ON: 日本語入力」「IM ON: 英字入力」
 ;;
 '(w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[Aa]"))
 ;;
 ;; バッファ切替時にはインプットメソッド状態を引き継がない (Windows ONLY)
 ;;
 '(w32-ime-buffer-switch-p t)
 ;;
 ;; 右 <Alt> + 左 <Ctrl> で <AltGr> が発送されないようにする (Windows ONLY)
 ;; <AltGr> は独自のキーコードであり、<C-M-> であるとみなされない
 ;;
 ;; see also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Keyboard.html
 ;;
 '(w32-recognize-altgr nil)
 ;;
 ;; Web ブラウザ
 ;;
 `(browse-url-browser-function ',(cond ((equal window-system 'w32)
                                        'browse-url-default-windows-browser)
                                       ((equal window-system 'mac)
                                        'browse-url-default-macosx-browser)
                                       (t
                                        'browse-url-default-browser)))
 ;;
 ;; GnuTLS trustfiles 追加
 ;;
 `(gnutls-trustfiles ',(mapcar 'convert-standard-filename
                               (if (member system-type '(ms-dos windows-nt))
                                   '("C:/programs/cygwin/usr/ssl/certs/ca-bundle.crt")
                                 '("/usr/local/etc/libressl/cert.pem"
                                   "/usr/local/etc/openssl/cert.pem"
                                   "/private/etc/ssl/cert.pem"
                                   "/etc/ssl/cert.pem")))))


;; ============================================================================
;; インプットメソッド初期化 (Windows ONLY)
;; ============================================================================
(if (and (require 'w32-ime nil :noerror)
         (fboundp 'w32-ime-initialize))
    (w32-ime-initialize))


;; ============================================================================
;; 環境変数 (Windows ONLY)
;; ============================================================================
(if (member system-type '(ms-dos windows-nt))
    ;; 環境変数 %PATH% では不足している分の追加
    (let* ((program-files-dir-x86 (or (getenv "PROGRAMFILES\(X86\)")
                                      (getenv "PROGRAMFILES")
                                      "C:/programs"))
           (paths `(,(concat program-files-dir-x86 "/Aspell/bin")
                    "C:/programs/cmigemo/bin"
                    "C:/programs/cygwin/bin")))
      (dolist (path paths)
        (setq path (convert-standard-filename path))
        (if (and (file-exists-p path)
                 (file-directory-p path))
            (add-to-list 'exec-path path)))))


;; ============================================================================
;; パッケージマネージャ (by `package')
;; ============================================================================
;; `defcustom' によって定義されたリストヘシンボルを追加したいため、
;; あえて明示的にロード
(when (and (require 'package nil :noerror)
           (boundp 'package-archives)
           (fboundp 'package-initialize)
           (fboundp 'package-list-packages-no-fetch))
  ;; 確実に定義された後で追加
  (add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

  ;; あらゆるパッケージロードに先んじての記述が必須
  (package-initialize)

  ;; `list-packages' のような短縮版を用意
  (defalias 'list-packages-no-fetch 'package-list-packages-no-fetch))


;; ============================================================================
;; 詳細設定補助 (by `use-package')
;; ============================================================================
(eval-after-load 'package
  '(progn
     ;; `package' が必ず使える状況を前提とする
     (if (not (package-installed-p 'use-package))
         (package-install 'use-package))

     (require 'use-package nil :noerror)))


;; ============================================================================
;; 詳細設定（マイナーモード）
;; ============================================================================
(eval-after-load 'use-package
  '(progn
     ;; -----------------------------------------------------------------------
     ;; 各種検索・置換強化
     ;; -----------------------------------------------------------------------
     (use-package anzu
       ;; :disabled
       :ensure t
       :defer t
       :bind (("M-%" . anzu-query-replace)
              ("C-M-%" . anzu-query-replace-regexp))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(anzu-mode-lighter nil)
        '(anzu-minimum-input-length 3)
        '(anzu-search-threshold 1000)
        '(anzu-replace-to-string-separator " -> "))

       ;; -----------------------------
       ;; デフォルト値（`migemo' 利用可能時）
       ;; -----------------------------
       (eval-after-load 'migemo
         '(custom-set-variables
           '(anzu-use-migemo t)))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'global-anzu-mode)
           (global-anzu-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; アーカイブファイルを直接編集
     ;; -----------------------------------------------------------------------
     (use-package jka-cmpr-hook
       ;; :disabled
       :demand t
       :config
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'auto-compression-mode)
           (auto-compression-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; 他ウインドウ弱調化
     ;;
     ;; see also:
     ;; `my-default-theme.el'
     ;; -----------------------------------------------------------------------
     (use-package auto-dim-other-buffers
       ;; :disabled
       :ensure t
       :defer t
       :hook ((after-init . my-auto-dim-other-buffers-mode-initialize))
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (defun my-auto-dim-other-buffers-mode-initialize ()
         "Initialize `auto-dim-other-buffers-mode'."
         (if (fboundp 'auto-dim-other-buffers-mode)
             (auto-dim-other-buffers-mode +1)))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'auto-dim-other-buffers-mode)
              (my-change-lighter auto-dim-other-buffers-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; 自動バッファ再読込
     ;; -----------------------------------------------------------------------
     (use-package autorevert
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'global-auto-revert-mode)
           (global-auto-revert-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; ブックマーク
     ;; -----------------------------------------------------------------------
     (use-package bookmark
       ;; :disabled
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(bookmark-version-control t)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(bookmark-default-file ,(convert-standard-filename "~/.emacs.bookmark.el"))))


     ;; -----------------------------------------------------------------------
     ;; ブックマーク (`bookmark') 拡張
     ;; -----------------------------------------------------------------------
     (use-package bookmark+
       ;; :disabled
       :after (:all bookmark)
       :ensure t
       :demand t)


     ;; -----------------------------------------------------------------------
     ;; プログラマ向けネーミング辞書
     ;; -----------------------------------------------------------------------
     (use-package codic
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-c C-q" . codic))
       :config
       ;; -----------------------------
       ;; HACK: 専用バッファをコマンドで `quit-window' させる
       ;; -----------------------------
       (unless (fboundp 'codic-view-kill)
         ;; 専用ウインドウを `quit-window' する関数が
         ;; 定義されていないなら追加
         (defun my-codic-view-kill ()
           "Quit `codic' window and bury its buffer."
           (interactive)
           (with-current-buffer (current-buffer)
             (quit-window t)))

         ;; 専用バッファでキーバインドを有効にするため、アドバイスを利用
         ;; 専用 hook がないため
         (defun my-codic-local-set-key (items)
           "Set `local-set-key' for `codic' result buffer."
           (with-current-buffer "*Codic Result*"
             (if (fboundp 'my-codic-view-kill)
                 (local-set-key (kbd "q") #'my-codic-view-kill))))

         (if (fboundp 'codic--view)
             (advice-add 'codic--view
                         :after
                         #'my-codic-local-set-key))))


     ;; -----------------------------------------------------------------------
     ;; 共通コマンドインタプリタ
     ;; -----------------------------------------------------------------------
     (use-package comint
       ;; :disabled
       :if (member system-type '(ms-dos windows-nt))
       :defer t
       :hook ((comint-mode . my-comint-mode-initialize))
       :init
       (custom-set-variables
        '(comint-scroll-to-bottom-on-input 'all)
        '(comint-move-point-for-output 'all)
        '(comint-buffer-maximum-size 5000)
        '(comint-process-echoes t)
        '(comint-eol-on-send t))

       ;; -----------------------------
       ;; プロセスごとのコーディングシステム変換表
       ;;
       ;; see also:
       ;; https://www.emacswiki.org/emacs/ShellMode#toc1
       ;; -----------------------------
       (add-to-list 'process-coding-system-alist
                    '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix)))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-comint-mode-initialize ()
         "Initialize `comint-mode' before file load."
         (if (boundp 'comint-input-sender-no-newline)
             (setq-local comint-input-sender-no-newline t))))


     ;; -----------------------------------------------------------------------
     ;; 補完フレームワーク
     ;; -----------------------------------------------------------------------
     (use-package company
       ;; :disabled
       :ensure t
       :defer t
       :hook ((after-init . global-company-mode))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; `company'
        ;;
        '(company-tooltip-limit 20)
        '(company-tooltip-minimum 10)
        '(company-tooltip-offset-display 'lines)
        '(company-tooltip-align-annotations t)
        '(company-tooltip-flip-when-above t)
        '(company-transformers '(company-sort-by-occurrence))
        '(company-minimum-prefix-length 1)
        '(company-abort-manual-when-too-short t)
        '(company-idle-delay 0.25)
        '(company-selection-wrap-around t)
        '(company-lighter-base nil)
        ;;
        ;; `company-dabbrev'
        ;;
        '(company-dabbrev-other-buffers t)
        '(company-dabbrev-downcase nil)
        ;;
        ;; `company-dabbrev-code'
        ;;
        '(company-dabbrev-code-modes '(batch-file-mode
                                       csharp-mode
                                       css-mode
                                       erlang-mode
                                       haskell-mode
                                       jde-mode
                                       js-mode
                                       js2-mode
                                       js3-mode
                                       lua-mode
                                       prog-mode
                                       python-mode
                                       sass-mode
                                       scss-mode))
        '(company-dabbrev-code-other-buffers t)
        '(company-dabbrev-code-everywhere t)
        '(company-dabbrev-code-ignore-case t)))


     ;; -----------------------------------------------------------------------
     ;; 補完フレームワーク (`company') 拡張（補完候補のソート）
     ;; -----------------------------------------------------------------------
     (use-package company-statistics
       ;; :disabled
       :after (:all company)
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(company-statistics-size 500)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(company-statistics-file ,(convert-standard-filename "~/.emacs.company-statistics-cache.el")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'company-statistics-mode)
           (company-statistics-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; 補完フレームワーク (`company') 拡張（補完候補のポップアップドキュメント）
     ;; -----------------------------------------------------------------------
     (use-package company-quickhelp
       ;; :disabled
       :after (:all company)
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(company-quickhelp-delay 0.25))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'company-quickhelp-mode)
           (company-quickhelp-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; コンパイル
     ;; -----------------------------------------------------------------------
     (use-package compile
       ;; :disabled
       :defer t
       :bind (("C-c C-l" . compile))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(compilation-window-height 15)
        ;;
        ;; ビルドツール・タスクランナーに依存させない
        ;;
        '(compile-command (purecopy ""))
        '(compilation-scroll-output t)
        '(compilation-always-kill t)
        '(compilation-context-lines t))
       :config
       (use-package subr-x
         ;; :disabled
         :demand t
         :config
         ;; ---------------------------
         ;; HACK: ウインドウの状態を問わず、常にリサイズをかける
         ;; ---------------------------
         ;; オーバーライド
         (defun compilation-set-window-height (window)
           "Set the height of WINDOW according to `compilation-window-height'."
           (let ((height (buffer-local-value 'compilation-window-height
                                             (window-buffer window))))
             (and height
                  ;; `window-full-width-p' は用いない
                  ;;
                  ;; If window is alone in its frame, aside from a minibuffer,
                  ;; don't change its height.
                  (not (eq window (frame-root-window (window-frame window))))
                  ;; Stef said that doing the saves in this order is safer:
                  (save-excursion
                    (save-selected-window
                      (select-window window)
                      (enlarge-window (- height (window-height))))))))

         ;; ---------------------------
         ;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
         ;; ---------------------------
         (defun my-compilation-message (cur-buffer msg)
           "Show status messages when compile done in `compilation-mode'."
           (let ((msg-text (string-trim msg)) ; 改行文字が含まれうる問題を回避
                 (msg-title (buffer-name))
                 (msg-face 'compilation-mode-line-fail))
             (message "%s: %s"
                      msg-title
                      (propertize msg-text
                                  'face
                                  (if (string-equal "finished" msg-text)
                                      'compilation-mode-line-exit
                                    'compilation-mode-line-fail)))))

         (add-hook 'compilation-finish-functions #'my-compilation-message)


         ;; ---------------------------
         ;; HACK: コンパイル完了後、
         ;;       ステータスに異常がなければ自動でウインドウを閉じる
         ;; ---------------------------
         (defcustom my-compilation-auto-quit-window-enable-buffer-names '("*compilation*")
           "Created buffer names by `compile' command."
           :group 'compilation
           :type '(list (repeat string)))

         ;; `process-status' と `exit-status' の値も得たいので、
         ;; アドバイスを利用
         ;; `compilation-finish-functions' にフックした関数では `msg' しか
         ;; 参照できないため
         (defun my-compilation-auto-quit-window (process-status exit-status msg)
           "Run `quit-window' when `compile' successed."
           (if (and (member (buffer-name)
                            my-compilation-auto-quit-window-enable-buffer-names)
                    (or (and (equal process-status 'exit)
                             (zerop exit-status))
                        ;; 改行文字が含まれうる問題を回避
                        (string-equal "finished" (string-trim msg))))
               (quit-window nil (get-buffer-window))))

         (if (fboundp 'compilation-handle-exit)
             (advice-add 'compilation-handle-exit
                         :after
                         #'my-compilation-auto-quit-window))

         ;; ---------------------------
         ;; HACK: ANSI エスケープシーケンスが正しく解釈されない問題を回避
         ;; ---------------------------
         (defun my-ansi-color-apply-on-compilation ()
           "Recognize ASCII color escape sequences for `compilation-mode' buffer."
           (if (and (require 'ansi-color nil :noerror)
                    (fboundp 'ansi-color-apply-on-region))
               (let ((start-marker (make-marker))
                     (end-marker (process-mark (get-buffer-process (current-buffer)))))
                 (set-marker start-marker (point-min))
                 (ansi-color-apply-on-region start-marker end-marker))))

         (add-hook 'compilation-filter-hook #'my-ansi-color-apply-on-compilation)))


     ;; -----------------------------------------------------------------------
     ;; 矩形選択
     ;; -----------------------------------------------------------------------
     (use-package cua-base
       ;; :disabled
       :defer t
       :init
       (if (fboundp 'cua-selection-mode)
           ;;
           ;; 特殊キーバインド無効
           ;;
           (cua-selection-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; バッファ内マッチ補完
     ;; -----------------------------------------------------------------------
     (use-package dabbrev
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; 補完時に大小文字を区別しない
        ;;
        '(dabbrev-case-fold-search t)))


     ;; -----------------------------------------------------------------------
     ;; デスクトップ環境保存・復旧
     ;; -----------------------------------------------------------------------
     (use-package desktop
       ;; :disabled
       :defer t
       :bind (("C-c d c" . desktop-clear)
              ("C-c d C-s" . desktop-save)
              ("C-c d s" . desktop-save-in-desktop-dir)
              ("C-c d d" . desktop-remove)
              ("C-c d f" . desktop-change-dir)
              ("C-c d r" . desktop-revert))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(desktop-save 'ask-if-new)
        '(desktop-load-locked-desktop t)
        '(desktop-missing-file-warning nil)
        ;;
        ;; 必要最小限の情報のみ保存させる
        ;;
        '(desktop-locals-to-save '(case-fold-search
                                   case-replace
                                   desktop-locals-to-save
                                   fill-column
                                   truncate-lines))
        '(desktop-restore-frames t)
        '(desktop-restore-in-current-display t)
        '(desktop-restore-forces-onscreen t)
        '(desktop-restore-reuses-frames t)
        '(desktop-file-name-format 'absolute)
        '(desktop-restore-eager t)
        '(desktop-lazy-verbose t)
        '(desktop-lazy-idle-delay 5))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'desktop-save-mode)
           (desktop-save-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; ディレクトリブラウジング
     ;; -----------------------------------------------------------------------
     (use-package dired
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-dired-mode-initialize ()
         "Initialize `dired-mode'."
         (if (fboundp 'dired-hide-details-mode)
             ;; 常にすべての情報を表示（簡易モードにしない）
             (dired-hide-details-mode -1)))

       (add-hook 'dired-mode-hook #'my-dired-mode-initialize))


     ;; -----------------------------------------------------------------------
     ;; ディレクトリブラウジング (`dired') 拡張
     ;; -----------------------------------------------------------------------
     (use-package dired+
       ;; :disabled
       :after (:all dired)
       :ensure t
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(diredp-hide-details-initially-flag nil)
        '(diredp-hide-details-propagate-flag nil)))


     ;; -----------------------------------------------------------------------
     ;; EditorConfig
     ;; -----------------------------------------------------------------------
     (use-package editorconfig
       ;; :disabled
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(editorconfig-mode-lighter ""))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'editorconfig-mode)
           (editorconfig-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; GNU Emacs Lisp ドキュメント表示
     ;; -----------------------------------------------------------------------
     (use-package eldoc
       ;; :disabled
       :defer t
       :hook ((lisp-mode . eldoc-mode)
              (emacs-lisp-mode . eldoc-mode)
              (lisp-interaction-mode . eldoc-mode)
              (ielm-mode . eldoc-mode))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(eldoc-minor-mode-string nil)
        '(eldoc-idle-delay 0.2)
        '(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)))


     ;; -----------------------------------------------------------------------
     ;; GNU Emacs Lisp ドキュメント表示 (`eldoc') 拡張（<M-:> による `eval'）
     ;; -----------------------------------------------------------------------
     (use-package eldoc-eval
       ;; :disabled
       :after (:any eldoc)
       :ensure t
       :defer t
       :hook ((lisp-mode . my-eldoc-eval-initialize)
              (emacs-lisp-mode . my-eldoc-eval-initialize)
              (lisp-interaction-mode . my-eldoc-eval-initialize)
              (ielm-mode . my-eldoc-eval-initialize))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(eldoc-in-minibuffer-show-fn 'eldoc-show-in-mode-line)
        '(eldoc-show-in-mode-line-delay 0.25)
        '(eldoc-eval-preferred-function 'pp-eval-expression)
        '(eldoc-in-minibuffer-own-frame-p nil)
        '(eldoc-in-minibuffer-mode-lighter nil)
        '(eldoc-mode-line-stop-rolling-on-input t))

       ;; -----------------------------
       ;; 起動
       ;;------------------------------
       (defun my-eldoc-eval-initialize ()
         "Initialize `eldoc-eval' when Lisp languages major mode hooks."
         (eldoc-in-minibuffer-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; GNU Emacs Lisp 定義ジャンプ・バック・ドキュメント閲覧
     ;; -----------------------------------------------------------------------
     (use-package elisp-slime-nav
       ;; :disabled
       :after (:any emacs-lisp-mode
                    lisp-interaction-mode
                    lisp-mode
                    ielm)
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;;------------------------------
       (if (fboundp 'elisp-slime-nav-mode)
           (elisp-slime-nav-mode +1))
       :config
       ;; -----------------------------
       ;; lighter
       ;;------------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'elisp-slime-nav-mode)
              (my-change-lighter elisp-slime-nav-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; Emmet
     ;; -----------------------------------------------------------------------
     (use-package emmet-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((html-mode . emmet-mode)
              (php-mode . emmet-mode))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(emmet-indentation 0)
        '(emmet-indent-after-insert t)
        '(emmet-use-style-tag-and-attr-detection t)
        '(emmet-self-closing-tag-style "/")
        '(emmet-preview-default t)
        '(emmet-insert-flash-time 0.25)
        '(emmet-move-cursor-after-expanding t)
        '(emmet-move-cursor-between-quotes t)
        '(emmet-postwrap-goto-edit-point t)))


     ;; -----------------------------------------------------------------------
     ;; `text/enriched' フォーマットファイル
     ;; -----------------------------------------------------------------------
     (use-package enriched
       ;; :disabled
       :defer t
       :config
       ;; -----------------------------
       ;; PATCH: v25.3 未満に存在するセキュリティホールの Fix
       ;;
       ;; see also:
       ;; https://lists.gnu.org/archive/html/emacs-devel/2017-09/msg00211.html
       ;; -----------------------------
       (if (or (< emacs-major-version 25)
               (and (= emacs-major-version 25)
                    (< emacs-minor-version 3)))
           (defun enriched-decode-display-prop (start end &optional param)
             (list start end))))


     ;; -----------------------------------------------------------------------
     ;; カーソル下の数値を増減
     ;; -----------------------------------------------------------------------
     (use-package evil-numbers
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-2" . evil-numbers/dec-at-pt)
              ("C-1" . evil-numbers/inc-at-pt)))


     ;; -----------------------------------------------------------------------
     ;; GNU/Linux, UNIX, macOS 環境変数 $PATH 自動取得・設定
     ;; -----------------------------------------------------------------------
     (use-package exec-path-from-shell
       ;; :disabled
       :if (member window-system '(mac ns x))
       :ensure t
       :demand t
       :init
       (if (fboundp 'exec-path-from-shell-initialize)
           (exec-path-from-shell-initialize)))


     ;; -----------------------------------------------------------------------
     ;; デフォルト行文字数の位置にインジケータを表示
     ;;
     ;; see also:
     ;; `fill-column'
     ;; -----------------------------------------------------------------------
     (use-package fill-column-indicator
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-c q" . fci-mode))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;; FIXME: `font-lock-comment-face' を用いたい
        ;;        しかし、指定すると、なぜか "red" が用いられてしまう
        ;;        現状は `default' フェイスで回避中
        `(fci-rule-color ,(face-attribute 'default :foreground))
        '(fci-rule-use-dashes t)
        '(fci-dash-pattern 0.5)
        ;;
        ;; HACK: `fci-mode' を有効にした後、
        ;;       `toggle-truncate-lines' で折り返し表示を有効にすると
        ;;       `line-move-visual' が強制的に nil となる問題を回避
        ;;
        '(fci-handle-line-move-visual nil)))


     ;; -----------------------------------------------------------------------
     ;; `dired' における `find' コマンド実行 (Windows ONLY)
     ;;
     ;; see also:
     ;; `dired'
     ;; -----------------------------------------------------------------------
     (use-package find-dired
       ;; :disabled
       :if (member system-type '(ms-dos windows-nt))
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; PATH は通っていないが、`exec-path' は通っている場合を想定
        ;;
        `(find-ls-option (cons (format "-exec %s -ld {} %s"
                                       (executable-find "ls")
                                       find-exec-terminator)
                               "-ld"))))


     ;; -----------------------------------------------------------------------
     ;; 自動静的解析
     ;; -----------------------------------------------------------------------
     (use-package flycheck
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-c f" . flycheck-mode))
       :hook ((after-init . global-flycheck-mode))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(flycheck-checker-error-threshold nil)
        '(flycheck-display-errors-delay 0.5)
        '(flycheck-idle-change-delay 0.25)
        '(flycheck-mode-line "")
        '(flycheck-disabled-checkers '(javascript-jscs)))
       :config
       ;; -----------------------------
       ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
       ;;       生成されうる警告バッファの出現を抑制
       ;; -----------------------------
       (eval-after-load 'warnings
         '(if (boundp 'warning-suppress-log-types)
              (add-to-list 'warning-suppress-log-types
                           '(flycheck syntax-checker))))

       ;; -----------------------------
       ;; PATCH: ESLint 優先利用
       ;;        JSHint -> ESLint を ESLint -> JSHint 順に変更
       ;; -----------------------------
       (if (boundp 'flycheck-checkers)
           (let* ((target-and-other-checkers (member 'javascript-eslint
                                                     flycheck-checkers)))
             (delete 'javascript-jshint flycheck-checkers)
             (setcdr target-and-other-checkers
                     (cons 'javascript-jshint
                           (cdr-safe target-and-other-checkers)))))

       ;; -----------------------------
       ;; PATCH: v.Nu サポート
       ;; -----------------------------
       (unless (flycheck-registered-checker-p 'vnu)
         ;; FIXME: v.Nu の標準出力が UTF-8 なので、環境によっては文字化けする
         (flycheck-define-checker vnu
           "A (X)HTML syntax and style checker using v.NU.

See URL `https://github.com/validator/validator'."
           :command ("vnu" "--format" "gnu" "--verbose" source)
           :error-patterns
           ;; ファイル名はフルパスで入ってくるため、チェックしない
           ((error line-start (minimal-match (one-or-more not-newline)) ":"
                   line "." column "-"
                   ;; `flycheck' は範囲指定（複数の line, column 指定）を
                   ;; サポートしていない
                   ;; ゆえに範囲の終了地点は無視
                   (one-or-more digit) "." (one-or-more digit) ": "
                   "error: " (message)
                   line-end)
            (warning line-start (minimal-match (one-or-more not-newline)) ":"
                     line "." column "-"
                     (one-or-more digit) "." (one-or-more digit) ": "
                     "info warning: " (message)
                     line-end)
            (info line-start (minimal-match (one-or-more not-newline)) ":"
                  line "." column "-"
                  (one-or-more digit) "." (one-or-more digit) ": "
                  "info: " (message)
                  line-end))
           :modes (html-mode nxhtml-mode web-mode))

         ;; 有効化
         (let ((target-and-other-checkers (member 'html-tidy flycheck-checkers)))
           (if target-and-other-checkers
               ;; デフォルトの (X)HTML チェッカ `html-tidy' と入れ替える
               (setcar target-and-other-checkers 'vnu)
             ;; 未追加ならリスト先頭に追加
             (add-to-list 'flycheck-checkers 'vnu))))

       ;; -----------------------------
       ;; PATCH: Sass（.scss/.sass 両形式）チェック時にキャッシュを使わせない
       ;; -----------------------------
       (dolist (checker '(scss sass))
         (if (and (flycheck-registered-checker-p checker)
                  (not (member "-C" (flycheck-checker-arguments checker))))
             ;; あえて破壊的に変更（元のリストに追加したい）
             (nconc (get checker 'flycheck-command) '("-C"))))

       ;; -----------------------------
       ;; PATCH: temp ファイルのデフォルトコーディングシステムを、
       ;;        強制的に UTF-8 (LF) とする
       ;; -----------------------------
       (defun flycheck-save-buffer-to-file (file-name)
         "Save the contents of the current buffer to FILE-NAME."
         ;; 他の部分は元定義と一致させる
         (make-directory (file-name-directory file-name) t)
         ;; FIXME: もっと柔軟に設定できるようにならないか？
         (let ((coding-system-for-write 'utf-8-unix) ; ここだけ変更・決め打ち
               (jka-compr-inhibit t))
           (write-region nil nil file-name nil 0))))


     ;; -----------------------------------------------------------------------
     ;; 自動静的解析拡張（モードライン変更）
     ;; -----------------------------------------------------------------------
     (use-package flycheck-color-mode-line
       ;; :disabled
       :after (:all flycheck)
       :ensure t
       :defer t
       :hook ((flycheck-mode . flycheck-color-mode-line-mode)))


     ;; -----------------------------------------------------------------------
     ;; 自動静的解析 (OLD)
     ;; -----------------------------------------------------------------------
     (use-package flymake
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(flymake-run-in-place nil))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'flymake-mode)
              (my-change-lighter flymake-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; フレーム
     ;; -----------------------------------------------------------------------
     (use-package frame
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; フレームサイズ変更を px 単位で実行できるようにする
        ;;
        '(frame-resize-pixelwise t))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'blink-cursor-mode)
           ;;
           ;; カーソルは点滅させない
           ;;
           (blink-cursor-mode -1)))


     ;; -----------------------------------------------------------------------
     ;; OBSOLETE: フレーム位置・サイズ復元
     ;; -----------------------------------------------------------------------
     ;; v24.4 (r113242) 以降では `desktop' に同一機能 `desktop-restore-frames'
     ;; が実装されている
     ;;
     ;; 条件分岐による起動制御を実施する
     ;; `frame-restore-mode' は自動検出して停止してくれるが、warning を吐くため
     ;;
     ;; see also:
     ;; http://bzr.savannah.gnu.org/lh/emacs/trunk/revision/113242
     ;; -----------------------------------------------------------------------
     (use-package frame-restore
       ;; :disabled
       :if (and (= emacs-major-version 24)
                (< emacs-minor-version 4))
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(frame-restore-parameters-file ,(convert-standard-filename "~/.emacs.frame-restore-parameters.el")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'frame-restore-mode)
           (frame-restore-mode +1))
       :config
       (if (not noninteractive)
           ;; なるべく「最後」に実行されるよう調整
           ;; 他のフレーム状態変更機能と衝突させないため
           (when (fboundp 'frame-restore-save-parameters)
             (remove-hook 'kill-emacs-hook #'frame-restore-save-parameters)
             (add-hook 'kill-emacs-hook #'frame-restore-save-parameters t))))


     ;; -----------------------------------------------------------------------
     ;; フレームセット
     ;; -----------------------------------------------------------------------
     (use-package frameset
       ;; :disabled
       :defer t
       ;; 全設定が完了してから実行しなければならない
       ;; 途中で追加される項目がありうるため
       :hook ((after-init . my-frameset-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-frameset-initialize ()
         "Initialize `frameset' when `after-init-hook' running."
         (eval-after-load 'frameset
           '(when (listp frameset-filter-alist)
              ;; `desktop' で保存不要な項目はすべて `:never' にする
              (dolist (key '(background-color
                             foreground-color
                             font
                             frameset--text-pixel-height
                             frameset--text-pixel-width
                             GUI:font))
                (setcdr (assoc key frameset-filter-alist) :never))))))


     ;; -----------------------------------------------------------------------
     ;; Google 翻訳インターフェース
     ;; -----------------------------------------------------------------------
     (use-package google-translate
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-c C-t p" . google-translate-at-point)
              ("C-c C-t o" . google-translate-at-point-reverse)
              ("C-c C-t q" . google-translate-query-translate)
              ("C-c C-t w" . google-translate-query-translate-reverse)
              ("C-c C-t s" . google-translate-smooth-translate))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(google-translate-default-source-language "auto")
        '(google-translate-default-target-language "ja")
        '(google-translate-enable-ido-completion t)
        '(google-translate-show-phonetic nil)
        '(google-translate-listen-program nil)
        '(google-translate-output-destination 'popup)
        '(google-translate-pop-up-buffer-set-focus t)
        '(google-translate-listen-button-label "[Listen]")))


     ;; -----------------------------------------------------------------------
     ;; `grep'
     ;; -----------------------------------------------------------------------
     (use-package grep
       ;; :disabled
       :demand (member system-type '(ms-dos windows-nt))
       :bind (("C-M-g" . rgrep))
       :init
       ;; -----------------------------
       ;; デフォルト値 (Windows ONLY)
       ;;------------------------------
       (if (member system-type '(ms-dos windows-nt))
           (custom-set-variables
            ;;
            ;; 例外が出るため NUL デバイスは使わせない
            ;;
            '(grep-use-null-device nil)))

       ;; -----------------------------
       ;; 初期化 (Windows ONLY)
       ;; -----------------------------
       (when (member system-type '(ms-dos windows-nt))
         ;; PATH は通っていないが、`exec-path' は通っている場合を想定
         ;;
         ;; すべて `defvar' 定義なので、 `autoload' 前後での
         ;; `custom-set-variables' による設定は不可能
         ;; 明示的ロード後～関数実行前までに設定しなければならない
         (setq grep-program
               (purecopy (or (executable-find "grep")
                             "grep")))
         (setq find-program
               (purecopy (or (executable-find "find")
                             "find")))
         (setq xargs-program
               (purecopy (or (executable-find "xargs")
                             "xargs"))))
       :config
       ;; -----------------------------
       ;; PATCH: grep 2.21 から環境変数 `GREP_OPTIONS' が deprecated に
       ;;        なったため、v24.x までの `grep' では結果に warning が出る
       ;;        問題を回避
       ;;        v25.0 以降はパッチが適用されたため、何もしない
       ;; -----------------------------
       ;; HACK: `add-to-list' を用いている部分は、元コードのままとする
       ;; -----------------------------
       ;; see also:
       ;; https://lists.gnu.org/archive/html/info-gnu/2014-11/msg00009.html
       ;; https://lists.gnu.org/archive/html/emacs-diffs/2014-09/msg00134.html
       ;; -----------------------------
       (if (< emacs-major-version 25)
           (defcustom grep-highlight-matches 'auto-detect
             "Use special markers to highlight grep matches.

Some grep programs are able to surround matches with special
markers in grep output.  Such markers can be used to highlight
matches in grep mode.  This requires `font-lock-mode' to be active
in grep buffers, so if you have globally disabled font-lock-mode,
you will not get highlighting.

This option sets the environment variable GREP_COLORS to specify
markers for highlighting and GREP_OPTIONS to add the --color
option in front of any explicit grep options before starting
the grep.

When this option is `auto', grep uses `--color=auto' to highlight
matches only when it outputs to a terminal (when `grep' is the last
command in the pipe), thus avoiding the use of any potentially-harmful
escape sequences when standard output goes to a file or pipe.

To make grep highlight matches even into a pipe, you need the option
`always' that forces grep to use `--color=always' to unconditionally
output escape sequences.

In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults' when the default value is `auto-detect'.
To change the default value, use Customize or call the function
`grep-apply-setting'."
             :type '(choice (const :tag "Do not highlight matches with grep markers" nil)
                            (const :tag "Highlight matches with grep markers" t)
                            (const :tag "Use --color=always" always)
                            (const :tag "Use --color" auto)
                            (other :tag "Not Set" auto-detect))
             :set 'grep-apply-setting
             :version "22.1"
             :group 'grep)

         (defun grep-process-setup ()
           "Setup compilation variables and buffer for `grep'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
           (when (eq grep-highlight-matches 'auto-detect)
             (grep-compute-defaults))
           (unless (or (eq grep-highlight-matches 'auto-detect)
                       (null grep-highlight-matches)
                       ;; Don't output color escapes if they can't be
                       ;; highlighted with `font-lock-face' by `grep-filter'.
                       (null font-lock-mode))
             ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
             ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'
             (setenv "TERM" "emacs-grep")
             ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions
             (setenv "GREP_COLOR" "01;31")
             ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions
             (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne"))
           (set (make-local-variable 'compilation-exit-message-function)
                (lambda (status code msg)
                  (if (eq status 'exit)
                      ;; This relies on the fact that `compilation-start'
                      ;; sets buffer-modified to nil before running the command,
                      ;; so the buffer is still unmodified if there is no output.
                      (cond ((and (zerop code) (buffer-modified-p))
                             '("finished (matches found)\n" . "matched"))
                            ((not (buffer-modified-p))
                             '("finished with no matches found\n" . "no match"))
                            (t
                             (cons msg code)))
                    (cons msg code))))
           (run-hooks 'grep-setup-hook))

         (defun grep-compute-defaults ()
           ;; Keep default values.
           (unless grep-host-defaults-alist
             (add-to-list
              'grep-host-defaults-alist
              (cons nil
                    `((grep-command ,grep-command)
                      (grep-template ,grep-template)
                      (grep-use-null-device ,grep-use-null-device)
                      (grep-find-command ,grep-find-command)
                      (grep-find-template ,grep-find-template)
                      (grep-find-use-xargs ,grep-find-use-xargs)
                      (grep-highlight-matches ,grep-highlight-matches)))))
           (let* ((host-id
                   (intern (or (file-remote-p default-directory) "localhost")))
                  (host-defaults (assq host-id grep-host-defaults-alist))
                  (defaults (assq nil grep-host-defaults-alist)))
             ;; There are different defaults on different hosts.  They must be
             ;; computed for every host once.
             (dolist (setting '(grep-command grep-template
                                             grep-use-null-device grep-find-command
                                             grep-find-template grep-find-use-xargs
                                             grep-highlight-matches))
               (set setting
                    (cadr (or (assq setting host-defaults)
                              (assq setting defaults)))))

             (unless (or (not grep-use-null-device) (eq grep-use-null-device t))
               (setq grep-use-null-device
                     (with-temp-buffer
                       (let ((hello-file (expand-file-name "HELLO" data-directory)))
                         (not
                          (and (if grep-command
                                   ;; `grep-command' is already set, so
                                   ;; use that for testing.
                                   (grep-probe grep-command
                                               `(nil t nil "^English" ,hello-file)
                                               #'call-process-shell-command)
                                 ;; otherwise use `grep-program'
                                 (grep-probe grep-program
                                             `(nil t nil "-nH" "^English" ,hello-file)))
                               (progn
                                 (goto-char (point-min))
                                 (looking-at
                                  (concat (regexp-quote hello-file)
                                          ":[0-9]+:English")))))))))
             (unless (and grep-command grep-find-command
                          grep-template grep-find-template)
               (let ((grep-options
                      (concat (and grep-highlight-matches
                                   (grep-probe grep-program
                                               `(nil nil nil "--color" "x" ,null-device)
                                               nil 1)
                                   (if (eq grep-highlight-matches 'always)
                                       "--color=always " "--color "))
                              (if grep-use-null-device "-n" "-nH")
                              (if (grep-probe grep-program
                                              `(nil nil nil "-e" "foo" ,null-device)
                                              nil 1)
                                  " -e"))))
                 (unless grep-command
                   (setq grep-command
                         (format "%s %s " grep-program grep-options)))
                 (unless grep-template
                   (setq grep-template
                         (format "%s <X> <C> %s <R> <F>" grep-program grep-options)))
                 (unless grep-find-use-xargs
                   (setq grep-find-use-xargs
                         (cond
                          ((grep-probe find-program
                                       `(nil nil nil ,null-device "-exec" "echo"
                                             "{}" "+"))
                           'exec-plus)
                          ((and
                            (grep-probe find-program `(nil nil nil ,null-device "-print0"))
                            (grep-probe xargs-program `(nil nil nil "-0" "echo")))
                           'gnu)
                          (t
                           'exec))))
                 (unless grep-find-command
                   (setq grep-find-command
                         (cond ((eq grep-find-use-xargs 'gnu)
                                ;; Windows shells need the program file name
                                ;; after the pipe symbol be quoted if they use
                                ;; forward slashes as directory separators.
                                (format "%s . -type f -print0 | \"%s\" -0 %s"
                                        find-program xargs-program grep-command))
                               ((memq grep-find-use-xargs '(exec exec-plus))
                                (let ((cmd0 (format "%s . -type f -exec %s"
                                                    find-program grep-command))
                                      (null (if grep-use-null-device
                                                (format "%s " null-device)
                                              "")))
                                  (cons
                                   (if (eq grep-find-use-xargs 'exec-plus)
                                       (format "%s %s{} +" cmd0 null)
                                     (format "%s {} %s%s" cmd0 null
                                             (shell-quote-argument ";")))
                                   (1+ (length cmd0)))))
                               (t
                                (format "%s . -type f -print | \"%s\" %s"
                                        find-program xargs-program grep-command)))))
                 (unless grep-find-template
                   (setq grep-find-template
                         (let ((gcmd (format "%s <C> %s <R>"
                                             grep-program grep-options))
                               (null (if grep-use-null-device
                                         (format "%s " null-device)
                                       "")))
                           (cond ((eq grep-find-use-xargs 'gnu)
                                  (format "%s . <X> -type f <F> -print0 | \"%s\" -0 %s"
                                          find-program xargs-program gcmd))
                                 ((eq grep-find-use-xargs 'exec)
                                  (format "%s . <X> -type f <F> -exec %s {} %s%s"
                                          find-program gcmd null
                                          (shell-quote-argument ";")))
                                 ((eq grep-find-use-xargs 'exec-plus)
                                  (format "%s . <X> -type f <F> -exec %s %s{} +"
                                          find-program gcmd null))
                                 (t
                                  (format "%s . <X> -type f <F> -print | \"%s\" %s"
                                          find-program xargs-program gcmd))))))))
             (when (eq grep-highlight-matches 'auto-detect)
               (setq grep-highlight-matches
                     (with-temp-buffer
                       (and (grep-probe grep-program '(nil t nil "--help"))
                            (progn
                              (goto-char (point-min))
                              (search-forward "--color" nil t))
                            ;; Windows and DOS pipes fail `isatty' detection in Grep.
                            (if (memq system-type '(windows-nt ms-dos))
                                'always 'auto)))))

             ;; Save defaults for this host.
             (setq grep-host-defaults-alist
                   (delete (assq host-id grep-host-defaults-alist)
                           grep-host-defaults-alist))
             (add-to-list
              'grep-host-defaults-alist
              (cons host-id
                    `((grep-command ,grep-command)
                      (grep-template ,grep-template)
                      (grep-use-null-device ,grep-use-null-device)
                      (grep-find-command ,grep-find-command)
                      (grep-find-template ,grep-find-template)
                      (grep-find-use-xargs ,grep-find-use-xargs)
                      (grep-highlight-matches ,grep-highlight-matches))))))))


     ;; -----------------------------------------------------------------------
     ;; 拡張補完・展開
     ;; -----------------------------------------------------------------------
     (use-package hippie-exp
       ;; :disabled
       :defer t
       :bind (("M-/" . hippie-expand)))


     ;; -----------------------------------------------------------------------
     ;; カレントカーソル行強調
     ;; -----------------------------------------------------------------------
     (use-package hl-line
       ;; :disabled
       :defer t
       ;; FIXME: `after-init-hook' 後に実行した `load-theme' に対応したい
       ;;        `advice-add' の after で `enable-theme' を実行してもダメ
       :hook ((after-init . my-hl-line-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-hl-line-initialize ()
         "Initialize `hl-line' settings."
         (when (and (require 'color nil :noerror) ; 未ロードがありうるため必須
                    (fboundp 'color-rgb-to-hsl)
                    (fboundp 'color-name-to-rgb)
                    (fboundp 'color-darken-name)
                    (fboundp 'color-lighten-name))
           (let* ((L-diff 20)
                  (background-color (face-attribute 'default :background))
                  (L (nth 2 (apply 'color-rgb-to-hsl
                                   (color-name-to-rgb background-color))))
                  (line-background-color (funcall (if (< L 0.5)
                                                      'color-lighten-name
                                                    'color-darken-name)
                                                  background-color
                                                  L-diff)))
             (custom-set-faces
              `(hl-line ((((class color))
                          (:background ,line-background-color :inherit nil))))))))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'global-hl-line-mode)
           (global-hl-line-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; 強化バッファ一覧
     ;; -----------------------------------------------------------------------
     (use-package ibuffer
       ;; :disabled
       :defer t
       :bind (("C-x C-b" . ibuffer))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(ibuffer-expert t))
       :config
       ;; -----------------------------
       ;; 機能拡張
       ;; -----------------------------
       (when (and (boundp 'ibuffer-formats)
                  (boundp 'ibuffer-sorting-mode)
                  (boundp 'ibuffer-last-sorting-mode)
                  (boundp 'ibuffer-sorting-reversep)
                  (boundp 'ibuffer-sorting-functions-alist))
         ;; バッファ名の表示を30文字に拡張
         ;; カラム幅が揃わなくなるため、-1 にはできない
         (let* (;; `customize-mark-to-save' の評価を t にするため、
                ;; 明示的にコピー
                (formats (copy-tree ibuffer-formats))
                (settings (assoc 'name (assoc 'mark formats))))
           ;; 該当する設定項目がなければ何もしない
           ;; 将来的に項目が変更された場合でも、例外を出さないための対策
           (when settings
             (setcdr settings '(30 30 :left :elide))
             (custom-set-variables
              `(ibuffer-formats ',formats))))

         ;; メジャーモード名 + ファイルパスでソート
         ;;
         ;; see also:
         ;; http://www.emacswiki.org/emacs/IbufferMode#toc10
         (define-ibuffer-sorter mode-name-and-path-alphabetic
           "Sort the buffers by their mode and paths.
Ordering is lexicographic."
           (:description "major mode + alphabetic")
           (string-lessp
            (with-current-buffer (car a)
              (let* ((file-path (buffer-file-name))
                     (buffer-name (buffer-name))
                     (path file-path)
                     (mode-name (format-mode-line mode-name))
                     (prefix "999"))
                (when (and (not (stringp file-path))
                           (stringp buffer-name))
                  (setq prefix "000")
                  (setq path buffer-name))
                (concat prefix ": " mode-name ": " path)))
            (with-current-buffer (car b)
              (let* ((file-path (buffer-file-name))
                     (buffer-name (buffer-name))
                     (path file-path)
                     (mode-name (format-mode-line mode-name))
                     (prefix "999"))
                (when (and (not (stringp file-path))
                           (stringp buffer-name))
                  (setq prefix "000")
                  (setq path buffer-name))
                (concat prefix ": " mode-name ": " path)))))

         (custom-set-variables
          '(ibuffer-default-sorting-mode 'mode-name-and-path-alphabetic))))


     ;; -----------------------------------------------------------------------
     ;; 強化バッファ一覧 (`ibuffer') 拡張（`projectile' サポート）
     ;; -----------------------------------------------------------------------
     (use-package ibuffer-projectile
       ;; :disabled
       :after (:all ibuffer
                    projectile)
       :ensure t
       :defer t
       :hook ((ibuffer . my-ibuffer-projectile-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-ibuffer-projectile-initialize ()
         "Initialize `ibuffer' settings."
         (if (fboundp 'ibuffer-projectile-set-filter-groups)
             (ibuffer-projectile-set-filter-groups))))


     ;; -----------------------------------------------------------------------
     ;; ファイル操作の簡略化
     ;; -----------------------------------------------------------------------
     (use-package ido
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(ido-enable-flex-matching t)
        '(ido-create-new-buffer 'always)
        '(ido-use-virtual-buffers t)
        '(ido-max-file-prompt-width 0)
        '(ido-use-filename-at-point 'guess)
        '(ido-unc-hosts t)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(ido-save-directory-list-file ,(convert-standard-filename "~/.emacs.ido-save-directory-list.el")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'ido-mode)
           (ido-mode +1))

       (eval-after-load 'ido
         '(if (fboundp 'ido-everywhere)
              (ido-everywhere +1))))


     ;; -----------------------------------------------------------------------
     ;; 画像の直接表示
     ;; -----------------------------------------------------------------------
     (use-package image-file
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'auto-image-file-mode)
           (auto-image-file-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; インクリメンタル検索
     ;; -----------------------------------------------------------------------
     (use-package isearch
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; 検索時に大小文字を区別しない
        ;;
        '(isearch-case-fold-search t)
        ;;
        ;; 逆検索時に大小文字を区別しない
        ;;
        '(isearch-last-case-fold-search t)))


     ;; -----------------------------------------------------------------------
     ;; JavaScript リファクタリング補助
     ;; -----------------------------------------------------------------------
     (use-package js2-refactor
       ;; :disabled
       :after (:all js2-mode)
       :ensure t
       :demand t)


     ;; -----------------------------------------------------------------------
     ;; LSP (Language Server Protocol) クライアント
     ;;
     ;; see also:
     ;; https://microsoft.github.io/language-server-protocol/
     ;; https://langserver.org/
     ;; -----------------------------------------------------------------------
     (use-package lsp-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((css-mode . lsp)
              (html-mode . lsp)
              (js-mode . lsp)
              (js2-mode . lsp)
              (json-mode . lsp)
              (php-mode . lsp)
              (sass-mode . lsp)
              (scss-mode . lsp)
              (sh-mode . lsp)
              (vue-mode . lsp))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(lsp-auto-guess-root t)
        '(lsp-restart 'ignore)
        '(lsp-prefer-flymake nil)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(lsp-session-file ,(convert-standard-filename "~/.emacs.lsp-session"))))


     ;; -----------------------------------------------------------------------
     ;; LSP (Language Server Protocol) クライアント拡張 (UI)
     ;; -----------------------------------------------------------------------
     (use-package lsp-ui
       ;; :disabled
       :after (:all lsp-mode)
       :ensure t
       :demand t
       :hook ((lsp-after-open . lsp-ui-flycheck-enable)))


     ;; -----------------------------------------------------------------------
     ;; Git インターフェース
     ;; -----------------------------------------------------------------------
     (use-package magit
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-x g" . magit-status))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; カレントバッファを表示しているウインドウに表示させる
        ;;
        '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)))


     ;; -----------------------------------------------------------------------
     ;; OBSOLETE: 環境に依存しないフレーム状態復元
     ;; -----------------------------------------------------------------------
     (use-package maxframe
       ;; :disabled
       :if (and (= emacs-major-version 24)
                (< emacs-minor-version 4))
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (when (and (not noninteractive)
                  (fboundp 'maximize-frame)
                  (fboundp 'restore-frame))
         ;; 最大化時は次の条件をすべて満たす必要がある:
         ;;   * 必ずフレーム構築の最後であること
         ;;   * 他のフレームサイズ状態変更機能よりもあとで実行されること
         (add-hook 'window-setup-hook #'maximize-frame)
         ;; FIXME: (w32-send-sys-command 61728) が async なので
         ;;        `restore-frame' → `frame-restore-save-parameters' の順に
         ;;        実行されるよう配慮してもムダ
         ;;        現状 `frame-restore-save-parameters' 実行時の
         ;;        フレームサイズが最大化時のものになってしまっている
         (add-hook 'kill-emacs-hook #'restore-frame)))


     ;; -----------------------------------------------------------------------
     ;; メニューバー
     ;; -----------------------------------------------------------------------
     (use-package menu-bar
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'menu-bar-mode)
           ;;
           ;; 非表示
           ;;
           (menu-bar-mode -1)))


     ;; -----------------------------------------------------------------------
     ;; ローマ字入力から日本語をインクリメンタル検索
     ;; -----------------------------------------------------------------------
     (use-package migemo
       ;; :disabled
       :ensure t
       :demand t
       :bind (:map isearch-mode-map
                   ("C-c C-s" . migemo-isearch-toggle-migemo))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; C/Migemo 利用設定
        ;;
        `(migemo-command ,(executable-find "cmigemo"))
        '(migemo-options '("-q" "--emacs"))
        ;;
        ;; 空白文字と認識させる対象を広げる
        ;;
        '(migemo-white-space-regexp "[[:space:]\s-]*")
        ;;
        ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
        ;;
        `(migemo-directory ,(convert-standard-filename "~"))
        ;;
        ;; `migemo' 側で定義されている `isearch' 関連キーバインドを使わせない
        ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
        ;;
        '(migemo-use-default-isearch-keybinding nil)
        ;;
        ;; 辞書ファイルはデフォルトのものを利用
        ;;
        `(migemo-dictionary ,(convert-standard-filename
                              (if (member system-type '(ms-dos windows-nt))
                                  "C:/programs/cmigemo/share/migemo/utf-8/migemo-dict"
                                "/usr/local/share/migemo/utf-8/migemo-dict")))
        '(migemo-user-dictionary nil)
        '(migemo-regex-dictionary nil)
        ;;
        ;; 辞書エンコーディングを明示
        ;;
        '(migemo-coding-system 'utf-8-unix)
        ;;
        ;; キャッシュを使わせる
        ;;
        '(migemo-use-pattern-alist t)
        '(migemo-use-frequent-pattern-alist t)
        '(migemo-pattern-alist-length 1024)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        '(migemo-pattern-alist-file (expand-file-name ".emacs.migemo-pattern" migemo-directory))
        '(migemo-frequent-pattern-alist-file (expand-file-name ".emacs.migemo-frequent" migemo-directory)))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (and (boundp 'migemo-command)
                migemo-command
                (boundp 'migemo-dictionary)
                (file-exists-p migemo-dictionary))
           (migemo-init)))


     ;; -----------------------------------------------------------------------
     ;; NSM (Network Security Manager)
     ;; -----------------------------------------------------------------------
     (use-package nsm
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(nsm-settings-file ,(convert-standard-filename "~/.emacs.network-security.data"))))


     ;; -----------------------------------------------------------------------
     ;; カーソルの移動履歴
     ;; -----------------------------------------------------------------------
     (use-package point-undo
       ;; :disabled
       :ensure t
       :demand t
       :bind (("M-]" . point-undo)
              ("M-[" . point-redo)))


     ;; -----------------------------------------------------------------------
     ;; 印刷 (PostScript)
     ;; -----------------------------------------------------------------------
     (use-package ps-print
       ;; :disabled
       :defer t
       :bind (("C-c p b" . ps-print-buffer)
              ("C-c p f" . ps-print-buffer-with-faces))
       :hook ((ps-print . my-ps-print-hook-listener))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; 用紙
        ;;
        '(ps-paper-type 'a4)
        '(ps-landscape-mode nil) ; 縦剥き
        ;;
        ;; 本文フォント
        ;;
        '(ps-font-family 'Courier)
        '(ps-font-size '(10.0 . 10.0)) ; 縦：10.0pt、横：10.0pt
        ;;
        ;; 色
        ;;
        '(ps-print-color-p t)
        '(ps-default-fg t)
        '(ps-default-bg t)
        '(ps-use-face-background t)
        ;;
        ;; プリンターに対する命令
        ;;
        '(ps-multibyte-buffer 'non-latin-printer)
        ;;
        ;; 行調整
        ;;
        '(ps-line-spacing 2.0)
        ;;
        ;; 行番号
        ;;
        '(ps-line-number t)
        '(ps-line-number-font "Times-Italic") ; FIXME: Courier にしたい
        ;;
        ;; 水平レイアウト
        ;;
        '(ps-left-margin (/ (* 72 1.4) 2.54)) ; 14mm（行番号が切れないようにする）
        '(ps-inter-column (/ (* 72 1.0) 2.54)) ; 10mm
        '(ps-right-margin (/ (* 72 0.54) 2.54)) ; 5.4mm（ヘッダ・フッタの box 右端が切れないようにする）
        ;;
        ;; 垂直レイアウト
        ;;
        '(ps-top-margin (/ (* 72 0.9) 2.54)) ; 9mm（ヘッダ box 上端が切れないようにする）
        '(ps-header-offset (/ (* 72 0.1) 2.54)) ; 1mm
        '(ps-footer-offset (/ (* 72 0.37) 2.54)) ; 3.7mm（フッタ box 上端へカブらないようにする）
        '(ps-bottom-margin (/ (* 72 0.55) 2.54)) ; 5.5mm（フッタ box 下端が切れないようにする）
        ;;
        ;; ヘッダ
        ;;
        '(ps-print-header t)
        '(ps-header-lines 2)
        '(ps-print-header-frame t)
        '(ps-left-header (list 'ps-get-buffer-name
                               'ps-header-dirpart))
        '(ps-right-header (list 'ps-time-stamp-yyyy-mm-dd
                                'ps-time-stamp-hh:mm:ss))
        '(ps-header-line-pad 0.15)
        '(ps-header-font-family 'Courier)
        '(ps-header-font-size '(10 . 10))
        '(ps-header-title-font-size '(10 . 10))
        ;;
        ;; フッタ
        ;;
        '(ps-print-footer t)
        '(ps-footer-lines 1)
        '(ps-print-footer-frame t)
        '(ps-left-footer nil)
        '(ps-right-footer (list "/pagenumberstring load"))
        '(ps-footer-line-pad 0.15)
        '(ps-footer-font-family 'Courier)
        '(ps-footer-font-size '(10 . 10))
        '(ps-footer-title-font-size '(10 . 10)))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-ps-print-hook-listener ()
         "Initialize `ps-print' when load hook `ps-print-hook'."

         ;; FIXME: 長い行の右端が切れてしまう問題を解決しなければならない
         ;;        いちいち改行をカレントバッファへ明示的に入れる方法はナシで
         ;;        プリント前処理 temp バッファを作ればいいかもしれないが……？

         ;; 極限まで細くする
         (if (boundp 'ps-header-frame-alist)
             (setcdr (assoc 'border-width ps-header-frame-alist) 0.1))))


     ;; -----------------------------------------------------------------------
     ;; 印刷ユーティリティ
     ;; -----------------------------------------------------------------------
     (use-package printing
       ;; :disabled
       :demand t
       :bind (("C-c p p" . pr-interface))
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'pr-update-menus)
           ;;
           ;; メニューに項目追加
           ;;
           (pr-update-menus +1)))


     ;; -----------------------------------------------------------------------
     ;; 汎用プロジェクト管理
     ;; -----------------------------------------------------------------------
     (use-package projectile
       ;; :disabled
       :ensure t
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(projectile-enable-caching t)
        '(projectile-completion-system (cond ((featurep 'ido) 'ido)
                                             ((featurep 'ivy) 'ivy)
                                             ((featurep 'helm) 'helm)
                                             (t 'default)))
        '(projectile-mode-line-prefix "")
        '(projectile-keymap-prefix (kbd "C-c C-p"))
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(projectile-cache-file ,(convert-standard-filename "~/.emacs.projectile.cache"))
        `(projectile-known-projects-file ,(convert-standard-filename "~/.emacs.projectile-bookmarks.eld")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'projectile-mode)
           (projectile-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; 自動カラー表示
     ;; -----------------------------------------------------------------------
     (use-package rainbow-mode
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (eval-after-load 'rainbow-mode
         '(when (boundp 'rainbow-html-colors-major-mode-list)
            (add-to-list 'rainbow-html-colors-major-mode-list 'sass-mode)
            (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode)
            (add-to-list 'rainbow-html-colors-major-mode-list 'less-mode)))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'rainbow-mode)
              (my-change-lighter rainbow-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; ファイル履歴保存
     ;; -----------------------------------------------------------------------
     (use-package recentf
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; すべての履歴を保存
        ;;
        '(recentf-max-saved-items nil)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(recentf-save-file ,(convert-standard-filename "~/.emacs.recentf.el"))))


     ;; -----------------------------------------------------------------------
     ;; モードラインからマイナーモードの表示をを隠す
     ;; -----------------------------------------------------------------------
     (use-package rich-minority
       ;; :disabled
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(rm-blacklist nil)
        '(rm-whitelist nil)
        '(rm-text-properties nil))

       ;; -----------------------------
       ;; 起動
       ;;------------------------------
       (if (fboundp 'rich-minority-mode)
           (rich-minority-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; ミニバッファの履歴を残す
     ;; -----------------------------------------------------------------------
     (use-package savehist
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; すべての履歴を保存
        ;;
        '(history-length t)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(savehist-file ,(convert-standard-filename "~/.emacs.savehist.el")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'savehist-mode)
           (savehist-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; ファイルごとにカーソル位置を保存
     ;; -----------------------------------------------------------------------
     (use-package saveplace
       ;; :disabled
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(save-place-file ,(convert-standard-filename "~/.emacs.saveplace.el")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (setq-default save-place t))


     ;; -----------------------------------------------------------------------
     ;; スクロールバー
     ;; -----------------------------------------------------------------------
     (use-package scroll-bar
       ;; :disabled
       :defer t
       ;; `after-init-hook' で実行しないと適用されない問題がある
       :hook ((after-init . my-scroll-bar-initilalize))
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       ;;
       ;; v24.x 以前
       ;;
       ;; ウインドウシステム上では、あらゆるスクロールバーを非表示化
       ;;
       (set-scroll-bar-mode (if window-system nil 'right))

       ;;
       ;; v25.x 以降
       ;;
       (defun my-scroll-bar-initilalize ()
         "Initialize `scroll-bar' settings."
         (eval-after-load 'scroll-bar
           '(when window-system
              (if (fboundp 'scroll-bar-mode)
                  (scroll-bar-mode -1))
              (if (fboundp 'horizontal-scroll-bar-mode)
                  (horizontal-scroll-bar-mode -1))))))


     ;; -----------------------------------------------------------------------
     ;; サーバ化
     ;; -----------------------------------------------------------------------
     ;; Windows 環境では `server-auth-dir' の「所有者」が：
     ;;   * Administrator (RID=500)
     ;;   * Administrators (RID=544)
     ;; である場合、`server-ensure-safe-dir' の評価が `nil' になる
     ;;
     ;; `server-auth-dir' で指定したフォルダの
     ;; 「プロパティ」→「セキュリティ」→「詳細設定」→「所有者」→「編集」
     ;; から、所有者をログオンユーザ自身に変更すること
     ;; -----------------------------------------------------------------------
     ;; Windows 環境では emacsclientw.exe 実行時に環境変数
     ;; %EMACS_SERVER_FILE% でサーバファイルのパスを明示しなければならない
     ;; （なぜ必要かは不明）
     ;;
     ;; この欠点をある程度回避した wemacs.cmd を用いること
     ;; -----------------------------------------------------------------------
     (use-package server
       ;; :disabled
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(server-auth-dir ,(convert-standard-filename "~/.emacs.server")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'server-force-delete)
           (server-force-delete))
       (if (fboundp 'server-start)
           (server-start)))


     ;; -----------------------------------------------------------------------
     ;; 基礎編集コマンド集
     ;; -----------------------------------------------------------------------
     (use-package simple
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'transient-mark-mode)
           ;;
           ;; 暫定マークを使用
           ;;
           (transient-mark-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; 各種カッコ関連機能拡張
     ;; -----------------------------------------------------------------------
     (use-package smartparens
       ;; :disabled
       :ensure t
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(sp-autoinsert-quote-if-followed-by-closing-pair t)
        '(sp-undo-pairs-separately t)
        '(sp-show-pair-from-inside t))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'smartparens-global-mode)
           (smartparens-global-mode +1))
       (if (fboundp 'show-smartparens-global-mode)
           (show-smartparens-global-mode +1))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'smartparens-mode)
              (my-change-lighter smartparens-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; タイムスタンプ記述
     ;; -----------------------------------------------------------------------
     (use-package time-stamp
       ;; :disabled
       :defer t
       :hook ((before-save . time-stamp))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; ISO 8601 (JIS X 0301) 形式にする
        ;;
        ;; see also:
        ;; https://ja.wikipedia.org/wiki/ISO_8601
        ;;
        ;; WARNING: `time-stamp-time-zone' を "+09:00" にしても、
        ;;          コロン以降が無視される
        ;;
        `(time-stamp-format
          ,(concat "%:y-%02m-%02dT%02H:%02M:%02S"
                   ;; タイムゾーンは別途指定
                   ;;
                   ;; 以下理由
                   ;;
                   ;; `time-stamp-string' の "%Z" は
                   ;; (format-time-string "%Z") と同義
                   ;; この値をそのまま扱うため、
                   ;; 環境の差異が出やすくマトモに使えない
                   ;;
                   ;; `time-stamp-string' の "%z" は
                   ;; (format-time-string "%#Z") と同義
                   ;; (format-time-string "%z") ではない点に注意
                   ;; この値をそのまま扱うため、
                   ;; 環境の差異が出やすくマトモに使えない
                   ;; また `format-time-string' 側のバグにより、
                   ;; 環境次第で文字化けする
                   ;;
                   ;; Windows 環境（環境変数 %TZ% 未指定・+09:00 ゾーン）では
                   ;; 次の値が用いられてしまう
                   ;; （どちらもエンコーディングは `cp932-2-byte'）：
                   ;;
                   ;; "%Z" (≒ "%Z"):  #("東京 (標準時)" 0 8
                   ;; "%z" (≒ "%#Z"): #("東京 (婦準時)" 0 8
                   ;;
                   ;; 「標」→「婦」に文字化けしているのがわかる
                   ;; また、`propertize' されている
                   ;;
                   ;; FIXME: 現状では、OS 側の動的なタイムゾーン変更に追従不能
                   ;;        都度評価にしたい
                   (replace-regexp-in-string
                    ;; コロンがない形式を返されるため、強制的にコロンを付与
                    ;; 厳密なチェックにより "±1259" 形式のみ対象にする
                    ;;   → 他は無視
                    "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
                    "\\1:\\2"
                    ;; タイムゾーンが UTC でも "Z" でなく "+0000" を返してくる
                    ;; 今のところ、あえて "Z" への変換はしないでおく
                    (format-time-string "%z"))))))


     ;; -----------------------------------------------------------------------
     ;; ツールバー
     ;; -----------------------------------------------------------------------
     (use-package tool-bar
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'tool-bar-mode)
           ;;
           ;; 非表示
           ;;
           (tool-bar-mode -1)))


     ;; -----------------------------------------------------------------------
     ;; ツールチップ
     ;; -----------------------------------------------------------------------
     (use-package tooltip
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'tooltip-mode)
           ;;
           ;; 非表示
           ;;
           (tooltip-mode -1)))


     ;; -----------------------------------------------------------------------
     ;; TRAMP (Transparent Remote Access, Multiple Protocols)
     ;; -----------------------------------------------------------------------
     (use-package tramp
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (eval-after-load 'tramp
         ;;
         ;; WARNING: 遅延ロード (`autoload') 後に実行しないと適用されない
         ;;
         '(custom-set-variables
           ;;
           ;; ローカル環境にのみ保存
           ;;
           `(tramp-persistency-file-name ,(convert-standard-filename "~/.emacs.tramp")))))


     ;; -----------------------------------------------------------------------
     ;; `undo' 拡張、`redo' 機能追加ならびに分岐履歴実装
     ;; -----------------------------------------------------------------------
     (use-package undo-tree
       ;; :disabled
       :ensure t
       :demand t
       :bind (("C-." . undo-tree-redo))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(undo-tree-mode-lighter ""))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'global-undo-tree-mode)
           (global-undo-tree-mode)))


     ;; -----------------------------------------------------------------------
     ;; `undo' 履歴の記憶
     ;; -----------------------------------------------------------------------
     (use-package undohist
       ;; :disabled
       :ensure t
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(undohist-directory ,(convert-standard-filename "~/.emacs.undohist")))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'undohist-initialize)
           (undohist-initialize)))


     ;; -----------------------------------------------------------------------
     ;; ファイル名を元に、より唯一性の高いバッファ名を生成
     ;; -----------------------------------------------------------------------
     (use-package uniquify
       ;; :disabled
       :demand t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(uniquify-buffer-name-style 'forward)
        '(uniquify-ignore-buffers-re "^*[^*]+*\\-")))


     ;; -----------------------------------------------------------------------
     ;; 空白文字強調
     ;; -----------------------------------------------------------------------
     (use-package whitespace
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; 「不正」位置の空白文字のみ強調
        ;;
        '(whitespace-style '(face
                             trailing
                             tabs
                             newline
                             empty
                             space-after-tab
                             space-before-tab
                             tab-mark
                             newline-mark))
        ;;
        ;; 行カラム最大値は `fill-column' を参照させる
        ;;
        '(whitespace-line-column nil))

       ;; -----------------------------
       ;; フェイス強調無効化
       ;; -----------------------------
       (custom-set-faces
        '(whitespace-space ((t
                             (:background nil)))))

       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'global-whitespace-mode)
           (global-whitespace-mode +1))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(progn
            (if (fboundp 'whitespace-mode)
                (my-change-lighter whitespace-mode nil))
            (if (fboundp 'whitespace-newline-mode)
                (my-change-lighter whitespace-newline-mode nil))
            (if (fboundp 'global-whitespace-mode)
                (my-change-lighter global-whitespace-mode nil))
            (if (fboundp 'global-whitespace-newline-mode)
                (my-change-lighter global-whitespace-newline-mode nil))))

       ;; -----------------------------
       ;; HACK: 全角空白 (U+3000) を HARD SPACE とみなして強調表示
       ;;
       ;; 表示テスト:
       ;;   U+0009: 「	」
       ;;   U+00A0: 「 」
       ;;   U+3000: 「　」
       ;; -----------------------------
       (when (and (boundp 'whitespace-style)
                  (boundp 'whitespace-display-mappings))
         (custom-set-variables
          ;;
          ;; 空白の強調を明示
          ;;
          `(whitespace-style ',(let ((styles (copy-tree whitespace-style)))
                                 ;; HARD SPACE の ON/OFF も含んでいる
                                 (add-to-list 'styles 'spaces)
                                 (add-to-list 'styles 'space-mark)))
          ;;
          ;; 検索条件を追加
          ;;
          '(whitespace-hspace-regexp "\\(\\(\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\|\x3000\\)+\\)")
          '(whitespace-trailing-regexp "\\([\t \u00A0\u3000]+\\)$"))

         ;;
         ;; 表示置換条件を追加
         ;;
         (add-to-list 'whitespace-display-mappings
                      '(space-mark ?\u3000 [?\u25a1] [?_ ?_])))

       ;; -----------------------------
       ;; HACK: 半角空白 (U+0020) を強調しないようにする
       ;;
       ;; 表示テスト:
       ;;   U+0020: 「 」
       ;; -----------------------------
       (if (boundp 'whitespace-display-mappings)
           (custom-set-variables
            ;;
            ;; 表示置換しないようにする
            ;;
            `(whitespace-display-mappings ',(delete '(space-mark ?\  [?\u00B7] [?.])
                                                    whitespace-display-mappings)))))


     ;; -----------------------------------------------------------------------
     ;; ウインドウ移動キーを直感的にする
     ;; -----------------------------------------------------------------------
     (use-package windmove
       ;; :disabled
       :defer t
       :bind (("C-S-b" . windmove-left)
              ("C-S-f" . windmove-right)
              ("C-S-p" . windmove-up)
              ("C-S-n" . windmove-down))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        ;;
        ;; フレーム端のウインドウでは無限スクロールするようにふるまう
        ;; 「マリオブラザーズ」左右画面端におけるループのような動き
        ;;
        '(windmove-wrap-around t)))


     ;; -----------------------------------------------------------------------
     ;; ウインドウの状態履歴を undo/redo
     ;; -----------------------------------------------------------------------
     (use-package winner
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'winner-mode)
           (winner-mode +1)))


     ;; -----------------------------------------------------------------------
     ;; スニペット挿入
     ;; -----------------------------------------------------------------------
     (use-package yasnippet
       ;; :disabled
       :ensure t
       :defer t
       :init
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       (if (fboundp 'yas-global-mode)
           (yas-global-mode +1))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'yas-minor-mode)
              (my-change-lighter yas-minor-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; END OF CONFIG: 詳細設定（マイナーモード）
     ;; -----------------------------------------------------------------------
     ))


;; ============================================================================
;; 詳細設定（メジャーモード）
;; ============================================================================
(eval-after-load 'use-package
  '(progn
     ;; -----------------------------------------------------------------------
     ;; Apache
     ;; -----------------------------------------------------------------------
     (use-package apache-mode
       ;; :disabled
       :ensure t
       :defer t
       :mode (("\\.conf\\'" . apache-mode))
       :config
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (eval-after-load 'apache-mode
         '(if (boundp 'apache-indent-level)
              (setq-local apache-indent-level 4))))


     ;; -----------------------------------------------------------------------
     ;; CSS
     ;; -----------------------------------------------------------------------
     (use-package css-mode
       ;; :disabled
       :defer t
       :hook ((css-mode . my-css-mode-initialize))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(css-indent-offset 2))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-css-mode-initialize ()
         "Initialize `css-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; GNU Emacs Lisp
     ;; -----------------------------------------------------------------------
     (use-package elisp-mode
       ;; :disabled
       :defer t
       :hook ((emacs-lisp-mode . my-emacs-lisp-mode-initialize)
              (lisp-interaction-mode . my-emacs-lisp-mode-initialize)
              (lisp-interaction-mode . my-lisp-interaction-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       ;;
       ;; 共通
       ;;
       (defun my-emacs-lisp-mode-initialize ()
         "Initialize `emacs-lisp-mode' and `lisp-interaction-mode' before file load."
         (setq-local indent-tabs-mode nil)
         (setq-local tab-width 8)

         ;; EditorConfig 対応
         ;;
         ;; TODO: `tab-width' 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style))))))

       ;;
       ;; `lisp-interaction-mode' ONLY
       ;;
       (defun my-lisp-interaction-mode-initialize ()
         "Initialize `lisp-interaction-mode-initialize' before file load."
         ;; `whitespace-mode' 無効化
         (eval-after-load 'whitespace
           '(progn
              (if (fboundp 'whitespace-mode)
                  (whitespace-mode -1))
              (when (boundp 'whitespace-style)
                (setq-local whitespace-style (copy-tree whitespace-style))
                (delete 'empty whitespace-style))))))


     ;; -----------------------------------------------------------------------
     ;; Haml
     ;; -----------------------------------------------------------------------
     (use-package haml-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((haml-mode . my-haml-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-haml-mode-initialize ()
         "Initialize `haml-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; Emacs Lisp インタラクション
     ;; -----------------------------------------------------------------------
     (use-package ielm
       ;; :disabled
       :defer t
       :hook ((ielm-mode-hook . my-ielm-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-ielm-mode-initialize ()
         "Initialize `ielm' major mode before file load."
         (setq-local indent-tabs-mode nil)
         (setq-local tab-width 8)))


     ;; -----------------------------------------------------------------------
     ;; JavaScript
     ;; -----------------------------------------------------------------------
     (use-package js2-mode
       ;; :disabled
       :ensure t
       :defer t
       :mode (("\\.js\\'" . js2-mode)
              ("\\.pac\\'" . js2-mode))
       :hook ((js2-mode . my-js2-mode-initialize))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(js2-highlight-level 3) ; すべての構文強調を有効化
        '(js2-basic-offset 4)
        '(js2-bounce-indent-p nil)
        '(js2-pretty-multiline-declarations t)
        '(js2-indent-switch-body nil) ; case 文はインデントしない
        '(js2-idle-timer-delay 0.2)
        '(js2-dynamic-idle-timer-adjust 0)
        '(js2-concat-multiline-strings t)
        ;;
        ;; 文法チェック関連
        ;;
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
        ;;
        ;; その他
        ;;
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
        ;;
        ;; JSLint
        ;;
        ;; 他ツールに任せるため、すべて無効化
        '(js2-include-jslint-globals nil))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-js2-mode-initialize ()
         "Initialize `js2-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; JSON
     ;; -----------------------------------------------------------------------
     (use-package json-mode
       ;; :disabled
       :ensure t
       :defer t
       :mode (("\\.bowerrc\\'" . json-mode)
              ("\\.ftppass\\'" . json-mode)
              ("\\.htmlhintrc\\'" . json-mode)
              ("\\.htmllintrc\\'" . json-mode)
              ("\\.jscsrc\\'" . json-mode)
              ("\\.jshintrc\\'" . json-mode)
              ("\\.json\\'" . json-mode)
              ("\\.stylelintrc\\'" . json-mode))
       :hook ((json-mode . my-json-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-json-mode-initialize ()
         "Initialize `json-mode' before file load."
         (setq-local indent-tabs-mode nil)
         (setq-local js-indent-level 2)
         (setq-local tab-width 2)

         ;; EditorConfig 対応
         ;;
         ;; TODO: `tab-width' と `js-indent-level' 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; Lisp
     ;; -----------------------------------------------------------------------
     (use-package lisp-mode
       ;; :disabled
       :defer t
       :hook ((lisp-mode . my-lisp-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-lisp-mode-initialize ()
         "Itialize `lisp-mode' before file load."
         (setq-local indent-tabs-mode nil)
         (setq-local tab-width 8)

         ;; EditorConfig 対応
         ;;
         ;; TODO: `tab-width' 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; Markdown
     ;; -----------------------------------------------------------------------
     (use-package markdown-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((markdown-mode . my-markdown-mode-initialize))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(markdown-command (or (executable-find "github-markup")
                               (executable-find "markdown")
                               "markdown"))
        '(markdown-command-needs-filename (equal markdown-command
                                                 (executable-find "github-markup")))
        '(markdown-coding-system 'utf-8-unix)
        ;;
        ;; プレビュー用に生成した実 HTML ファイルの残存を防ぐ
        ;;
        '(markdown-live-preview-delete-export 'delete-on-export))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-markdown-mode-initialize ()
         "Initialize `markdown-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style))))))
       :config
       ;; -----------------------------
       ;; 起動
       ;; -----------------------------
       ;;
       ;; プレーンテキストファイルは除外
       ;;
       (setq auto-mode-alist
             (delete '("\\.text\\'" . markdown-mode) auto-mode-alist)))


     ;; -----------------------------------------------------------------------
     ;; PHP
     ;; -----------------------------------------------------------------------
     (use-package php-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((php-mode . my-php-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-php-mode-initialize ()
         "Initialize `php-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; Sass (extension: ".sass")
     ;; -----------------------------------------------------------------------
     (use-package sass-mode
       ;; :disabled
       :ensure t
       :defer t
       :mode (("\\.sass\\'" . sass-mode))
       :hook ((sass-mode . my-sass-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-sass-mode-initialize ()
         "Initialize `sass-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; Sass (extension: ".scss")
     ;; -----------------------------------------------------------------------
     (use-package scss-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((scss-mode . my-scss-mode-initialize))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        ;;
        ;; コンパイルは常に手動（保存時は何もしない）
        ;; 各種ツール経由でコンパイルされうるため
        ;;
        '(scss-compile-at-save nil))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-scss-mode-initialize ()
         "Initialize `scss-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; SGML, (X)HTML
     ;; -----------------------------------------------------------------------
     (use-package sgml-mode
       ;; :disabled
       :defer t
       :mode (("\\.[sx]?html?\\'" . html-mode))
       :hook ((sgml-mode . my-sgml-mode-initialize)
              (html-mode . my-html-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       ;;
       ;; SGML
       ;;
       (defun my-sgml-mode-initialize ()
         "Initialize `sgml-mode' before file load."
         (setq-local indent-tabs-mode nil)

         (when (featurep 'sgml-electric-tag-pair-mode)
           (declare-function sgml-electric-tag-pair-mode "sgml-mode")
           (sgml-electric-tag-pair-mode +1))

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style))))))

       ;;
       ;; (X)HTML
       ;;
       (defun my-html-mode-initialize ()
         "Initialize `html-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; TeX
     ;; -----------------------------------------------------------------------
     (use-package tex-mode
       ;; :disabled
       :defer t
       :hook ((tex-mode . my-tex-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-tex-mode-initialize ()
         "Initialize `tex-mode' before file load."
         (setq-local truncate-lines nil)))


     ;; -----------------------------------------------------------------------
     ;; Text
     ;; -----------------------------------------------------------------------
     (use-package text-mode
       ;; :disabled
       :defer t
       :hook ((text-mode . my-tex-mode-initialize))
       :init
       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-text-mode-initialize ()
         "Initialize `text-mode' before file load."
         (setq-local truncate-lines nil)))


     ;; -----------------------------------------------------------------------
     ;; Vue.js
     ;; -----------------------------------------------------------------------
     (use-package vue-mode
       ;; :disabled
       :ensure t
       :defer t)


     ;; -----------------------------------------------------------------------
     ;; XML
     ;; -----------------------------------------------------------------------
     (use-package nxml-mode
       ;; :disabled
       :defer t
       :mode (("\\.xml\\'" . nxml-mode)
              ("\\.plist\\'" . nxml-mode))
       :hook ((nxml-mode . my-nxml-mode-initialize))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(nxml-child-indent 2)
        '(nxml-attribute-indent 0)
        '(nxml-slash-auto-complete-flag t)
        '(nxml-bind-meta-tab-to-complete-flag t)
        '(nxml-sexp-element-flag t)
        '(nxml-char-ref-display-glyph-flag t))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-nxml-mode-initialize ()
         "Initialize `nxml-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; YAML
     ;; -----------------------------------------------------------------------
     (use-package yaml-mode
       ;; :disabled
       :ensure t
       :defer t
       :mode (("\\.eslintrc\\'" . nxml-mode))
       :hook ((yaml-mode . my-yaml-mode-initialize))
       :init

       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(yaml-indent-offset 2))

       ;; -----------------------------
       ;; 初期化
       ;; -----------------------------
       (defun my-yaml-mode-initialize ()
         "Initialize `yaml-mode' before file load."
         (setq-local indent-tabs-mode nil)

         ;; EditorConfig 対応
         (eval-after-load 'editorconfig
           '(if (hash-table-p editorconfig-properties-hash)
                (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                       (indent-style (equal indent-style-data "tab")))
3                  (if (not (equal indent-tabs-mode indent-style))
                      (setq-local indent-tabs-mode indent-style)))))))


     ;; -----------------------------------------------------------------------
     ;; END OF CONFIG: 詳細設定（メジャーモード）
     ;; -----------------------------------------------------------------------
     ))


;; ============================================================================
;; 詳細設定（その他）
;; ============================================================================
(eval-after-load 'use-package
  '(progn
     ;; -----------------------------------------------------------------------
     ;; EWW (Emacs Web Wowser)
     ;; -----------------------------------------------------------------------
     (use-package eww
       ;; :disabled
       :defer t
       :bind (("C-c C-e" . eww))
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;; -----------------------------
       (custom-set-variables
        '(eww-search-prefix "https://www.google.co.jp/search?&q=")
        '(eww-history-limit 100)))


     ;; -----------------------------------------------------------------------
     ;; JavaScript 開発環境
     ;; -----------------------------------------------------------------------
     (use-package indium
       ;; :disabled
       :ensure t
       :demand t
       :hook ((js-mode . indium-interaction-mode)
              (js2-mode . indium-interaction-mode))
       :config
       ;; -----------------------------
       ;; lighter
       ;; -----------------------------
       (eval-after-load 'my-utils
         '(if (fboundp 'indium-interaction-mode)
              (my-change-lighter indium-interaction-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; URL ツール
     ;; -----------------------------------------------------------------------
     (use-package url
       ;; :disabled
       :defer t
       :init
       ;; -----------------------------
       ;; デフォルト値
       ;;------------------------------
       (custom-set-variables
        '(url-using-proxy t)))


     ;; -----------------------------------------------------------------------
     ;; END OF CONFIG: 詳細設定（その他）
     ;; -----------------------------------------------------------------------
     ))


;; ============================================================================
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
