;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-02-01T16:43:38+09:00>

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
;; (prefer-coding-system 'utf-8-unix) は絶対に使わないこと！
;; システムごとに最適化された、デフォルト定義（自動設定）の結果を破壊するため
;;
;; 他の設定は `00-02-coding.el' を参照
(set-coding-system-priority 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; macOS ONLY
(when (equal system-type 'darwin)
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
 ;; IM 設定 (Windows ONLY)
 ;;
 ;; 識別名 "W32-IME"' は、IME パッチが適用されていなければ使えない
 ;; 他環境ではデフォルトのままとする
 ;;
 `(default-input-method ,(cond ((or (equal system-type 'ms-dos)
                                    (equal system-type 'windows-nt))
                                "W32-IME")
                               (t
                                default-input-method)))
 ;;
 ;; バッファ毎の IME 状態モードライン表示 (Windows ONLY)
 ;;
 '(w32-ime-mode-line-state-indicator "[Aa]")
 ;;
 ;; IM 状態モードライン表示の一覧 (Windows ONLY)
 ;;
 ;; 順に「IME OFF」「IME ON: 日本語入力」「IME ON: 英字入力」
 ;;
 '(w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[Aa]"))
 ;;
 ;; バッファ切替時には IM 状態を引き継がない (Windows ONLY)
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
                               (cond ((or (equal system-type 'ms-dos)
                                          (equal system-type 'windows-nt))
                                      '("C:/programs/cygwin/usr/ssl/certs/ca-bundle.crt"))
                                     (t
                                      '("/usr/local/etc/libressl/cert.pem"
                                        "/usr/local/etc/openssl/cert.pem"
                                        "/etc/ssl/cert.pem"))))))


;; ============================================================================
;; IM 設定 (windows ONLY)
;; ============================================================================
(if (and (require 'w32-ime nil :noerror)
         (fboundp 'w32-ime-initialize))
    (w32-ime-initialize))


;; ============================================================================
;; 環境変数 (Windows ONLY)
;; ============================================================================
(if (or (equal system-type 'ms-dos)
        (equal system-type 'windows-nt))
    ;; 環境変数 PATH では不足している分の追加
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
  ;; --------------------------------------------------------------------------
  ;; 初期化
  ;; --------------------------------------------------------------------------
  ;; 確実に定義された後で追加
  (add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

  ;; あらゆるパッケージロードに先んじて記述しなければならない
  (package-initialize)

  ;; `list-packages' のような短縮版を用意
  (defalias 'list-packages-no-fetch 'package-list-packages-no-fetch))


;; ============================================================================
;; 詳細設定補助 (by `use-package')
;; ============================================================================
(eval-after-load 'package
  ;; パッケージマネージャ関連機能が、必ず使える状況を前提とする
  '(progn
     (if (not (package-installed-p 'use-package))
         (package-install 'use-package))

     (require 'use-package nil :noerror)))


;; ============================================================================
;; 詳細設定
;; ============================================================================
(eval-after-load 'use-package
  '(progn
     ;; -----------------------------------------------------------------------
     ;; マイナーモード
     ;; -----------------------------------------------------------------------
     ;; -------------------------------
     ;; 各種検索・置換強化
     ;; -------------------------------
     (use-package anzu
       ;; :disabled
       :ensure t
       :defer t
       :bind (("M-%" . anzu-query-replace)
              ("C-M-%" . anzu-query-replace-regexp))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(anzu-mode-lighter nil)
        '(anzu-minimum-input-length 3)
        '(anzu-search-threshold 1000)
        '(anzu-replace-to-string-separator " -> "))

       ;;
       ;; デフォルト値（`migemo' 利用可能時）
       ;;
       (eval-after-load 'migemo
         '(custom-set-variables
           '(anzu-use-migemo t)))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'global-anzu-mode)
           (global-anzu-mode +1)))


     ;; -------------------------------
     ;; アーカイブファイルを直接編集
     ;; -------------------------------
     (use-package jka-cmpr-hook
       ;; :disabled
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'auto-compression-mode)
           (auto-compression-mode +1)))


     ;; -------------------------------
     ;; 他ウインドウ弱調化
     ;;
     ;; see also:
     ;; `my-default-theme.el'
     ;; -------------------------------
     (use-package auto-dim-other-buffers
       ;; :disabled
       :ensure t
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (defun my-auto-dim-other-buffers-mode-initialize ()
         "Initialize `auto-dim-other-buffers-mode'."
         (if (fboundp 'auto-dim-other-buffers-mode)
             (auto-dim-other-buffers-mode +1)))

       (add-hook 'after-init-hook #'my-auto-dim-other-buffers-mode-initialize)
       :config
       ;;
       ;; lighter
       ;;
       (eval-after-load 'my-utils
         '(if (fboundp 'auto-dim-other-buffers-mode)
              (my-change-lighter auto-dim-other-buffers-mode nil))))


     ;; -------------------------------
     ;; 自動バッファ再読込
     ;; -------------------------------
     (use-package autorevert
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'global-auto-revert-mode)
           (global-auto-revert-mode +1)))


     ;; -------------------------------
     ;; ブックマーク
     ;; -------------------------------
     (use-package bookmark
       ;; :disabled
       :init
       (custom-set-variables
        '(bookmark-version-control t)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(bookmark-default-file ,(convert-standard-filename "~/.emacs.bookmark.el"))))


     ;; -------------------------------
     ;; ブックマーク (`bookmark') 拡張
     ;; -------------------------------
     (use-package bookmark+
       ;; :disabled
       :requires (bookmark)
       :ensure t)


     ;; -------------------------------
     ;; プログラマ向けネーミング辞書
     ;; -------------------------------
     (use-package codic
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-c C-q" . codic))
       :config
       ;;
       ;; HACK: 専用バッファをコマンドで `quit-window' させる
       ;;
       (unless (fboundp 'codic-view-kill)
         ;; 専用ウインドウを `quit-window' する関数が
         ;; 定義されていないなら、追加
         (defun my-codic-quit ()
           "Quit `codic' window and bury its buffer."
           (interactive)
           (with-current-buffer (current-buffer)
             (quit-window t)))

         ;; 専用バッファでキーバインドを有効にするため、アドバイスを利用
         ;; 専用 hook がないため
         (defun my-codic-local-set-key (items)
           "Set `local-set-key' for `codic' result buffer."
           (with-current-buffer "*Codic Result*"
             (if (fboundp 'codic-quit)
                 (local-set-key (kbd "q") #'my-codic-quit))))

         (if (fboundp 'codic--view)
             (advice-add 'codic--view
                         :after
                         #'my-codic-local-set-key))))


     ;; -------------------------------
     ;; 共通コマンドインタプリタ
     ;; -------------------------------
     (use-package comint
       ;; :disabled
       :if (or (equal system-type 'ms-dos)
               (equal system-type 'windows-nt))
       :defer t
       :init
       (custom-set-variables
        '(comint-scroll-to-bottom-on-input 'all)
        '(comint-move-point-for-output 'all)
        '(comint-buffer-maximum-size 5000)
        '(comint-process-echoes t)
        '(comint-eol-on-send t))

       ;;
       ;; プロセスごとのコーディングシステム変換表
       ;;
       ;; see also:
       ;; https://www.emacswiki.org/emacs/ShellMode#toc1
       ;;
       (add-to-list 'process-coding-system-alist
                    '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix)))

       ;;
       ;; 初期化
       ;;
       (defun my-comint-mode-initialize ()
         "Initialize `comint-mode' before file load."
         (if (boundp 'comint-input-sender-no-newline)
             (setq-local comint-input-sender-no-newline t)))

       (add-hook 'comint-mode-hook #'my-comint-mode-initialize))


     ;; -------------------------------
     ;; 補完フレームワーク
     ;; -------------------------------
     (use-package company
       ;; :disabled
       :ensure t
       :defer t
       :hook ((after-init-hook . global-company-mode))
       :init
       ;;
       ;; デフォルト値
       ;;
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


     ;; -------------------------------
     ;; 補完フレームワーク（補完候補のソート）
     ;; -------------------------------
     (use-package company-statistics
       ;; :disabled
       :requires (company)
       :ensure t
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(company-statistics-size 500)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(company-statistics-file ,(convert-standard-filename "~/.emacs.company-statistics-cache.el")))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'company-statistics-mode)
           (company-statistics-mode +1)))


     ;; -------------------------------
     ;; 補完フレームワーク（補完候補のポップアップドキュメント）
     ;; -------------------------------
     (use-package company-quickhelp
       ;; :disabled
       :requires (company)
       :ensure t
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(company-quickhelp-delay 0.25))

       ;;
       ;; 起動
       ;;
       (eval-after-load 'company-mode
         '(if (fboundp 'company-quickhelp-mode)
              (company-quickhelp-mode +1))))


     ;; -------------------------------
     ;; コンパイル
     ;; -------------------------------
     (use-package compile
       ;; :disabled
       :defer t
       :bind (("C-c C-l" . compile))
       :init
       ;;
       ;; デフォルト値
       ;;
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
         :init
         ;;
         ;; HACK: ウインドウの状態を問わず、常にリサイズをかける
         ;;
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

         ;;
         ;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
         ;;
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


         ;;
         ;; HACK: コンパイル完了後、
         ;;       ステータスに異常がなければ自動でウインドウを閉じる
         ;;
         (defcustom my-compilation-auto-quit-window-enable-buffer-name '("*compilation*")
           "TODO: ドキュメント書く"
           :group 'compilation
           :type '(list (repeat string)))

         ;; `process-status' と `exit-status' の値も得たいので、アドバイスを利用
         ;; `compilation-finish-functions' にフックした関数では `msg' しか
         ;; 参照できないため
         (defun my-compilation-auto-quit-window (process-status exit-status msg)
           "Run `quit-window' when `compile' successed."
           (if (and (member (buffer-name)
                            my-compilation-auto-quit-window-enable-buffer-name)
                    (or (and (equal process-status 'exit)
                             (zerop exit-status))
                        ;; 改行文字が含まれうる問題を回避
                        (string-equal "finished" (string-trim msg))))
               (quit-window nil (get-buffer-window))))

         (if (fboundp 'compilation-handle-exit)
             (advice-add 'compilation-handle-exit
                         :after
                         #'my-compilation-auto-quit-window))

         ;;
         ;; HACK: ANSI エスケープシーケンスが正しく解釈されない問題を回避
         ;;
         (defun my-ansi-color-apply-on-compilation ()
           "Recognize ASCII color escape sequences for `compilation-mode' buffer."
           (if (and (require 'ansi-color nil :noerror)
                    (fboundp 'ansi-color-apply-on-region))
               (let ((start-marker (make-marker))
                     (end-marker (process-mark (get-buffer-process (current-buffer)))))
                 (set-marker start-marker (point-min))
                 (ansi-color-apply-on-region start-marker end-marker))))

         (add-hook 'compilation-filter-hook #'my-ansi-color-apply-on-compilation)))


     ;; -------------------------------
     ;; 矩形選択
     ;; -------------------------------
     (use-package cua-base
       ;; :disabled
       :defer t
       :init
       (if (fboundp 'cua-selection-mode)
           ;;
           ;; 特殊キーバインド無効
           ;;
           (cua-selection-mode +1)))


     ;; -------------------------------
     ;; バッファ内マッチ補完
     ;; -------------------------------
     (use-package dabbrev
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
         ;; 補完時に大小文字を区別しない
        '(dabbrev-case-fold-search t)))


     ;; -------------------------------
     ;; デスクトップ環境保存・復旧
     ;; -------------------------------
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
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(desktop-save 'ask-if-new)
        '(desktop-load-locked-desktop t)
        '(desktop-missing-file-warning nil)
        ;; 必要最小限の情報のみ保存させる
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

       ;;
       ;; 起動
       ;;
       (if (fboundp 'desktop-save-mode)
           (desktop-save-mode +1)))


     ;; -------------------------------
     ;; ディレクトリブラウジング
     ;; -------------------------------
     (use-package dired
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       ;; TODO: ここのコードは `find-ls-option' や `find-exec-terminator' が
       ;;       未定義であるため、コメントアウトするとエラーになってしまう
       ;;       Windows ではクリティカルなので、必ず解決すること！
       ;;
       ;; (custom-set-variables
       ;;  ;; PATH は通っていないが、`exec-path' は通っている場合を想定
       ;;  ;; (Windows ONLY)
       ;;  `(find-ls-option ,(if (or (equal system-type 'ms-dos)
       ;;                            (equal system-type 'windows-nt))
       ;;                        (cons (format "-exec %s -ld {} %s"
       ;;                                      (executable-find "ls")
       ;;                                      find-exec-terminator)
       ;;                              "-ld")
       ;;                      find-ls-option)))

       ;;
       ;; 初期化
       ;;
       (defun my-dired-mode-initialize ()
         "Initialize `dired-mode'."
         ;; 常にすべての情報を表示（簡易モードにしない）
         (if (fboundp 'dired-hide-details-mode)
             (dired-hide-details-mode -1)))

       (add-hook 'dired-mode-hook #'my-dired-mode-initialize))


     ;; -------------------------------
     ;; ディレクトリブラウジング (`dired') 拡張
     ;; -------------------------------
     (use-package dired+
       ;; :disabled
       :requires (dired)
       :ensure t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(diredp-hide-details-initially-flag nil)
        '(diredp-hide-details-propagate-flag nil)))


     ;; -------------------------------
     ;; EditorConfig
     ;; -------------------------------
     (use-package editorconfig
       ;; :disabled
       :ensure t
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(editorconfig-mode-lighter ""))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'editorconfig-mode)
           (editorconfig-mode +1)))


     ;; -------------------------------
     ;; GNU Emacs Lisp ドキュメント表示
     ;; -------------------------------
     (use-package eldoc
       ;; :disabled
       :defer t
       :bind (("M-%" . anzu-query-replace)
              ("C-M-%" . anzu-query-replace-regexp))
       :hook ((lisp-mode-hook . eldoc-mode)
              (emacs-lisp-mode-hook . eldoc-mode)
              (lisp-interaction-mode-hook . eldoc-mode)
              (ielm-mode-hook . eldoc-mode))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(eldoc-minor-mode-string nil)
        '(eldoc-idle-delay 0.2)
        '(eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)))


     ;; -------------------------------
     ;; Emmet
     ;; -------------------------------
     (use-package emmet-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((html-mode-hook . emmet-mode))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(emmet-indentation 2)
        '(emmet-move-cursor-between-quotes t)))


     ;; -------------------------------
     ;; `text/enriched' フォーマットファイル
     ;; -------------------------------
     (use-package enriched
       ;; :disabled
       :defer t
       :config
       ;; PATCH: v25.3 未満に存在するセキュリティホールの Fix
       ;;
       ;; see also:
       ;; https://lists.gnu.org/archive/html/emacs-devel/2017-09/msg00211.html
       (if (or (< emacs-major-version 25)
               (and (= emacs-major-version 25)
                    (< emacs-minor-version 3)))
           (defun enriched-decode-display-prop (start end &optional param)
             (list start end))))

     ;; -------------------------------
     ;; カーソル下の数値を増減
     ;; -------------------------------
     (use-package evil-numbers
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-2" . evil-numbers/inc-at-pt)
              ("C-1" . evil-numbers/dec-at-pt)))

     ;; -------------------------------
     ;; GNU/Linux, UNIX, macOS 環境変数 $PATH 自動取得・設定
     ;; -------------------------------
     (use-package exec-path-from-shell
       ;; :disabled
       :ensure t
       :defer t
       :config
       (if (and (member window-system '(mac ns x))
                (fboundp 'exec-path-from-shell-initialize))
           (exec-path-from-shell-initialize)))


     ;; -------------------------------
     ;; デフォルト行文字数の位置にインジケータを表示
     ;;
     ;; see also:
     ;; `fill-column'
     ;; -------------------------------
     (use-package fill-column-indicator
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-c q" . evil-numbers/inc-at-pt))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        `(fci-rule-color ,(face-attribute 'font-lock-comment-face :foreground))
        '(fci-rule-use-dashes t)
        '(fci-dash-pattern 0.5)
        ;;
        ;; HACK: `fci-mode' を有効にした後、
        ;;       `toggle-truncate-lines' で折り返し表示を有効にすると
        ;;       `line-move-visual' が強制的に nil となる問題を回避
        ;;
        '(fci-handle-line-move-visual nil)))


     ;; -------------------------------
     ;; 自動静的解析
     ;; -------------------------------
     (use-package flycheck
       ;; :disabled
       :defer t
       :bind (("C-c f" . flycheck-mode))
       :hook ((after-init-hook . global-flycheck-mode))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(flycheck-checker-error-threshold nil)
        '(flycheck-display-errors-delay 0.5)
        '(flycheck-idle-change-delay 0.25)
        '(flycheck-mode-line "")
        '(flycheck-disabled-checkers '(javascript-jscs)))
       :config
       ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
       ;;       生成されうる警告バッファの出現を抑制
       (eval-after-load 'warnings
         '(if (boundp 'warning-suppress-log-types)
              (add-to-list 'warning-suppress-log-types
                           '(flycheck syntax-checker))))

       ;;
       ;; PATCH: ESLint 優先利用
       ;;
       ;; JSHint -> ESLint を ESLint -> JSHint 順に変更
       (if (boundp 'flycheck-checkers)
           (let* ((target-and-other-checkers (member 'javascript-eslint
                                                     flycheck-checkers)))
             (delete 'javascript-jshint flycheck-checkers)
             (setcdr target-and-other-checkers
                     (cons 'javascript-jshint
                           (cdr-safe target-and-other-checkers)))
             flycheck-checkers))

       ;;
       ;; PATCH: v.Nu サポート
       ;;
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
           (cond
            ;; デフォルトの (X)HTML チェッカ `html-tidy' と入れ替える
            (target-and-other-checkers
             (setcar target-and-other-checkers 'vnu))
            ;; 未追加ならリスト先頭に追加
            (t
             (add-to-list 'flycheck-checkers 'vnu)))))

       ;;
       ;; PATCH: Sass（.scss/.sass 両形式）チェック時にキャッシュを使わせない
       ;;
       (dolist (checker '(scss sass))
         (if (and (flycheck-registered-checker-p checker)
                  (not (member "-C" (flycheck-checker-arguments checker))))
             ;; あえて破壊的に変更（元のリストに追加したい）
             (nconc (get checker 'flycheck-command) '("-C"))))

       ;;
       ;; PATCH: temp ファイルのデフォルトコーディングシステムを、
       ;;        強制的に UTF-8 (LF) とする
       ;;
       (defun flycheck-save-buffer-to-file (file-name)
         "Save the contents of the current buffer to FILE-NAME."
         ;; 他の部分は元定義と一致させる
         (make-directory (file-name-directory file-name) t)
         ;; FIXME: もっと柔軟に設定できるようにならないか？
         (let ((coding-system-for-write 'utf-8-unix) ; ここだけ変更・決め打ち
               (jka-compr-inhibit t))
           (write-region nil nil file-name nil 0))))


     ;; -------------------------------
     ;; 自動静的解析モードライン変更
     ;; -------------------------------
     (use-package flycheck-color-mode-line
       ;; :disabled
       :requires (flycheck)
       :ensure t
       :defer t
       :hook ((flycheck-mode-hook . flycheck-color-mode-line-mode)))


     ;; -------------------------------
     ;; 自動静的解析 (OLD)
     ;; -------------------------------
     (use-package flymake
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(flymake-run-in-place nil))
       :config
       ;;
       ;; lighter
       ;;
       (eval-after-load 'my-utils
         '(if (fboundp 'flymake-mode)
              (my-change-lighter flymake-mode nil))))


     ;; -------------------------------
     ;; フレーム
     ;; -------------------------------
     (use-package frame
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;;
        ;; フレームサイズ変更を px 単位で実行できるようにする
        ;;
        '(frame-resize-pixelwise t))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'blink-cursor-mode)
           ;;
           ;; カーソルは点滅させない
           ;;
           (blink-cursor-mode -1)))


     ;; -------------------------------
     ;; OBSOLETE: フレーム位置・サイズ復元
     ;; -------------------------------
     ;; v24.4 (r113242) 以降では `desktop' に同一機能 `desktop-restore-frames'
     ;; が実装されている
     ;;
     ;; 条件分岐による起動制御を実施する
     ;; `frame-restore-mode' は自動検出して停止してくれるが、warning を吐くため
     ;;
     ;; see also:
     ;; http://bzr.savannah.gnu.org/lh/emacs/trunk/revision/113242
     ;; -------------------------------
     (use-package frame-restore
       ;; :disabled
       :if (and (= emacs-major-version 24)
                (< emacs-minor-version 4))
       :ensure t
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(frame-restore-parameters-file ,(convert-standard-filename "~/.emacs.frame-restore-parameters.el")))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'frame-restore-mode)
           (frame-restore-mode +1))
       :config
       (if (not noninteractive)
           ;; なるべく「最後」に実行されるよう調整
           ;; 他のフレーム状態変更機能と衝突させないため
           (when (fboundp 'frame-restore-save-parameters)
             (remove-hook 'kill-emacs-hook #'frame-restore-save-parameters)
             (add-hook 'kill-emacs-hook #'frame-restore-save-parameters t))))


     ;; -------------------------------
     ;; フレームセット
     ;; -------------------------------
     (use-package frameset
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 初期化
       ;;
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
                (setcdr (assoc key frameset-filter-alist) :never)))))

       ;; 全設定が完了してから実行しなければならない
       ;; 途中で追加される項目がありうるため
       (add-hook 'after-init-hook #'my-frameset-initialize))


     ;; -------------------------------
     ;; `grep'
     ;; -------------------------------
     (use-package grep
       ;; :disabled
       :defer (not (or (equal system-type 'ms-dos)
                       (equal system-type 'windows-nt)))
       :bind (("C-M-g" . rgrep))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;; 例外が出るため NUL デバイスは使わせない (Windows ONLY)
        `(grep-use-null-device (if (or (equal system-type 'ms-dos)
                                       (equal system-type 'windows-nt))
                                   nil
                                 grep-use-null-device)))

       ;;
       ;; 初期化 (Windows ONLY)
       ;;
       (when (or (equal system-type 'ms-dos)
                 (equal system-type 'windows-nt))
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
       ;;
       ;; PATCH: grep 2.21 から環境変数 `GREP_OPTIONS' が deprecated に
       ;;        なったため、v24.x までの `grep' では結果に warning が出る
       ;;        問題を回避
       ;;        v25.0 以降はパッチが適用されたため、何もしない
       ;;
       ;; HACK: `add-to-list' を用いている部分は、元コードのままとする
       ;;
       ;; see also:
       ;; https://lists.gnu.org/archive/html/info-gnu/2014-11/msg00009.html
       ;; https://lists.gnu.org/archive/html/emacs-diffs/2014-09/msg00134.html
       ;;
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
                      (grep-highlight-matches ,grep-highlight-matches))))))))))


     ;; -------------------------------
     ;; 拡張補完・展開
     ;; -------------------------------
     (use-package hippie-exp
       ;; :disabled
       :ensure t
       :defer t
       :bind (("M-/" . hippie-expand)))


     ;; -------------------------------
     ;; カレントカーソル行強調
     ;; -------------------------------
     (use-package hl-line
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 初期化
       ;;
       (defun my-hl-line-initialize ()
         "Initialize `hl-line' settings."
         (when (and (require 'color nil :noerror) ; 未ロードの場合がありうるため必須
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
                          (:inherit nil :background ,line-background-color)))))
             nil)))

       ;; FIXME: `after-init-hook' 後に実行した `load-theme' に対応したい
       ;;        `advice-add' で `enable-theme' の after 実行してもうまくいかない
       (add-hook 'after-init-hook #'my-hl-line-initialize)

       ;;
       ;; 起動
       ;;
       (if (fboundp 'global-hl-line-mode)
           (global-hl-line-mode +1)))


     ;; -------------------------------
     ;; 拡張バッファ一覧
     ;; -------------------------------
     (use-package ibuffer
       ;; :disabled
       :defer t
       :bind (("C-x C-b" . ibuffer))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(ibuffer-expert t))

       (eval-after-load 'ibuffer
         '(when (and (boundp 'ibuffer-formats)
                     (boundp 'ibuffer-sorting-mode)
                     (boundp 'ibuffer-last-sorting-mode)
                     (boundp 'ibuffer-sorting-reversep)
                     (boundp 'ibuffer-sorting-functions-alist))
            ;; バッファ名の表示を30文字に拡張
            ;; カラム幅が揃わなくなるため、-1 にはできない
            (let* (;; `customize-mark-to-save' の評価を t にするため、明示的にコピー
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
             '(ibuffer-default-sorting-mode 'mode-name-and-path-alphabetic)))))


     ;; -------------------------------
     ;; 拡張バッファ一覧 `projectile' 拡張
     ;; -------------------------------
     (use-package ibuffer-projectile
       ;; :disabled
       :requires (ibuffer projectile)
       :ensure t
       :defer t
       :init
       ;;
       ;; 初期化
       ;;
       (defun my-ibuffer-initialize ()
         "Initialize `ibuffer' settings."
         (if (fboundp 'ibuffer-projectile-set-filter-groups)
             (ibuffer-projectile-set-filter-groups)))

       (add-hook 'ibuffer-hook #'my-ibuffer-initialize))


     ;; -------------------------------
     ;; ファイル操作の簡略化
     ;; -------------------------------
     (use-package ido
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
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

       ;;
       ;; 起動
       ;;
       (if (fboundp 'ido-mode)
           (ido-mode +1))

       (eval-after-load 'ido
         '(if (fboundp 'ido-everywhere)
              (ido-everywhere +1))))


     ;; -------------------------------
     ;; 画像の直接表示
     ;; -------------------------------
     (use-package image-file
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'auto-image-file-mode)
           (auto-image-file-mode +1)))


     ;; -------------------------------
     ;; インクリメンタル検索
     ;; -------------------------------
     (use-package isearch
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;;
        ;; 検索時に大小文字を区別しない
        ;;
        '(isearch-case-fold-search t)
        ;;
        ;; 逆検索時に大小文字を区別しない
        ;;
        '(isearch-last-case-fold-search t)))


     ;; -------------------------------
     ;; LSP (Language Server Protocol) クライアント
     ;;
     ;; see also:
     ;; https://microsoft.github.io/language-server-protocol/
     ;; https://langserver.org/
     ;; -------------------------------
     (use-package lsp-mode
       ;; :disabled
       :ensure t
       :defer t
       :hook ((css-mode-hook . lsp)
              (html-mode-hook . lsp)
              (js-mode-hook . lsp)
              (js2-mode-hook . lsp)
              (json-mode-hook . lsp)
              (php-mode-hook . lsp)
              (sass-mode-hook . lsp)
              (scss-mode-hook . lsp)
              (sh-mode-hook . lsp)
              (vue-mode-hook . lsp))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(lsp-auto-guess-root t)
        '(lsp-restart 'ignore)
        '(lsp-prefer-flymake nil)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(lsp-session-file ,(convert-standard-filename "~/.emacs.lsp-session"))))


     ;; -------------------------------
     ;; LSP (Language Server Protocol) クライアント UI
     ;; -------------------------------
     (use-package lsp-ui
       ;; :disabled
       :requires (lsp-mode)
       :ensure t
       :hook ((lsp-after-open-hook . lsp-ui-flycheck-enable)))


     ;; -------------------------------
     ;; Git インターフェース
     ;; -------------------------------
     (use-package magit
       ;; :disabled
       :ensure t
       :defer t
       :bind (("C-x g" . magit-status))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;; カレントバッファを表示しているウインドウに表示させる
        '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)))


     ;; -------------------------------
     ;; OBSOLETE: 環境に依存しないフレーム状態復元
     ;; -------------------------------
     (use-package maxframe
       ;; :disabled
       :if (and (= emacs-major-version 24)
                (< emacs-minor-version 4))
       :ensure t
       :defer t
       :init
       ;;
       ;; 起動
       ;;
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
         ;;        現状 `frame-restore-save-parameters' 実行時のフレームサイズが
         ;;        最大化時のものになってしまっている
         (add-hook 'kill-emacs-hook #'restore-frame)))


     ;; -------------------------------
     ;; メニューバー
     ;; -------------------------------
     (use-package menu-bar
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'menu-bar-mode)
           ;;
           ;; 非表示
           ;;
           (menu-bar-mode -1)))


     ;; -------------------------------
     ;; ローマ字入力から日本語をインクリメンタル検索
     ;; -------------------------------
     (use-package migemo
       ;; ;; :disabled
       :ensure t
       :bind (:map isearch-mode-map
              ("C-c C-s" . migemo-isearch-toggle-migemo))
       :init
       ;;
       ;; デフォルト値
       ;;
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
                              (if (or (equal system-type 'ms-dos)
                                      (equal system-type 'windows-nt))
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

       ;;
       ;; 起動
       ;;
       (if (and (boundp 'migemo-command)
                migemo-command
                (boundp 'migemo-dictionary)
                (file-exists-p migemo-dictionary))
           (migemo-init)))


     ;; -------------------------------
     ;; NSM (Network Security Manager)
     ;; -------------------------------
     (use-package nsm
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;; ローカル環境にのみ保存
        `(nsm-settings-file ,(convert-standard-filename "~/.emacs.network-security.data"))))


     ;; -------------------------------
     ;; カーソルの移動履歴
     ;; -------------------------------
     (use-package point-undo
       ;; :disabled
       :ensure t
       :bind (("M-]" . point-undo)
              ("M-[" . point-redo)))


     ;; -------------------------------
     ;; 印刷 (PostScript)
     ;; -------------------------------
     (use-package ps-print
       ;; :disabled
       :defer t
       :bind (("C-c p b" . ps-print-buffer)
              ("C-c p f" . ps-print-buffer-with-faces))
       :init
       ;;
       ;; デフォルト値
       ;;
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

       ;;
       ;; 初期化
       ;;
       (defun my-ps-print-hook-listener ()
         "Initialize `ps-print' when load hook `ps-print-hook'."

         ;; FIXME: 長い行の右端が切れてしまう問題を解決しなければならない
         ;;        いちいち改行を（カレントバッファへ）明示的に入れる方法はナシで
         ;;        プリント前処理 temp バッファを作ればいいかもしれないが……？

         ;; 極限まで細くする
         (if (boundp 'ps-header-frame-alist)
             (setcdr (assoc 'border-width ps-header-frame-alist) 0.1)))

       (add-hook 'ps-print-hook #'my-ps-print-hook-listener))


     ;; -------------------------------
     ;; 印刷ユーティリティ
     ;; -------------------------------
     (use-package printing
       ;; :disabled
       :bind (("C-c p p" . pr-interface))
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'pr-update-menus)
           ;;
           ;; メニューに項目追加
           ;;
           (pr-update-menus +1)))


     ;; -------------------------------
     ;; 汎用プロジェクト管理
     ;; -------------------------------
     (use-package projectile
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(projectile-enable-caching t)
        '(projectile-completion-system (cond ((featurep 'ido) 'ido)
                                             ((featurep 'ivy) 'ivy)
                                             ((featurep 'helm) 'helm)
                                             (t 'default)))
        '(projectile-mode-line-prefix "")
        '(projectile-keymap-prefix (kbd "C-c C-p"))
        ;; ローカル環境にのみ保存
        `(projectile-cache-file ,(convert-standard-filename "~/.emacs.projectile.cache"))
        `(projectile-known-projects-file ,(convert-standard-filename "~/.emacs.projectile-bookmarks.eld"))))


     ;; -------------------------------
     ;; 自動カラー表示
     ;; -------------------------------
     (use-package rainbow-mode
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (eval-after-load 'rainbow-mode
         '(when (boundp 'rainbow-html-colors-major-mode-list)
            (add-to-list 'rainbow-html-colors-major-mode-list 'sass-mode)
            (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode)
            (add-to-list 'rainbow-html-colors-major-mode-list 'less-mode)))
       :config
       ;;
       ;; lighter
       ;;
       (eval-after-load 'my-utils
         '(if (fboundp 'rainbow-mode)
              (my-change-lighter rainbow-mode nil))))


     ;; -------------------------------
     ;; ファイル履歴保存
     ;; -------------------------------
     (use-package recentf
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;;
        ;; すべての履歴を保存
        ;;
        '(recentf-max-saved-items nil)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(recentf-save-file ,(convert-standard-filename "~/.emacs.recentf.el"))))


     ;; -------------------------------
     ;; ミニバッファの履歴を残す
     ;; -------------------------------
     (use-package savehist
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;;
        ;; すべての履歴を保存
        ;;
        '(history-length t)
        ;;
        ;; ローカル環境にのみ保存
        ;;
        `(savehist-file ,(convert-standard-filename "~/.emacs.savehist.el")))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'savehist-mode)
           (savehist-mode +1)))


     ;; -------------------------------
     ;; ファイルごとにカーソル位置を保存
     ;; -------------------------------
     (use-package saveplace
       ;; :disabled
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;; ローカル環境にのみ保存
        `(save-place-file ,(convert-standard-filename "~/.emacs.saveplace.el")))

       ;;
       ;; 起動
       ;;
       (setq-default save-place t))


     ;; -------------------------------
     ;; スクロールバー
     ;; -------------------------------
     (use-package scroll-bar
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       ;; v24.x 以前
       (eval-after-load 'scroll-bar
         ;;
         ;; ウインドウシステム上では、あらゆるスクロールバーを非表示化
         ;;
         '(set-scroll-bar-mode (if window-system nil 'right)))

       ;; v25.x 以降
       (defun my-scroll-bar-initilalize ()
         "Initialize `scroll-bar' settings."
         (eval-after-load 'scroll-bar
           '(when window-system
              (if (fboundp 'scroll-bar-mode)
                  (scroll-bar-mode -1))
              (if (fboundp 'horizontal-scroll-bar-mode)
                  (horizontal-scroll-bar-mode -1)))))

       ;; このタイミングで実行しないと適用されない問題がある
       (add-hook 'after-init-hook #'my-scroll-bar-initilalize))


     ;; -------------------------------
     ;; サーバ化
     ;; -------------------------------
     ;; Windows 環境では `server-auth-dir' の「所有者」が：
     ;;   * Administrator (RID=500)
     ;;   * Administrators (RID=544)
     ;; である場合、`server-ensure-safe-dir' の評価が `nil' になる
     ;;
     ;; `server-auth-dir' で指定したフォルダの
     ;; 「プロパティ」→「セキュリティ」→「詳細設定」→「所有者」→「編集」
     ;; から、所有者をログオンユーザ自身に変更すること
     ;; -------------------------------
     ;; Windows 環境では emacsclientw.exe 実行時に環境変数
     ;; %EMACS_SERVER_FILE% でサーバファイルのパスを明示しなければならない
     ;; （なぜ必要かは不明）
     ;;
     ;; この欠点をある程度回避した wemacs.cmd を用いること
     ;; -------------------------------
     (use-package server
       ;; :disabled
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;; ローカル環境にのみ保存
        `(server-auth-dir ,(convert-standard-filename "~/.emacs.server")))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'server-force-delete)
           (server-force-delete))
       (if (fboundp 'server-start)
           (server-start)))


     ;; -------------------------------
     ;; 基礎編集コマンド集
     ;; -------------------------------
     (use-package simple
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'transient-mark-mode)
           ;;
           ;; 暫定マークを使用
           ;;
           (transient-mark-mode +1)))


     ;; -------------------------------
     ;; 各種カッコ関連機能拡張
     ;; -------------------------------
     (use-package smartparens
       ;; :disabled
       :ensure t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(sp-autoinsert-quote-if-followed-by-closing-pair t)
        '(sp-undo-pairs-separately t)
        '(sp-show-pair-from-inside t))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'smartparens-global-mode)
           (smartparens-global-mode +1))
       (if (fboundp 'show-smartparens-global-mode)
           (show-smartparens-global-mode +1))
       :config
       ;;
       ;; lighter
       ;;
       (eval-after-load 'my-utils
         '(if (fboundp 'smartparens-mode)
              (my-change-lighter smartparens-mode nil))))


     ;; -------------------------------
     ;; タイムスタンプ記述
     ;; -------------------------------
     (use-package time-stamp
       ;; :disabled
       :ensure t
       :defer t
       :hook ((before-save-hook . time-stamp))
       :init
       ;;
       ;; デフォルト値
       ;;
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
                    ;; 厳密なチェックにより "±1259" 形式のみ対象にする（他は無視）
                    "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
                    "\\1:\\2"
                    ;; タイムゾーンが UTC でも "Z" でなく "+0000" を返してくる
                    ;; 今のところ、あえて "Z" への変換はしないでおく
                    (format-time-string "%z"))))))


     ;; -------------------------------
     ;; ツールバー
     ;; -------------------------------
     (use-package tool-bar
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'tool-bar-mode)
           ;;
           ;; 非表示
           ;;
           (tool-bar-mode -1)))


     ;; -------------------------------
     ;; ツールチップ
     ;; -------------------------------
     (use-package tooltip
       ;; :disabled
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'tooltip-mode)
           ;;
           ;; 非表示
           ;;
           (tooltip-mode -1)))


     ;; -------------------------------
     ;; TRAMP (Transparent Remote Access, Multiple Protocols)
     ;; -------------------------------
     (use-package tramp
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        `(tramp-persistency-file-name ,(convert-standard-filename "~/.emacs.tramp"))))


     ;; -------------------------------
     ;; `undo' 拡張、`redo' 機能追加ならびに分岐履歴実装
     ;; -------------------------------
     (use-package undo-tree
       ;; :disabled
       :ensure t
       :bind (("C-." . undo-tree-redo))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(undo-tree-mode-lighter ""))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'global-undo-tree-mode)
           (global-undo-tree-mode)))


     ;; -------------------------------
     ;; `undo' 履歴の記憶
     ;; -------------------------------
     (use-package undohist
       ;; :disabled
       :ensure t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;; ローカル環境にのみ保存
        `(undohist-directory ,(convert-standard-filename "~/.emacs.undohist")))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'undohist-initialize)
           (undohist-initialize)))


     ;; -------------------------------
     ;; ファイル名を元に、より唯一性の高いバッファ名を生成
     ;; -------------------------------
     (use-package uniquify
       ;; :disabled
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(uniquify-buffer-name-style 'forward)
        '(uniquify-ignore-buffers-re "^*[^*]+*\\-")))


     ;; -------------------------------
     ;; URL ツール
     ;; -------------------------------
     (use-package url
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        '(url-using-proxy t)))


     ;; -------------------------------
     ;; 空白文字強調
     ;; -------------------------------
     (use-package whitespace
       ;; :disabled
       :defer t
       :init
       ;;
       ;; デフォルト値
       ;;
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

       (custom-set-faces
        ;; フェイス強調無効化
        '(whitespace-space ((t
                             (:background nil)))))

       ;;
       ;; 起動
       ;;
       (if (fboundp 'global-whitespace-mode)
           (global-whitespace-mode +1))
       :config
       ;;
       ;; lighter
       ;;
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

       ;;
       ;; HACK: 全角空白 (U+3000) を HARD SPACE とみなして強調表示
       ;;
       ;; 表示テスト:
       ;;   U+0009: 「	」
       ;;   U+00A0: 「 」
       ;;   U+3000: 「　」
       ;;
       (when (and (boundp 'whitespace-style)
                  (boundp 'whitespace-display-mappings))
         (custom-set-variables
          ;; 空白の強調を明示
          `(whitespace-style ',(let ((styles (copy-tree whitespace-style)))
                                 ;; HARD SPACE の ON/OFF も含んでいる
                                 (add-to-list 'styles 'spaces)
                                 (add-to-list 'styles 'space-mark)))
          ;; 検索条件を追加
          '(whitespace-hspace-regexp "\\(\\(\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\|\x3000\\)+\\)")
          '(whitespace-trailing-regexp "\\([\t \u00A0\u3000]+\\)$"))

         ;; 表示置換条件を追加
         (add-to-list 'whitespace-display-mappings
                      '(space-mark ?\u3000 [?\u25a1] [?_ ?_])))

       ;;
       ;; HACK: 半角空白 (U+0020) を強調しないようにする
       ;;
       ;; 表示テスト:
       ;;   U+0020: 「 」
       ;;
       (if (boundp 'whitespace-display-mappings)
           ;; 表示置換しないようにする
           (custom-set-variables
            `(whitespace-display-mappings ',(delete '(space-mark ?\  [?\u00B7] [?.])
                                                    whitespace-display-mappings)))))


     ;; -------------------------------
     ;; ウインドウ移動キーを直感的にする
     ;; -------------------------------
     (use-package windmove
       ;; :disabled
       :defer t
       :bind (("C-S-b" . windmove-left)
              ("C-S-f" . windmove-right)
              ("C-S-p" . windmove-up)
              ("C-S-n" . windmove-down))
       :init
       ;;
       ;; デフォルト値
       ;;
       (custom-set-variables
        ;;
        ;; フレーム端のウインドウでは無限スクロールするようにふるまう
        ;; 「マリオブラザーズ」左右画面端におけるループのような動き
        ;;
        '(windmove-wrap-around t)))


     ;; -------------------------------
     ;; ウインドウの状態履歴を undo/redo
     ;; -------------------------------
     (use-package winner
       ;; :disabled
       :ensure t
       :defer t
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'winner-mode)
           (winner-mode +1)))


     ;; -------------------------------
     ;; スニペット挿入
     ;; -------------------------------
     (use-package yasnippet
       ;; :disabled
       :ensure t
       :defer t
       :bind (("M-%" . anzu-query-replace)
              ("C-M-%" . anzu-query-replace-regexp))
       :init
       ;;
       ;; 起動
       ;;
       (if (fboundp 'yas-global-mode)
           (yas-global-mode +1))
       :config
       ;;
       ;; lighter
       ;;
       (eval-after-load 'my-utils
         '(if (fboundp 'yas-minor-mode)
              (my-change-lighter yas-minor-mode nil))))


     ;; -----------------------------------------------------------------------
     ;; メジャーーモード
     ;; -----------------------------------------------------------------------
     ;; -------------------------------
     ;; TODO: ここに説明を書いていく
     ;; -------------------------------
     ))


;; ============================================================================
;; 詳細設定ロード (by `init-loader')
;; ============================================================================
;; TODO: `use-package' を用いた「詳細設定」のほうに移していく
;; ============================================================================
(custom-set-variables
 '(init-loader-directory (locate-user-emacs-file "inits"))
 '(init-loader-show-log-after-init 'error-only)
 ;; 設定ファイルのバイトコンパイルは非推奨
 ;;
 ;; see also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
 '(init-loader-byte-compile nil))

(eval-after-load 'package
  ;; パッケージマネージャ関連機能が、必ず使える状況を前提とする
  '(progn
     (if (not (package-installed-p 'init-loader))
         (package-install 'init-loader))

     ;; 起動
     (if (and (require 'init-loader nil :noerror)
              (fboundp 'init-loader-load))
         (init-loader-load))))


;; ============================================================================
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
