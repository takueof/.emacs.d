;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-02-01T00:50:44+09:00>

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
 '(auto-save-list-file-prefix (convert-standard-filename "~/.emacs.auto-save-list/.saves-")) ; ローカル環境化
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
 `(browse-url-browser-function ,(cond ((equal window-system 'w32)
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
     ;; ローマ字入力から日本語をインクリメンタル検索
     ;; -------------------------------
     (use-package migemo
       ;; :disabled t
       :ensure t
       :bind (:map isearch-mode-map
              ("C-c C-s" . #'migemo-isearch-toggle-migemo))
       :init
       (custom-set-variables
        ;; C/Migemo 利用設定
        `(migemo-command ,(executable-find "cmigemo"))
        '(migemo-options '("-q" "--emacs"))
        ;; 空白文字と認識させる対象を広げる
        '(migemo-white-space-regexp "[[:space:]\s-]*")
        ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
        `(migemo-directory ,(convert-standard-filename "~"))
        ;; `migemo' 側で定義されている `isearch' 関連キーバインドを使わせない
        ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
        '(migemo-use-default-isearch-keybinding nil)
        ;; 辞書ファイルはデフォルトのものを利用
        `(migemo-dictionary ,(convert-standard-filename
                              (cond ((or (equal system-type 'ms-dos)
                                         (equal system-type 'windows-nt))
                                     "C:/programs/cmigemo/share/migemo/utf-8/migemo-dict")
                                    (t
                                     "/usr/local/share/migemo/utf-8/migemo-dict"))))
        '(migemo-user-dictionary nil)
        '(migemo-regex-dictionary nil)
        ;; 辞書エンコーディングを明示
        '(migemo-coding-system 'utf-8-unix)
        ;; キャッシュを使わせる
        '(migemo-use-pattern-alist t)
        '(migemo-use-frequent-pattern-alist t)
        '(migemo-pattern-alist-length 1024)
        ;; ローカル環境にのみ保存
        '(migemo-pattern-alist-file (expand-file-name ".emacs.migemo-pattern" migemo-directory))
        '(migemo-frequent-pattern-alist-file (expand-file-name ".emacs.migemo-frequent" migemo-directory)))
       :config
       (if (and (boundp 'migemo-command)
                migemo-command
                (boundp 'migemo-dictionary)
                (file-exists-p migemo-dictionary))
           (migemo-init)))


     ;; -------------------------------
     ;; TODO: ここに説明を書いていく
     ;; -------------------------------
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
