;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2022 Taku Watabe
;; Time-stamp: <2022-05-12T15:03:48+09:00>

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

;; This config file can use "GNU Emacs" ONLY,
;; unsupported other "emacsen" ("XEmacs" and others).
;;
;; This file is VERY LONG.
;; So, I DARE USE file local variables in the FIRST LINE.
;;
;; Show initialization time:
;; (emacs-init-time)

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
;; See also:
;; `mule-conf.el'
(define-coding-system-alias 'utf-8-bom 'utf-8-with-signature)

;; `japanese-cp932' を `shift_jis' として強制認識
;;
;; MIME を用いた自動エンコーディング判定を行うコード（`sgml-mode' など）でも
;; 例外が出ないようにする
(coding-system-put 'japanese-cp932
                   :mime-charset 'shift_jis)

;; `japanese-shift-jis' を Microsoft Code Page 932 (`japanese-cp932') に変更
;;
;; GNU Emacs における Shift_JIS 実装 `japanese-shift-jis' は、
;; JIS X 0208 附属書1にある定義を厳格に実装したもの
;; ゆえに、一部文字（例：「～」(U+FF5E)）が未定義であるなどし、
;; 実用上問題が発生しやすい
;;
;; そこで、タイムスタンプ時点で最も普及している Microsoft の Shift_JIS 実装
;; Microsoft Code Page 932 (`japanese-cp932') を、
;; デフォルトの Shift_JIS 実装として認識させる
;;
;; See also:
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


;; ============================================================================
;; 各ユーティリティをロード
;; ============================================================================
(require 'my-utils nil :noerror) ; 共通
(require 'my-fonts nil :noerror) ; フォント
(require 'my-modeline nil :noerror) ; モードライン


;; ============================================================================
;; グローバルキーバインド
;; ============================================================================
;; <Backspace> と <DEL> を 交換
(keyboard-translate ?\C-h ?\C-?)

;; <DEL> を <C-d> にする
(keyboard-translate ?\C-? ?\C-d)

;; `ido-undo-merge-work-directory' 実行のため <C-z> を押しすぎた場合、
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

;; タッチパッドによる各種操作を無効化 (macOS ONLY)
(when (member system-type '(darwin))
  (global-unset-key [magnify-up])
  (global-unset-key [magnify-down])
  (global-unset-key [S-magnify-up])
  (global-unset-key [S-magnify-down]))

;; 独自定義
(with-eval-after-load 'my-utils
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
  (global-set-key (kbd "C-c RET f") #'my-change-files-coding-system))


;; ============================================================================
;; ANSI エスケープシーケンス
;; ============================================================================
;; `comint-mode' および派生モードで、ANSI エスケープシーケンスの解釈を
;; 開始させる。
(if (fboundp 'ansi-color-for-comint-mode-on)
    (ansi-color-for-comint-mode-on))


;; ============================================================================
;; インプットメソッド
;; ============================================================================
;; WARNING: `window-system' 外の環境（例：ターミナルエミュレータ）では例外発生
;;          `window-system' 外の環境は除外することで回避
;; ============================================================================
(when window-system
  ;; 各種シンボル（`current-input-method' など）が `mule-cmds' で定義されている
  ;; 例外を出さず、確実に初期化する
  (with-eval-after-load "mule-cmds" ; 未 `provide'
    ;; ------------------------------------------------------------------------
    ;; インプットメソッド切替時に、フェイス `cursor' を変更
    ;; ------------------------------------------------------------------------
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
      (let ((current-input-method (if (fboundp 'mac-input-source)
                                      (let ((input-source (mac-input-source)))
                                        (if (numberp (string-match "\\.US\\'" input-source))
                                            nil
                                          input-source))
                                    current-input-method)))
        (set-cursor-color
         (face-attribute (if current-input-method
                             'my-cursor-input-method-activated
                           'my-cursor-default)
                         :background))))

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
 ;; 起動時に発生するガベージコレクトを防止
 ;;
 `(gc-cons-threshold ,(* 128 1024 1024)) ;; 128MB
 ;;
 ;; フレームタイトルはカレントバッファ名を基準にする
 ;;
 '(frame-title-format (format "%%b - emacs %s" emacs-version))
 ;;
 ;; スタートアップ表示は一切させない
 ;;
 ;; See also:
 ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
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
 ;; プレビューウインドウの表示を即時にする
 ;;
 '(register-preview-delay nil)
 ;;
 ;; スクロール時、自動スクロールをアグレッシブにする
 ;;
 ;; See also:
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
 ;; See also:
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
 ;; 証明書
 ;;
 `(gnutls-trustfiles ',(mapcar 'convert-standard-filename
                               (if (member system-type '(ms-dos windows-nt))
                                   '("C:/programs/cygwin/usr/ssl/certs/ca-bundle.crt")
                                 '("/usr/local/etc/libressl/cert.pem"
                                   "/usr/local/etc/openssl/cert.pem"
                                   "/private/etc/ssl/cert.pem"
                                   "/etc/ssl/cert.pem")))))


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
                    "C:/programs/Ghostgum/gsview"
                    "C:/programs/cygwin/bin")))
      (dolist (path paths)
        (setq path (convert-standard-filename path))
        (if (and (file-exists-p path)
                 (file-directory-p path))
            (add-to-list 'exec-path path)))))


;; ============================================================================
;; NSM (Network Security Manager)
;; ============================================================================
;; WARNING: `package' といったネットワークセキュリティを利用するパッケージの
;;          利用前に設定を実施しなければならない
;;          後のほうで `nsm-settings-file' を設定してしまうと意味がない
;; ============================================================================
;; HACK: `require' しておかないと、
;;       なぜか `custom-set-variables' が効かない
;; ============================================================================
(when (and (require 'nsm nil :noerror)
           (boundp 'nsm-settings-file))
  (custom-set-variables
   ;; ローカル環境にのみ保存
   '(nsm-settings-file "~/.emacs.network-security.data")))


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

  ;; あらゆるパッケージロードに先んじての記述が必須
  (package-initialize)

  ;; `list-packages' のような短縮版を用意
  (defalias 'list-packages-no-fetch 'package-list-packages-no-fetch))


;; ============================================================================
;; 詳細設定補助 (by `leaf')
;;
;; `package' が必ず使える状況を前提とする
;; ============================================================================
;; WARNING: `package' が必ず使える状況を前提とする
;;          `package' の初期化より後に設定しなければならない
;; ============================================================================
(when (and (fboundp 'package-installed-p)
           (fboundp 'package-refresh-contents)
           (fboundp 'package-install)
           (not (package-installed-p 'leaf)))
  (package-refresh-contents)
  (package-install 'leaf))


;; ============================================================================
;; 詳細設定
;;
;; See also:
;; https://github.com/conao3/leaf.el
;; https://qiita.com/conao3/items/dc88bdadb0523ef95878
;; ============================================================================
;; WARNING: `leaf' が必ず使える状況を前提とする
;;          `leaf' のインストールより後に設定しなければならない
;; ============================================================================
(when (fboundp 'leaf)
  ;; ==========================================================================
  ;; `leaf' キーワード群
  ;; ==========================================================================
  (leaf leaf-keywords
    :package t
    :config
    (if (fboundp 'leaf-keywords-init)
        (leaf-keywords-init)))


  ;; ==========================================================================
  ;; パッケージマネージャ (by `el-get')
  ;; ==========================================================================
  ;; WARNING: 全てのパッケージに影響するため、
  ;;          なるべく早いタイミングでインストールするようにしてある
  ;; ==========================================================================
  (leaf el-get
    :package t
    :custom `((el-get-git-shallow-clone . t)))


  ;; ==========================================================================
  ;; IME patch
  ;; ==========================================================================
  ;; WARNING: 全てのパッケージに影響するため、
  ;;          なるべく早いタイミングでインストールするようにしてある
  ;; ==========================================================================
  (leaf tr-ime
    :when (member system-type '(ms-dos windows-nt))
    :package t
    :config
    (if (fboundp 'tr-ime-advanced-install)
        (tr-ime-advanced-install))
    (if (fboundp 'w32-ime-initialize)
        (w32-ime-initialize)))


  ;; ==========================================================================
  ;; カラーテーマ
  ;; ==========================================================================
  (leaf *themes
    :custom `(;; For "Modus"
              (modus-themes-bold-constructs . t)
              (modus-themes-syntax . '(faint alt-syntax green-strings yellow-comments))
              (modus-themes-mode-line . '(moody accented borderless))
              (modus-themes-paren-match . '(bold intense))
              (modus-themes-region . '(accented bg-only no-extend)))
    :config
    ;; 利用可能なカラーテーマを設定
    (let ((required-themes '(;; 利用したいカラーテーマの一覧
                             ;; 優先度が高い順に降順ソートしておくこと
                             modus-vivendi
                             my-default
                             wheatgrass))
          (availabled-themes (custom-available-themes)))
      ;; 利用したいカラーテーマが見つからなければ何もしない
      (catch 'required-theme-found
        (dolist (theme required-themes)
          (when (member theme availabled-themes)
            (load-theme theme t)
            (throw 'required-theme-found theme))))))


  ;; ==========================================================================
  ;; 未分類パッケージ（非メジャー／マイナーモード）
  ;; ==========================================================================
  (leaf *packages
    :config
    ;; ------------------------------------------------------------------------
    ;; Node.js モジュールパス解決
    ;; ------------------------------------------------------------------------
    (leaf add-node-modules-path
      :package t
      :hook (prog-mode-hook . my-add-node-modules-path-initialize)
      :init
      (defun my-add-node-modules-path-initialize ()
        "Initialize `add-node-modules-path'."
        (if (fboundp 'add-node-modules-path)
            (add-node-modules-path))))


    ;; ------------------------------------------------------------------------
    ;; 未コミット diff
    ;; ------------------------------------------------------------------------
    (leaf diff-hl
      :package t
      :config
      (if (fboundp 'global-diff-hl-mode)
          (global-diff-hl-mode)))

    ;; ------------------------------------------------------------------------
    ;; EWW (Emacs Web Wowser, Web Browser)
    ;; ------------------------------------------------------------------------
    (leaf eww
      :bind (("C-c C-e" . eww))
      :custom `((eww-search-prefix . "https://www.google.co.jp/search?&q=")
                (eww-history-limit . 100)))


    ;; ------------------------------------------------------------------------
    ;; GNU/Linux, UNIX, macOS 環境変数 $PATH 自動取得・設定
    ;; ------------------------------------------------------------------------
    (leaf exec-path-from-shell
      :when (member window-system '(mac ns x))
      :package t
      :require t
      :config
      (if (fboundp 'exec-path-from-shell-initialize)
          (exec-path-from-shell-initialize)))


    ;; ------------------------------------------------------------------------
    ;; スペルチェッカ
    ;; ------------------------------------------------------------------------
    (leaf ispell
      :custom `((ispell-dictionary . "english")
                (ispell-extra-args . '("--sug-mode=fast"
                                       "--run-together"
                                       "--run-together-limit=5"
                                       "--run-together-min=2"))))


    ;; ------------------------------------------------------------------------
    ;; Git インターフェース
    ;; ------------------------------------------------------------------------
    (leaf magit
      :package t
      :bind (("C-x g" . magit-status))
      :custom `(;; カレントバッファを表示しているウインドウに表示させる
                (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1)))


    ;; ------------------------------------------------------------------------
    ;; nvm 経由での Node.js 利用をサポート
    ;; ------------------------------------------------------------------------
    (leaf nvm
      :package t
      :config
      (if (fboundp 'nvm-use-for)
          ;; `~/.nvmrc' がなければ何もしない
          (ignore-errors (nvm-use-for))))


    ;; ------------------------------------------------------------------------
    ;; PostScript（印刷）
    ;; ------------------------------------------------------------------------
    (leaf ps-print
      :bind (("C-c p p" . ps-print-buffer-with-faces))
      :hook ((ps-print-hook . my-ps-print-hook-listener))
      :custom `(;; エラーは紙媒体・システム両方に送信
                (ps-error-handler-message . 'paper-and-system)
                ;; 用紙は A4 サイズ・縦・回転なし
                (ps-paper-type . 'a4)
                (ps-landscape-mode . nil)
                (ps-print-upside-down . nil)
                ;; 全ページを印刷
                (ps-selected-pages . nil)
                (ps-even-or-odd-pages . nil)
                ;; コントロール文字を認識させない（非 ASCII 文字対策）
                (ps-print-control-characters . nil)
                ;; 用紙1枚につき1ページを印刷
                (ps-n-up-printing . 1)
                ;; シートの罫線とページの間に余白を設ける
                (ps-n-up-margin . ,(/ (* 72 1.00) 2.54)) ; 10.0mm
                ;; 行番号を印刷させる
                (ps-line-number . t)
                (ps-line-number-step . 1)
                (ps-line-number-start . 1)
                ;; 水平レイアウト
                (ps-left-margin . ,(/ (* 72 1.40) 2.54)) ; 14.0mm（行番号が切れないようにする）
                (ps-inter-column . ,(/ (* 72 1.00) 2.54)) ; 10.0mm
                (ps-right-margin . ,(/ (* 72 0.54) 2.54)) ; 5.4mm（ヘッダ・フッタの box 右端が切れないようにする）
                ;; 垂直レイアウト
                (ps-bottom-margin . ,(/ (* 72 0.55) 2.54)) ; 5.5mm（フッタ box 下端が切れないようにする）
                (ps-top-margin . ,(/ (* 72 2.95) 2.54)) ; 29.5mm (24.0mm + 5.5mm)（ヘッダ box 上端が切れないようにする・用紙上端～ヘッダ box 上端までの余白を、フッタ box 下端～用紙下端までの長さと同一にする）
                (ps-header-offset . 0) ; なし
                (ps-header-line-pad . 0.15)
                (ps-footer-offset . ,(/ (* 72 0.42) 2.54)) ; 4.2mm (0.5mm + 3.7mm)（フッタ box 上端へカブらないようにする・テキスト box 下端～フッタ box 上端までの余白を、ヘッダ box 下端～テキスト box 上端までの長さと同一にする）
                (ps-footer-line-pad . 0.15)
                ;; ヘッダに情報を追加させる
                (ps-print-header . t)
                (ps-print-header-frame . t)
                (ps-header-lines . 2)
                (ps-left-header . '(ps-get-buffer-name
                                    ps-header-dirpart))
                (ps-right-header . '(ps-time-stamp-yyyy-mm-dd
                                     ps-time-stamp-hh:mm:ss))
                ;; フッタに情報を追加させる
                (ps-print-footer . t)
                (ps-footer-lines . 1)
                (ps-print-footer-frame . t)
                (ps-left-footer . nil)
                (ps-right-footer . '("/pagenumberstring load"))
                ;; monospace なフォントを用いさせる
                (ps-font-family . 'Courier)
                (ps-font-size . '(8.0 . 8.0)) ; pt
                (ps-header-font-family . 'Courier)
                (ps-header-font-size . '(8.0 . 8.0)) ; pt
                (ps-header-title-font-size . '(8.0 . 8.0)) ; pt
                (ps-footer-font-family . 'Courier)
                (ps-footer-font-size . '(8.0 . 8.0)) ; pt
                (ps-line-number-font . "Courier") ; "Times-Italic"
                (ps-line-number-font-size . 6.0) ; pt
                ;; カラー印刷を有効にする
                (ps-print-color-p . t)
                (ps-default-fg . t)
                (ps-default-bg . t)
                (ps-use-face-background . t)
                ;; 行間を少し空けさせる
                (ps-line-spacing . 2.0)
                ;; 段落間は空けさせない
                (ps-paragraph-spacing . 0)
                ;; Latin-1 外の文字（日本語など）を印刷できるようにする
                ;; フォント一覧に `ps-mule-font-info-database-default' を
                ;; 使うよう強制
                (ps-multibyte-buffer . nil))
      :init
      (defun my-ps-print-hook-listener ()
        "Initialize `ps-print' when load `ps-print-hook'."
        ;; 極限まで細くする
        (if (boundp 'ps-header-frame-alist)
            (setcdr (assoc 'border-width ps-header-frame-alist) 0.1)))
      :config
      ;;----------------------------
      ;; WARNING: `ps-lpr-command' と `ps-lpr-switches' だけは、
      ;;          デフォルト値を利用する可能性がある関係上、
      ;;          `:config' で `custom-set-variables' する必要がある
      ;;----------------------------
      (custom-set-variables
       ;; ウインドウシステムごとに、印刷コマンドとオプションを最適化
       `(ps-lpr-command ,(cond (;; HACK: `GSview' を経由 (Windows ONLY)
                                (equal window-system 'w32)
                                (or (executable-find "gsview64")
                                    (executable-find "gsview")
                                    "gsview"))
                               (;; HACK: `Preview.app' を経由 (macOS ONLY)
                                (equal window-system 'mac)
                                "open")
                               (t
                                ps-lpr-command)))
       `(ps-lpr-switches ',(cond (;; HACK: `GSview' を経由 (Windows ONLY)
                                  (equal window-system 'w32)
                                  nil)
                                 (;; HACK: `Preview.app' を経由 (macOS ONLY)
                                  (equal window-system 'mac)
                                  '("-f" "-a" "Preview.app"))
                                 (t
                                  ps-lpr-switches))))
      ;; ---------------------------
      ;; `ps-print' がロードされるまで何もしない
      ;; ---------------------------
      ;; PostScript (mule)
      ;; ---------------------------
      ;; `ps-mule-font-info-database-default' が非 `autoload'
      ;; ゆえに、明示的なロードが必要
      (leaf ps-mule
        :require t
        :config
        ;; -------------------------
        ;; PostScript (BDF: Glyph Bitmap Distribution Format)
        ;; -------------------------
        ;; `ps-mule-font-info-database-bdf' が非 `autoload'
        ;; ゆえに、明示的なロードが必須
        (leaf ps-bdf
          :require t
          :config
          ;; Latin-1 外の文字（日本語など）を印刷できるようにする
          (if (boundp 'bdf-directory-list)
              ;; 自分用 BDF フォントディレクトリを追加
              (add-to-list 'bdf-directory-list (convert-standard-filename (expand-file-name "~/bdf"))))
          (if (and (boundp 'ps-mule-font-info-database-default)
                   (boundp 'ps-mule-font-info-database-bdf))
              ;; 自分用 BDF フォントを追加
              ;;
              ;; `ps-mule-font-info-database-bdf' が `ps-mule.el' で
              ;; `defconst' 定義されていることを考慮し、
              ;; 確実に定義された後で設定
              (let ((database-bdf (copy-tree ps-mule-font-info-database-bdf)))
                ;; 不要なアイテムをすべて除去
                (setq database-bdf (assq-delete-all 'jisx0201 database-bdf))
                (setq database-bdf (assq-delete-all 'japanese-jisx0208 database-bdf))
                (setq database-bdf (assq-delete-all 'japanese-jisx0212 database-bdf))
                ;; 自分用 BDF フォントを追加
                ;;
                ;; FIXME: 現状の設定だと日本語文字が文字化けする
                ;;        対策法を見つけること
                ;;
                (setq database-bdf (append '((japanese-jisx0213-2
                                              (normal bdf "migu-1m.bdf")
                                              (bold bdf "migu-1m-bold.bdf")))
                                           '((japanese-jisx0213.2004-1
                                              (normal bdf "migu-1m.bdf")
                                              (bold bdf "migu-1m-bold.bdf")))
                                           '((unicode-bmp
                                              (normal bdf "migu-1m.bdf")
                                              (bold bdf "migu-1m-bold.bdf")))
                                           '((iso-8859-1
                                              (normal bdf "migu-1m.bdf")
                                              (bold bdf "migu-1m-bold.bdf")))
                                           database-bdf))
                ;;---------------------
                ;; WARNING: `ps-mule-font-info-database-default' は、
                ;;          デフォルト値を加工する関係上、
                ;;          `:config' で `custom-set-variables' する必要がある
                ;;---------------------
                (custom-set-variables
                 `(ps-mule-font-info-database-default ',database-bdf)))))))


    ;; ------------------------------------------------------------------------
    ;; タイムスタンプ記述
    ;; ------------------------------------------------------------------------
    (leaf time-stamp
      :hook ((before-save-hook . time-stamp))
      :custom `(;; ISO 8601 (JIS X 0301) 形式にする
                ;;
                ;; See also:
                ;; https://ja.wikipedia.org/wiki/ISO_8601
                ;;
                ;; WARNING: `time-stamp-time-zone' を "+09:00" にしても、
                ;;          コロン以降が無視される
                ;;
                ;; タイムゾーンは別途指定、以下理由：
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
                ;; FIXME: 現状、OS 側の動的なタイムゾーン変更に追従不能
                ;;        都度評価にしたい
                (time-stamp-format . ,(concat "%:y-%02m-%02dT%02H:%02M:%02S"
                                              (replace-regexp-in-string
                                               ;; コロンがない形式を返されるため、強制的にコロンを付与
                                               ;; 厳密なチェックにより "±1259" 形式のみ対象にする
                                               ;;   → 他は無視
                                               "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
                                               "\\1:\\2"
                                               ;; タイムゾーンが UTC でも "Z" でなく "+0000" を返す
                                               ;; 今のところ、あえて "Z" への変換はしないでおく
                                               (format-time-string "%z"))))))


    ;; ------------------------------------------------------------------------
    ;; TRAMP (Transparent Remote Access, Multiple Protocols)
    ;; ------------------------------------------------------------------------
    (leaf tramp
      :require t
      :custom `(;; WARNING: `load' か `autoload' 後に実行しないと適用されない
                ;; ローカル環境にのみ保存
                (tramp-persistency-file-name . "~/.emacs.tramp")))


    ;; ------------------------------------------------------------------------
    ;; `undo' 履歴の記憶
    ;; ------------------------------------------------------------------------
    (leaf undohist
      :package t
      :require t
      :custom `(;; ローカル環境にのみ保存
                (undohist-directory . "~/.emacs.undohist"))
      :config
      (if (fboundp 'undohist-initialize)
          (undohist-initialize)))


    ;; ------------------------------------------------------------------------
    ;; ファイル名を元に、より唯一性の高いバッファ名を生成
    ;; ------------------------------------------------------------------------
    (leaf uniquify
      :require t
      :custom `((uniquify-buffer-name-style . 'forward)
                (uniquify-ignore-buffers-re . "^*[^*]+*\\-")))


    ;; ------------------------------------------------------------------------
    ;; URL ツール
    ;; ------------------------------------------------------------------------
    (leaf url
      :custom `((url-using-proxy . t)))


    ;; ------------------------------------------------------------------------
    ;; ウインドウ移動キーを直感的にする
    ;; ------------------------------------------------------------------------
    (leaf windmove
      :bind (("C-S-b" . windmove-left)
             ("C-S-f" . windmove-right)
             ("C-S-p" . windmove-up)
             ("C-S-n" . windmove-down))
      :custom `(;; フレーム端のウインドウでは無限スクロールするようにふるまう
                ;; 「マリオブラザーズ」左右画面端におけるループのような動き
                (windmove-wrap-around . t)))
    ) ; END


  ;; ==========================================================================
  ;; マイナーモード
  ;; ==========================================================================
  (leaf *minor-mode
    :config
    ;; ------------------------------------------------------------------------
    ;; スペース区切りによる複数キーワードを使った絞り込み
    ;; ------------------------------------------------------------------------
    (leaf affe
      :package t
      :after (consult orderless)
      :custom `((affe-regexp-function . #'orderless-pattern-compiler)
                (affe-highlight-function . #'orderless--highlight))
      :config
      (if (fboundp 'consult-customize)
          (consult-customize
           affe-grep
           :preview-key (kbd "M-."))))


    ;; ------------------------------------------------------------------------
    ;; 各種検索・置換強化
    ;; ------------------------------------------------------------------------
    (leaf anzu
      :package t
      :bind (("M-%" . anzu-query-replace)
             ("C-M-%" . anzu-query-replace-regexp))
      :custom `((anzu-minimum-input-length . 3)
                (anzu-search-threshold . 1000)
                (anzu-replace-to-string-separator . " -> "))
      :init
      ;; `migemo' 利用可能時
      (leaf anzu
        :after (migemo)
        :custom `((anzu-use-migemo . t)))
      :config
      (if (fboundp 'global-anzu-mode)
          (global-anzu-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 他ウインドウ弱調化
    ;;
    ;; See also:
    ;; `my-default-theme.el'
    ;; ------------------------------------------------------------------------
    (leaf auto-dim-other-buffers
      :package t
      :hook ((after-init-hook . my-auto-dim-other-buffers-mode-initialize))
      :init
      (defun my-auto-dim-other-buffers-mode-initialize ()
        "Initialize `auto-dim-other-buffers-mode'."
        (if (fboundp 'auto-dim-other-buffers-mode)
            (auto-dim-other-buffers-mode +1))))


    ;; ------------------------------------------------------------------------
    ;; 自動バッファ再読込
    ;; ------------------------------------------------------------------------
    (leaf autorevert
      :config
      (if (fboundp 'global-auto-revert-mode)
          (global-auto-revert-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; ブックマーク
    ;; ------------------------------------------------------------------------
    (leaf bookmark
      :require t
      :custom `((bookmark-version-control . t)
                ;; ローカル環境にのみ保存
                (bookmark-default-file . "~/.emacs.bookmark.el")))


    ;; ------------------------------------------------------------------------
    ;; ブックマーク (`bookmark') 拡張
    ;; ------------------------------------------------------------------------
    (leaf bookmark+
      :el-get (bookmark+
               :type github
               :pkgname "emacsmirror/bookmark-plus")
      :after (bookmark)
      :require t)


    ;; ------------------------------------------------------------------------
    ;; プログラマ向けネーミング辞書
    ;; ------------------------------------------------------------------------
    (leaf codic
      :package t
      :bind (("C-c C-q" . codic))
      :config
      ;; ------------------------------
      ;; HACK: 専用バッファをコマンドで `quit-window' させる
      ;; ------------------------------
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
            (advice-add #'codic--view
                        :after
                        #'my-codic-local-set-key))))


    ;; ------------------------------------------------------------------------
    ;; 共通コマンドインタプリタ (Windows ONLY)
    ;; ------------------------------------------------------------------------
    (leaf comint
      :when (member system-type '(ms-dos windows-nt))
      :hook ((comint-mode-hook . my-comint-mode-initialize))
      :custom `((comint-scroll-to-bottom-on-input . 'all)
                (comint-move-point-for-output . 'all)
                (comint-buffer-maximum-size . 5000)
                (comint-process-echoes . t)
                (comint-eol-on-send . t))
      :init
      (defun my-comint-mode-initialize ()
        "Initialize `comint-mode'."
        (if (boundp 'comint-input-sender-no-newline)
            (setq-local comint-input-sender-no-newline t)))

      ;; -----------------------------
      ;; プロセスごとのコーディングシステム変換表
      ;;
      ;; See also:
      ;; https://www.emacswiki.org/emacs/ShellMode#toc1
      ;; -----------------------------
      (add-to-list 'process-coding-system-alist
                   '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix))))


    ;; ------------------------------------------------------------------------
    ;; 補完フレームワーク
    ;; ------------------------------------------------------------------------
    (leaf company
      :package t
      :hook ((after-init-hook . global-company-mode))
      :custom `(;; `company'
                (company-tooltip-limit . 20)
                (company-tooltip-minimum . 10)
                (company-tooltip-offset-display . 'lines)
                (company-tooltip-align-annotations . t)
                (company-tooltip-flip-when-above . t)
                (company-transformers . '(company-sort-by-occurrence))
                (company-minimum-prefix-length . 1)
                (company-abort-manual-when-too-short . t)
                (company-idle-delay . 0.25)
                (company-selection-wrap-around . t)
                ;; `company-dabbrev'
                (company-dabbrev-other-buffers . t)
                (company-dabbrev-downcase . nil)
                ;; `company-dabbrev-code'
                (company-dabbrev-code-modes . '(batch-file-mode
                                                csharp-mode
                                                css-mode
                                                erlang-mode
                                                haskell-mode
                                                html-mode
                                                jde-mode
                                                js-mode
                                                js2-mode
                                                lua-mode
                                                prog-mode
                                                python-mode
                                                scss-mode
                                                typescript-mode))
                (company-dabbrev-code-other-buffers . t)
                (company-dabbrev-code-everywhere . t)
                (company-dabbrev-code-ignore-case . t)))


    ;; ------------------------------------------------------------------------
    ;; 補完フレームワーク (`company') 拡張（補完候補のソート）
    ;; ------------------------------------------------------------------------
    (leaf company-statistics
      :after (company)
      :package t
      :custom `((company-statistics-size . 500)
                ;; ローカル環境にのみ保存
                (company-statistics-file . "~/.emacs.company-statistics-cache.el"))
      :config
      (if (fboundp 'company-statistics-mode)
          (company-statistics-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 補完フレームワーク (`company') 拡張（ポップアップドキュメント）
    ;; ------------------------------------------------------------------------
    (leaf company-quickhelp
      :after (company)
      :package t
      :custom `((company-quickhelp-delay . 0.25))
      :config
      (if (fboundp 'company-quickhelp-mode)
          (company-quickhelp-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; コンパイル
    ;; ------------------------------------------------------------------------
    (leaf compile
      :after (nvm exec-path-from-shell)
      :bind (("C-c C-l" . compile))
      :hook ((compilation-filter-hook . my-compilation-ansi-color-apply))
      :custom `((compilation-window-height . 15)
                ;; ビルドツール・タスクランナーに依存させない
                (compile-command . "")
                (compilation-scroll-output . t)
                (compilation-always-kill . t)
                (compilation-context-lines . t))
      :init
      ;; ------------------------------
      ;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
      ;; ------------------------------
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

      (add-to-list 'compilation-finish-functions 'my-compilation-message)

      ;; ------------------------------
      ;; HACK: ANSI エスケープシーケンスが正しく解釈されない問題を回避
      ;; ------------------------------
      (defun my-compilation-ansi-color-apply ()
        "Recognize ASCII color escape sequences for `compilation-mode' buffer."
        (if (and (require 'ansi-color nil :noerror)
                 (fboundp 'ansi-color-apply-on-region))
            (let ((start-marker (make-marker))
                  (end-marker (process-mark (get-buffer-process (current-buffer)))))
              (set-marker start-marker (point-min))
              (ansi-color-apply-on-region start-marker end-marker))))
      :config
      ;; ------------------------------
      ;; HACK: コンパイル完了後、正常に終了していれば自動でウインドウを閉じる
      ;; ------------------------------
      (defcustom my-compilation-auto-quit-window-enable-buffer-names '("*compilation*")
        "Created buffer names by `compile' command."
        :group 'compilation
        :type '(list (repeat string)))

      ;; `process-status' と `exit-status' の値も得たいので、アドバイスを利用
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
          (advice-add #'compilation-handle-exit
                      :after
                      #'my-compilation-auto-quit-window)))


    ;; ------------------------------------------------------------------------
    ;; 補完
    ;; ------------------------------------------------------------------------
    (leaf consult
      :package t
      :bind (;; Overrides
             ("C-s" . my-consult-line)
             ("C-x b" . consult-buffer)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
             ;; mode-specific-map
             ("C-c h" . consult-history)
             ("C-c m" . consult-mode-command)
             ("C-c b" . consult-bookmark)
             ("C-c k" . consult-kmacro)
             ;; goto-map
             ("M-g e" . consult-compile-error)
             ("M-g f" . consult-flymake)
             ("M-g g" . consult-goto-line)
             ([remap goto-line] . consult-goto-line)
             ("M-g o" . consult-outline)
             ("M-g m" . consult-mark)
             ("M-g k" . consult-global-mark)
             ("M-g i" . consult-imenu)
             ("M-g I" . consult-project-imenu)
             ;; search-map
             ("M-s f" . consult-find)
             ("M-s L" . consult-locate)
             ("M-s g" . consult-grep)
             ("M-s G" . consult-git-grep)
             ("M-s r" . consult-ripgrep)
             ("M-s l" . consult-line)
             ("M-s m" . consult-multi-occur)
             ("M-s k" . consult-keep-lines)
             ("M-s u" . consult-focus-lines)
             ("M-s e" . consult-isearch))
      :hook ((completion-list-mode . consult-preview-at-point-mode))
      :custom `((register-preview-function . #'consult-register-format)
                (xref-show-xrefs-function . #'consult-xref)
                (xref-show-definitions-function . #'consult-xref)
                (consult-project-root-function . #'my-consult-project-root-function))
      :init
      (advice-add #'register-preview
                  :override
                  #'consult-register-window)

      (advice-add #'completing-read-multiple
                  :override
                  #'consult-completing-read-multiple)
      :config
      (defun my-consult-line (&optional at-point)
        "Consult-line uses things-at-point if set C-u prefix."
        (interactive "P")
        (if (fboundp 'consult-line)
            (if at-point
                (consult-line (thing-at-point 'symbol))
              (consult-line))))

      (defun my-consult-project-root-function ()
        "Function which returns project root directory."
        (if (and (fboundp 'project-current)
                 (fboundp 'project-roots))
            (if-let (project (project-current))
                (car (project-roots project)))))

      (if (fboundp 'consult-customize)
          (consult-customize
           consult-theme
           :preview-key '(:debounce 0.2 any)
           consult-ripgrep consult-git-grep consult-grep
           consult-bookmark consult-recent-file consult-xref
           :preview-key (kbd "M-."))))


    ;; ------------------------------------------------------------------------
    ;; 矩形選択
    ;; ------------------------------------------------------------------------
    (leaf cua-base
      :config
      (if (fboundp 'cua-selection-mode)
          ;; 特殊キーバインド無効
          (cua-selection-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; バッファ内マッチ補完
    ;; ------------------------------------------------------------------------
    (leaf dabbrev
      :custom `(;; 補完時に大小文字を区別しない
                (dabbrev-case-fold-search . t)))


    ;; ------------------------------------------------------------------------
    ;; モードラインからモードの表示を消す
    ;; ------------------------------------------------------------------------
    (leaf delight
      :package t
      :config
      (if (fboundp 'delight)
          (delight '(;; 降順ソート
                     (anzu-mode nil "anzu")
                     (auto-dim-other-buffers-mode nil "auto-dim-other-buffers")
                     (company-mode nil "company")
                     (editorconfig-mode nil "editorconfig")
                     (eldoc-mode nil "eldoc")
                     (flycheck-mode nil "flycheck")
                     (flymake-mode nil "flymake")
                     (flyspell-mode nil "flyspell")
                     (flyspell-prog-mode nil "flyspell")
                     (global-anzu-mode nil "anzu")
                     (global-company-mode nil "company")
                     (global-flycheck-mode nil "flycheck")
                     (global-whitespace-mode nil "whitespace")
                     (projectile-mode nil "projectile")
                     (show-smartparens-global-mode nil "smartparens")
                     (show-smartparens-mode nil "smartparens")
                     (smartparens-global-mode nil "smartparens")
                     (smartparens-mode nil "smartparens")
                     (text-scale-mode nil "face-remap")
                     (whitespace-mode nil "whitespace")
                     (yas-global-mode nil "yasnippet")
                     (yas-minor-mode nil "yasnippet")))))


    ;; ------------------------------------------------------------------------
    ;; デスクトップ環境保存・復旧
    ;; ------------------------------------------------------------------------
    (leaf desktop
      ;; ------------------------------
      ;; HACK: `:leaf-defer' がないと `eval-after-load' 内で `:init' が
      ;;       実行される
      ;;         -> 結果的に `autoload' 関数である `desktop-save-mode' が
      ;;            実行されなくなる
      ;; ------------------------------
      :leaf-defer nil
      :bind (("C-c d c" . desktop-clear)
             ("C-c d C-s" . desktop-save)
             ("C-c d s" . desktop-save-in-desktop-dir)
             ("C-c d d" . desktop-remove)
             ("C-c d f" . desktop-change-dir)
             ("C-c d r" . desktop-revert))
      :custom `((desktop-save . 'ask-if-new)
                (desktop-load-locked-desktop . t)
                (desktop-missing-file-warning . t)
                ;; 必要最小限の情報のみ保存させる
                (desktop-locals-to-save . '(case-fold-search
                                            case-replace
                                            desktop-locals-to-save
                                            fill-column
                                            truncate-lines))
                (desktop-restore-frames . t)
                (desktop-restore-in-current-display . t)
                (desktop-restore-forces-onscreen . t)
                (desktop-restore-reuses-frames . t)
                (desktop-file-name-format . 'absolute)
                (desktop-restore-eager . t)
                (desktop-lazy-verbose . t)
                (desktop-lazy-idle-delay . 5))
      :config
      (if (fboundp 'desktop-save-mode)
          (desktop-save-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; ディレクトリブラウジング
    ;; ------------------------------------------------------------------------
    (leaf dired
      :hook ((dired-mode-hook . my-dired-mode-initialize))
      :init
      (defun my-dired-mode-initialize ()
        "Initialize `dired-mode'."
        (if (fboundp 'dired-hide-details-mode)
            ;; 常にすべての情報を表示（簡易モードにしない）
            (dired-hide-details-mode -1))))


    ;; ------------------------------------------------------------------------
    ;; ディレクトリブラウジング (`dired') 拡張
    ;; ------------------------------------------------------------------------
    (leaf dired+
      :el-get (dired+
               :type github
               :pkgname "emacsmirror/dired-plus")
      :after (dired)
      :require t
      :custom `((diredp-hide-details-initially-flag . nil)
                (diredp-hide-details-propagate-flag . nil)))


    ;; ------------------------------------------------------------------------
    ;; EditorConfig
    ;; ------------------------------------------------------------------------
    (leaf editorconfig
      :package t
      :config
      (if (fboundp 'editorconfig-mode)
          (editorconfig-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; GNU Emacs Lisp ドキュメント表示
    ;; ------------------------------------------------------------------------
    (leaf eldoc
      :hook ((emacs-lisp-mode-hook . eldoc-mode)
             (ielm-mode-hook . eldoc-mode)
             (lisp-interaction-mode-hook . eldoc-mode)
             (lisp-mode-hook . eldoc-mode))
      :custom `((eldoc-minor-mode-string . nil)
                (eldoc-idle-delay . 0.2)
                (eldoc-echo-area-use-multiline-p . 'truncate-sym-name-if-fit)))


    ;; ------------------------------------------------------------------------
    ;; GNU Emacs Lisp 定義ジャンプ・バック・ドキュメント閲覧
    ;; ------------------------------------------------------------------------
    (leaf elisp-slime-nav
      :after (eldoc)
      :package t
      :config
      (if (fboundp 'elisp-slime-nav-mode)
          (elisp-slime-nav-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; コンテキストメニュー
    ;; ------------------------------------------------------------------------
    (leaf embark
      :package t
      :bind (("C-." . embark-act)
             ("C-;" . embark-dwim)
             ("C-x ? b" . embark-bindings))
      :custom `((prefix-help-command . #'embark-prefix-help-command))
      :config
      (add-to-list 'display-buffer-alist
                   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                     nil
                     (window-parameters (mode-line-format . none)))))


    ;; ------------------------------------------------------------------------
    ;; Embark ⇔ Consult 連携
    ;; ------------------------------------------------------------------------
    (leaf embark-consult
      :package t
      :leaf-defer nil
      :after (embark consult)
      :hook ((embark-collect-mode . consult-preview-at-point-mode)))


    ;; ------------------------------------------------------------------------
    ;; カーソル下の数値を増減
    ;; ------------------------------------------------------------------------
    (leaf evil-numbers
      :package t
      :bind (("C-3" . evil-numbers/dec-at-pt)
             ("C-4" . evil-numbers/inc-at-pt)))


    ;; ------------------------------------------------------------------------
    ;; デフォルト行文字数の位置にインジケータを表示
    ;;
    ;; See also:
    ;; `fill-column'
    ;; ------------------------------------------------------------------------
    (leaf fill-column-indicator
      :package t
      :bind (("C-c q" . fci-mode))
      :custom `(;; --------------------
                ;; FIXME: `font-lock-comment-face' を用いたい
                ;;        しかし、指定すると、なぜか "red" が用いられる
                ;;        現状は `default' フェイスで回避中
                ;; --------------------
                (fci-rule-color . ,(face-attribute 'default :foreground))
                (fci-rule-use-dashes . t)
                (fci-dash-pattern . 0.5)
                ;; --------------------
                ;; HACK: `fci-mode' を有効にした後、
                ;;       `toggle-truncate-lines' で折り返しを有効にすると
                ;;       `line-move-visual' が強制的に nil となる問題を回避
                ;; --------------------
                (fci-handle-line-move-visual . nil)))


    ;; ------------------------------------------------------------------------
    ;; `dired' における `find' コマンド実行 (Windows ONLY)
    ;;
    ;; See also:
    ;; `dired'
    ;; ------------------------------------------------------------------------
    (leaf find-dired
      :when (member system-type '(ms-dos windows-nt))
      :after (find-dired)
      :config
      ;; ------------------------------
      ;; HACK: `:custom' で設定すると `find-exec-terminator' の展開が
      ;;       `find-dired' の `eval-after-load' より先になりエラーとなる
      ;;       仕方なく `:config' で泥臭く設定しなければならない
      ;; ------------------------------
      (custom-set-variables
       `(find-ls-option ,(cons (format "-exec %s -ld {} %s"
                                       (executable-find "ls")
                                       find-exec-terminator)
                               "-ld"))))


    ;; ------------------------------------------------------------------------
    ;; 自動静的解析
    ;; ------------------------------------------------------------------------
    (leaf flycheck
      :package t
      :bind (("C-c f" . flycheck-mode))
      :hook ((after-init-hook . global-flycheck-mode))
      :custom `((flycheck-checker-error-threshold . nil)
                (flycheck-display-errors-delay . 0.5)
                (flycheck-idle-change-delay . 0.25)
                (flycheck-disabled-checkers . '(javascript-jscs)))
      :config
      ;; ------------------------------
      ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
      ;;       生成されうる警告バッファの出現を抑制
      ;; ------------------------------
      (with-eval-after-load 'warnings
        (if (boundp 'warning-suppress-log-types)
            (add-to-list 'warning-suppress-log-types
                         '(flycheck syntax-checker))))

      ;; ------------------------------
      ;; PATCH: ESLint 優先利用
      ;;        JSHint -> ESLint を ESLint -> JSHint 順に変更
      ;; ------------------------------
      (if (boundp 'flycheck-checkers)
          (let* ((target-and-other-checkers (member 'javascript-eslint
                                                    flycheck-checkers)))
            (delete 'javascript-jshint flycheck-checkers)
            (setcdr target-and-other-checkers
                    (cons 'javascript-jshint
                          (cdr-safe target-and-other-checkers)))))

      ;; ------------------------------
      ;; PATCH: v.Nu サポート
      ;; ------------------------------
      (unless (flycheck-registered-checker-p 'vnu)
        ;; FIXME: v.Nu の標準出力が UTF-8 なので、環境によっては文字化けする
        (flycheck-define-checker vnu
          "A (X)HTML syntax and style checker using v.NU.

See also: `https://github.com/validator/validator'."
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
          :modes (html-mode nxhtml-mode))

        ;; 有効化
        (let ((target-and-other-checkers (member 'html-tidy flycheck-checkers)))
          (if target-and-other-checkers
              ;; デフォルトの (X)HTML チェッカ `html-tidy' と入れ替える
              (setcar target-and-other-checkers 'vnu)
            ;; 未追加ならリスト先頭に追加
            (add-to-list 'flycheck-checkers 'vnu))))

      ;; ------------------------------
      ;; PATCH: Sass（.scss/.sass 両形式）チェック時にキャッシュを使わせない
      ;; ------------------------------
      (dolist (checker '(scss sass))
        (if (and (flycheck-registered-checker-p checker)
                 (not (member "-C" (flycheck-checker-arguments checker))))
            ;; あえて破壊的に変更（元のリストに追加したい）
            (nconc (get checker 'flycheck-command) '("-C"))))

      ;; ------------------------------
      ;; PATCH: temp ファイルのデフォルトコーディングシステムを、
      ;;        強制的に UTF-8 (LF) とする
      ;; ------------------------------
      ;; オーバーライド
      (defun flycheck-save-buffer-to-file (file-name)
        "Save the contents of the current buffer to FILE-NAME."
        ;; 他の部分は元定義と一致させる
        (make-directory (file-name-directory file-name) t)
        ;; FIXME: もっと柔軟に設定できるようにならないか？
        (let ((coding-system-for-write 'utf-8-unix) ; ここだけ変更・決め打ち
              (jka-compr-inhibit t))
          (write-region nil nil file-name nil 0))))


    ;; ------------------------------------------------------------------------
    ;; 自動静的解析拡張（モードライン変更）
    ;; ------------------------------------------------------------------------
    (leaf flycheck-color-mode-line
      :after (flycheck)
      :package t
      :hook ((flycheck-mode-hook . flycheck-color-mode-line-mode)))


    ;; ------------------------------------------------------------------------
    ;; 自動静的解析 (OLD)
    ;; ------------------------------------------------------------------------
    (leaf flymake
      :custom `((flymake-run-in-place . nil)))


    ;; ------------------------------------------------------------------------
    ;; 自動スペルチェッカ
    ;; ------------------------------------------------------------------------
    (leaf flyspell
      :hook (;; Full
             (markdown-mode-hook . flyspell-mode)
             (org-mode-hook . flyspell-mode)
             (text-mode-hook . flyspell-mode)
             ;; Comments ONLY
             (css-mode-hook . flyspell-prog-mode)
             (emacs-lisp-mode-hook . flyspell-prog-mode)
             (html-mode-hook . flyspell-prog-mode)
             (ielm-mode-hook . flyspell-prog-mode)
             (js-mode-hook . flyspell-prog-mode)
             (js2-mode-hook . flyspell-prog-mode)
             (lisp-interaction-mode-hook . flyspell-prog-mode)
             (lisp-mode-hook . flyspell-prog-mode)
             (php-mode-hook . flyspell-prog-mode)
             (scss-mode-hook . flyspell-prog-mode)
             (typescript-mode-hook . flyspell-prog-mode)
             (web-mode-hook . flyspell-prog-mode))
      :custom `((flyspell-delay . 1.0)))


    ;; ------------------------------------------------------------------------
    ;; フレーム
    ;; ------------------------------------------------------------------------
    (leaf frame
      :custom `(;; フレームサイズ変更を px 単位で実行できるようにする
                (frame-resize-pixelwise . t))
      :config
      (if (fboundp 'blink-cursor-mode)
          ;; カーソルは点滅させない
          (blink-cursor-mode -1))

      ;; 半透明化
      (if (and window-system
               (fboundp 'set-frame-parameter))
          (set-frame-parameter nil 'alpha '(90 . 80))))


    ;; ------------------------------------------------------------------------
    ;; フレームセット
    ;; ------------------------------------------------------------------------
    (leaf frameset
      ;; 全設定が完了してから実行しなければならない
      ;; 途中で追加される項目がありうるため
      :hook ((after-init-hook . my-frameset-initialize))
      :init
      (defun my-frameset-initialize ()
        "Initialize `frameset' when `after-init-hook' running."
        (when (listp frameset-filter-alist)
          ;; `desktop' で保存不要な項目はすべて `:never' にする
          (dolist (key '(background-color
                         foreground-color
                         font
                         frameset--text-pixel-height
                         frameset--text-pixel-width
                         GUI:font))
            (setcdr (assoc key frameset-filter-alist) :never)))))


    ;; ------------------------------------------------------------------------
    ;; Google 翻訳インターフェース
    ;; ------------------------------------------------------------------------
    (leaf google-translate
      :package t
      :bind (("C-c C-t p" . google-translate-at-point)
             ("C-c C-t o" . google-translate-at-point-reverse)
             ("C-c C-t q" . google-translate-query-translate)
             ("C-c C-t w" . google-translate-query-translate-reverse)
             ("C-c C-t s" . google-translate-smooth-translate))
      :custom `((google-translate-default-source-language . "auto")
                (google-translate-default-target-language . "ja")
                (google-translate-enable-ido-completion . t)
                (google-translate-show-phonetic . nil)
                (google-translate-listen-program . nil)
                (google-translate-output-destination . 'popup)
                (google-translate-pop-up-buffer-set-focus . t)
                (google-translate-listen-button-label . "[Listen]")))


    ;; ------------------------------------------------------------------------
    ;; `grep'
    ;; ------------------------------------------------------------------------
    (leaf grep
      :bind (("C-M-g" . rgrep)))


    ;; --------------------------------
    ;; `grep' (Windows ONLY)
    ;; --------------------------------
    (leaf grep
      :when (member system-type '(ms-dos windows-nt))
      ;; ------------------------------
      ;; HACK: `autoload' 未対応変数を変更する必要があるため、
      ;;       明示的にロードさせる必要がある
      ;; ------------------------------
      :require t
      :custom `(;; 例外が出るため NUL デバイスは使わせない
                (grep-use-null-device . nil))
      :config
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


    ;; ------------------------------------------------------------------------
    ;; インデントガイド
    ;; ------------------------------------------------------------------------
    (leaf highlight-indent-guides
      :package t
      :hook ((prog-mode-hook . highlight-indent-guides-mode))
      :custom `((highlight-indent-guides-method . 'column)
                (highlight-indent-guides-responsive . 'stack)))


    ;; ------------------------------------------------------------------------
    ;; 拡張補完・展開
    ;; ------------------------------------------------------------------------
    (leaf hippie-exp
      :bind (("M-/" . hippie-expand)))


    ;; ------------------------------------------------------------------------
    ;; カレントカーソル行強調
    ;; ------------------------------------------------------------------------
    (leaf hl-line
      ;; FIXME: `after-init-hook' 後に実行した `load-theme' に対応したい
      ;;        `advice-add' の :after で `enable-theme' を実行してもダメ
      :hook ((after-init-hook . my-hl-line-initialize))
      :init
      (defun my-hl-line-initialize ()
        "Initialize `hl-line'."
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
                         (:background ,line-background-color :inherit nil)))))))
        ;; 実行
        (if (fboundp 'global-hl-line-mode)
            (global-hl-line-mode +1))))


    ;; ------------------------------------------------------------------------
    ;; 特殊コメント強調
    ;; ------------------------------------------------------------------------
    (leaf hl-todo
      :package t
      :custom `((hl-todo-keyword-faces . '(("HOLD" . "#d0bf8f")
                                           ("TODO" . "#cc9393")
                                           ("NEXT" . "#dca3a3")
                                           ("THEM" . "#dc8cc3")
                                           ("PROG" . "#7cb8bb")
                                           ("OKAY" . "#7cb8bb")
                                           ("DONT" . "#5f7f5f")
                                           ("FAIL" . "#8c5353")
                                           ("DONE" . "#afd8af")
                                           ("NOTE"   . "#d0bf8f")
                                           ("KLUDGE" . "#d0bf8f")
                                           ("HACK"   . "#d0bf8f")
                                           ("TEMP"   . "#d0bf8f")
                                           ("FIXME"  . "#cc9393")
                                           ("XXX+"   . "#cc9393")
                                           ("CAUTION" . "#ffff66")
                                           ("WARNING" . "#ff6666"))))
      :config
      (if (fboundp 'global-hl-todo-mode)
          (global-hl-todo-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 強化バッファ一覧
    ;; ------------------------------------------------------------------------
    (leaf ibuffer
      :bind (("C-x C-b" . ibuffer))
      :custom `((ibuffer-expert . t))
      :config
      ;; ------------------------------
      ;; 機能拡張
      ;; ------------------------------
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
            ;; WARNING: この `custom-set-variables' は `:custom' に移動できない
            ;;          変数 `settings' で加工を行った結果が入るため
            (custom-set-variables
             `(ibuffer-formats ',formats))))

        ;; メジャーモード名 + ファイルパスでソート
        ;;
        ;; See also:
        ;; https://www.emacswiki.org/emacs/IbufferMode#toc10
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

        ;; WARNING: この `custom-set-variables' は `:custom' に移動できない
        ;;          `define-ibuffer-sorter' で定義したモード名が入るため
        (custom-set-variables
         '(ibuffer-default-sorting-mode 'mode-name-and-path-alphabetic))))


    ;; ------------------------------------------------------------------------
    ;; 強化バッファ一覧 (`ibuffer') 拡張（`projectile' サポート）
    ;; ------------------------------------------------------------------------
    (leaf ibuffer-projectile
      :after (projectile ibuffer)
      :package t
      :hook ((ibuffer-hook . my-ibuffer-projectile-initialize))
      :init
      (defun my-ibuffer-projectile-initialize ()
        "Initialize `ibuffer'."
        (if (fboundp 'ibuffer-projectile-set-filter-groups)
            (ibuffer-projectile-set-filter-groups))))


    ;; ------------------------------------------------------------------------
    ;; ファイル操作の簡略化
    ;; ------------------------------------------------------------------------
    (leaf ido
      :custom `((ido-enable-flex-matching . t)
                (ido-create-new-buffer . 'always)
                (ido-use-virtual-buffers . t)
                (ido-max-file-prompt-width . 0)
                (ido-use-filename-at-point . 'guess)
                (ido-unc-hosts . t)
                ;; ローカル環境にのみ保存
                (ido-save-directory-list-file . "~/.emacs.ido-save-directory-list.el"))
      :config
      (if (fboundp 'ido-mode)
          (ido-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; ファイル操作の簡略化（全環境に適用）
    ;; ------------------------------------------------------------------------
    (leaf ido-everywhere
      :after (ido)
      :config
      (if (fboundp 'ido-everywhere)
          (ido-everywhere +1)))


    ;; ------------------------------------------------------------------------
    ;; 画像の直接表示
    ;; ------------------------------------------------------------------------
    (leaf image-file
      :config
      (if (fboundp 'auto-image-file-mode)
          (auto-image-file-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; インクリメンタル検索
    ;; ------------------------------------------------------------------------
    (leaf isearch
      :custom `(;; 検索時に大小文字を区別しない
                (isearch-case-fold-search . t)
                ;; 逆検索時に大小文字を区別しない
                (isearch-last-case-fold-search . t)))


    ;; ------------------------------------------------------------------------
    ;; アーカイブファイルを直接編集
    ;; ------------------------------------------------------------------------
    (leaf jka-cmpr-hook
      :require t
      :config
      (if (fboundp 'auto-compression-mode)
          (auto-compression-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; JavaScript リファクタリング補助
    ;; ------------------------------------------------------------------------
    (leaf js2-refactor
      :after (js2-mode)
      :package t
      :require t)


    ;; ------------------------------------------------------------------------
    ;; LSP (Language Server Protocol) クライアント拡張 (UI)
    ;; ------------------------------------------------------------------------
    ;; WARNING: `lsp-mode' が自動ロードする
    ;;          念のため `lsp-mode' より前にインストール
    ;; ------------------------------------------------------------------------
    (leaf lsp-ui
      :package t)


    ;; ------------------------------------------------------------------------
    ;; LSP (Language Server Protocol) クライアント拡張 (Tailwind CSS)
    ;; ------------------------------------------------------------------------
    ;; WARNING: `lsp-mode' が自動ロードする
    ;;          念のため `lsp-mode' より前にインストール
    ;; ------------------------------------------------------------------------
    (leaf lsp-tailwindcss
      :package t
      :custom `((lsp-tailwindcss-add-on-mode . t)))


    ;; ------------------------------------------------------------------------
    ;; LSP (Language Server Protocol) クライアント
    ;;
    ;; See also:
    ;; https://microsoft.github.io/language-server-protocol/
    ;; https://langserver.org/
    ;; ------------------------------------------------------------------------
    (leaf lsp-mode
      :package t
      :hook (;; 有効化は必要最小限にとどめる
             (css-mode-hook . lsp)
             (graphql-mode-hook . lsp)
             (js-mode-hook . lsp)
             (js2-mode-hook . lsp)
             (php-mode-hook . lsp)
             (scss-mode-hook . lsp)
             (typescript-mode-hook . lsp)
             (web-mode-hook . lsp))
      :custom `((lsp-restart . 'auto-restart)
                (lsp-headerline-breadcrumb-enable . nil)
                (lsp-progress-function . 'ignore)
                ;; ローカル環境にのみ保存
                (lsp-session-file . "~/.emacs.lsp-session")))


    ;; ------------------------------------------------------------------------
    ;; 補完候補一覧の側に項目情報を表示
    ;; ------------------------------------------------------------------------
    (leaf marginalia
      :package t
      :config
      (if (fboundp 'marginalia-mode)
          (marginalia-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; メニューバー
    ;; ------------------------------------------------------------------------
    (leaf menu-bar
      :config
      (if (fboundp 'menu-bar-mode)
          ;; 非表示
          (menu-bar-mode -1)))


    ;; ------------------------------------------------------------------------
    ;; ローマ字入力から日本語をインクリメンタル検索
    ;; ------------------------------------------------------------------------
    (leaf migemo
      :leaf-defer nil
      :after (exec-path-from-shell)
      :package t
      :require t
      :bind ((:isearch-mode-map
              :package isearch
              ("C-c C-s" . migemo-isearch-toggle-migemo)))
      :custom `(;; C/Migemo 利用設定
                (migemo-command . ,(executable-find "cmigemo"))
                (migemo-options . '("-q" "--emacs"))
                ;; 空白文字と認識させる対象を広げる
                (migemo-white-space-regexp . "[[:space:]\s-]*")
                ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
                (migemo-directory . ,(convert-standard-filename "~"))
                ;; `migemo' 側で定義されている `isearch' 関連キーバインドを使わせない
                ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
                (migemo-use-default-isearch-keybinding . nil)
                ;; 辞書ファイルはデフォルトのものを利用
                (migemo-dictionary . ,(convert-standard-filename
                                       (if (member system-type '(ms-dos windows-nt))
                                           "C:/programs/cmigemo/share/migemo/utf-8/migemo-dict"
                                         "/usr/local/share/migemo/utf-8/migemo-dict")))
                (migemo-user-dictionary . nil)
                (migemo-regex-dictionary . nil)
                ;; 辞書エンコーディングを明示
                (migemo-coding-system . 'utf-8-unix)
                ;; キャッシュを使わせる
                (migemo-use-pattern-alist . t)
                (migemo-use-frequent-pattern-alist . t)
                (migemo-pattern-alist-length . 1024)
                ;; ローカル環境にのみ保存
                (migemo-pattern-alist-file . "~/.emacs.migemo-pattern")
                (migemo-frequent-pattern-alist-file . "~/.emacs.migemo-frequent"))
      :config
      (if (and (fboundp 'migemo-init)
               (boundp 'migemo-command)
               (boundp 'migemo-dictionary)
               (file-exists-p migemo-dictionary))
          (migemo-init)))


    ;; ------------------------------------------------------------------------
    ;; 無順序スペース区切り補完
    ;; ------------------------------------------------------------------------
    (leaf orderless
      :package t
      :require t
      :custom `((completion-styles . '(orderless)))
      :config
      ;; `migemo' 利用可能時
      ;;
      ;; See also:
      ;; https://nyoho.jp/diary/?date=20210615
      (leaf orderless
        :after (migemo)
        :config
        (if (fboundp 'migemo-get-pattern)
            (defun my-orderless-migemo (component)
              "Match COMPONENT as `migemo'."
              (let ((pattern (migemo-get-pattern component)))
                (condition-case nil
                    (progn (string-match-p pattern "") pattern)
                  (invalid-regexp nil)))))

        (if (and (boundp 'orderless-matching-styles)
                 (fboundp 'my-orderless-migemo))
            (add-to-list 'orderless-matching-styles #'my-orderless-migemo t))))


    ;; ------------------------------------------------------------------------
    ;; 汎用フォーマッタ
    ;; ------------------------------------------------------------------------
    (leaf prettier
      :package t
      :hook ((after-init-hook . global-prettier-mode)))


    ;; ------------------------------------------------------------------------
    ;; 汎用プロジェクト管理
    ;; ------------------------------------------------------------------------
    (leaf projectile
      :package t
      :require t
      :custom `((projectile-enable-caching . t)
                (projectile-completion-system . ',(cond ((featurep 'ido) 'ido)
                                                        ((featurep 'ivy) 'ivy)
                                                        ((featurep 'helm) 'helm)
                                                        (t 'default)))
                (projectile-keymap-prefix . ,(kbd "C-c C-p"))
                ;; ローカル環境にのみ保存
                (projectile-cache-file . "~/.emacs.projectile.cache")
                (projectile-known-projects-file . "~/.emacs.projectile-bookmarks.eld"))
      :config
      (if (fboundp 'projectile-mode)
          (projectile-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 自動カラー表示
    ;; ------------------------------------------------------------------------
    (leaf rainbow-mode
      :config
      (with-eval-after-load 'rainbow-mode
        (when (boundp 'rainbow-html-colors-major-mode-list)
          (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode))))


    ;; ------------------------------------------------------------------------
    ;; ファイル履歴保存
    ;; ------------------------------------------------------------------------
    (leaf recentf
      :custom `(;; 履歴保存数は絞る
                (recentf-max-saved-items . 20)
                ;; ローカル環境にのみ保存
                (recentf-save-file . "~/.emacs.recentf.el")))


    ;; ------------------------------------------------------------------------
    ;; ミニバッファの履歴を残す
    ;; ------------------------------------------------------------------------
    (leaf savehist
      :custom `(;; 履歴保存数は絞る
                (history-length . 100)
                ;; ローカル環境にのみ保存
                (savehist-file . "~/.emacs.savehist.el"))
      :config
      (if (fboundp 'savehist-mode)
          (savehist-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; ファイルごとにカーソル位置を保存
    ;; ------------------------------------------------------------------------
    (leaf saveplace
      :require t
      :custom `(;; ローカル環境にのみ保存
                (save-place-file . "~/.emacs.saveplace.el"))
      :config
      (cond ((fboundp 'save-place-mode) ; v25.1 以降
             (save-place-mode +1))
            ((boundp 'save-place) ; v25.1 未満
             (setq-default save-place t))))


    ;; ------------------------------------------------------------------------
    ;; スクロールバー
    ;; ------------------------------------------------------------------------
    (leaf scroll-bar
      ;; `after-init-hook' で実行しないと適用されない問題がある
      :hook ((after-init-hook . my-scroll-bar-initilalize))
      :init
      ;; ウインドウシステム上では、あらゆるスクロールバーを非表示化
      (defun my-scroll-bar-initilalize ()
        "Initialize `scroll-bar'."
        (with-eval-after-load 'scroll-bar
          (when window-system
            (if (fboundp 'scroll-bar-mode)
                (scroll-bar-mode -1))
            (if (fboundp 'horizontal-scroll-bar-mode)
                (horizontal-scroll-bar-mode -1))))))


    ;; ------------------------------------------------------------------------
    ;; サーバ化
    ;; ------------------------------------------------------------------------
    ;; Windows 環境では `server-auth-dir' の「所有者」が：
    ;;   * Administrator (RID=500)
    ;;   * Administrators (RID=544)
    ;; である場合、`server-ensure-safe-dir' の評価が `nil' になる
    ;;
    ;; `server-auth-dir' で指定したフォルダの
    ;; 「プロパティ」→「セキュリティ」→「詳細設定」→「所有者」→「編集」
    ;; から、所有者をログオンユーザ自身に変更すること
    ;; ------------------------------------------------------------------------
    ;; Windows 環境では emacsclientw.exe 実行時に環境変数
    ;; %EMACS_SERVER_FILE% でサーバファイルのパスを明示しなければならない
    ;; （なぜ必要かは不明）
    ;;
    ;; この欠点をある程度回避した wemacs.cmd を用いること
    ;; ------------------------------------------------------------------------
    (leaf server
      :package t
      :custom `(;; ローカル環境にのみ保存
                (server-auth-dir . "~/.emacs.server"))
      :config
      (if (fboundp 'server-force-delete)
          (server-force-delete))

      (if (fboundp 'server-start)
          (server-start)))


    ;; ------------------------------------------------------------------------
    ;; 基礎編集コマンド集
    ;; ------------------------------------------------------------------------
    (leaf simple
      :config
      (if (fboundp 'transient-mark-mode)
          ;; 暫定マークを使用
          (transient-mark-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 各種カッコ関連機能拡張
    ;; ------------------------------------------------------------------------
    (leaf smartparens
      :package t
      :require t
      :custom `((sp-show-pair-from-inside . nil)
                (sp-undo-pairs-separately . t))
      :config
      (if (fboundp 'show-smartparens-global-mode)
          (show-smartparens-global-mode +1))

      (if (fboundp 'smartparens-global-mode)
          (smartparens-global-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 各種カッコ関連機能拡張・公式デフォルト設定
    ;; ------------------------------------------------------------------------
    (leaf smartparens-config
      :after (smartparens)
      :require t)


    ;; ------------------------------------------------------------------------
    ;; ツールバー
    ;; ------------------------------------------------------------------------
    (leaf tool-bar
      :config
      (if (fboundp 'tool-bar-mode)
          ;; 非表示
          (tool-bar-mode -1)))


    ;; ------------------------------------------------------------------------
    ;; ツールチップ
    ;; ------------------------------------------------------------------------
    (leaf tooltip
      :config
      (if (fboundp 'tooltip-mode)
          ;; 非表示
          (tooltip-mode -1)))


    ;; ------------------------------------------------------------------------
    ;; `redo' 追加
    ;; ------------------------------------------------------------------------
    (leaf undo-fu
      :leaf-defer nil
      :package t
      :require t
      :bind (("C-/" . undo-fu-only-undo)
             ("C-?" . undo-fu-only-redo)))


    ;; ------------------------------------------------------------------------
    ;; 垂直インタラクティブ補完
    ;; ------------------------------------------------------------------------
    (leaf vertico
      :package t
      :custom `((vertico-count . 20))
      :config
      (if (fboundp 'vertico-mode)
          (vertico-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; 巨大ファイル表示
    ;; ------------------------------------------------------------------------
    (leaf vlf
      :disabled t ;; FIXME: ON にすると `bookmark' と `desktop' が不安定になる
      :package t
      :require (vlf-setup)
      :bind ((:vlf-mode-map
              :package vlf
              ("C-c C-v" . vlf-prefix-map)))
      :custom `((vlf-application . 'always)
                (vlf-batch-size . ,(* 1 1024 1024)))) ;; 1MB


    ;; ------------------------------------------------------------------------
    ;; 空白文字強調
    ;; ------------------------------------------------------------------------
    (leaf whitespace
      :hook ((after-change-major-mode-hook . my-whitespace-mode-initialize))
      :custom `(;; 「不正」位置の空白文字のみ強調
                (whitespace-style . '(empty
                                      face
                                      newline
                                      newline-mark
                                      space-after-tab
                                      space-before-tab
                                      space-mark ; HARD SPACE の ON/OFF も含む
                                      spaces ; HARD SPACE の ON/OFF も含む
                                      tab-mark
                                      tabs
                                      trailing))
                ;; --------------------
                ;; HACK: 全角空白 (U+3000) を HARD SPACE とみなして強調表示
                ;;
                ;; 表示テスト:
                ;;   U+0009: 「	」
                ;;   U+00A0: 「 」
                ;;   U+3000: 「　」
                ;; --------------------
                (whitespace-hspace-regexp . "\\(\u00A0\\|\u08A0\\|\u0920\\|\u0E20\\|\u0F20\\|\u3000\\)+")
                (whitespace-trailing-regexp . "\\([\t \u00A0\u3000]+\\)$")
                ;; 行カラム最大値は `fill-column' を参照させる
                (whitespace-line-column . nil)
                ;; --------------------
                ;; HACK: 半角空白 (U+0020) を強調しないようにする
                ;;
                ;; 表示テスト:
                ;;   U+0020: 「 」
                ;; --------------------
                (whitespace-display-mappings . '(;; EOL -> DOLLAR SIGN
                                                 (newline-mark ?\n [?$ ?\n])
                                                 ;; TAB -> CURRENCY SIGN
                                                 (space-mark ?\u00A0 [?¤] [?_])
                                                 ;; IDEOGRAPHIC SPACE -> WHITE SQUARE
                                                 (space-mark ?\u3000 [?\u25a1] [?_ ?_])
                                                 ;; Tab -> RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
                                                 (tab-mark ?\t [?» ?\t] [?\\ ?\t]))))
      :custom-face `((whitespace-space . '((t
                                            (:background nil)))))
      :init
      (defun my-whitespace-mode-initialize ()
        "Initialize `whitespace'."
        ;; ----------------------------
        ;; HACK: 一部メジャーモードでは無効化
        ;; ----------------------------
        (with-eval-after-load 'whitespace
          (if (and (fboundp 'whitespace-mode)
                   (member major-mode '(;; 降順ソート
                                        lisp-interaction-mode
                                        )))
              (whitespace-mode -1))))
      :config
      (if (fboundp 'global-whitespace-mode)
          (global-whitespace-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; ウインドウの状態履歴を undo/redo
    ;; ------------------------------------------------------------------------
    (leaf winner
      :config
      (if (fboundp 'winner-mode)
          (winner-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; スニペット挿入
    ;; ------------------------------------------------------------------------
    (leaf yasnippet
      :package t
      :config
      (if (fboundp 'yas-global-mode)
          (yas-global-mode +1)))


    ;; ------------------------------------------------------------------------
    ;; `yasnippet' 公式コレクション
    ;; ------------------------------------------------------------------------
    (leaf yasnippet-snippets
      :after (yasnippet)
      :package t)
    ) ; END


  ;; ==========================================================================
  ;; メジャーモード
  ;; ==========================================================================
  (leaf *major-mode
    :config
    ;; ------------------------------------------------------------------------
    ;; Apache
    ;; ------------------------------------------------------------------------
    (leaf apache-mode
      :package t
      :mode (("\\.conf\\'" . apache-mode))
      :config
      (if (boundp 'apache-indent-level)
          (setq-local apache-indent-level 4)))


    ;; ------------------------------------------------------------------------
    ;; CSS
    ;; ------------------------------------------------------------------------
    (leaf css-mode
      :hook ((css-mode-hook . my-css-mode-initialize))
      :custom `((css-indent-offset . 2))
      :init
      (defun my-css-mode-initialize ()
        "Initialize `css-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; GNU Emacs Lisp
    ;; ------------------------------------------------------------------------
    (leaf elisp-mode
      :hook ((emacs-lisp-mode-hook . my-emacs-lisp-mode-initialize)
             (lisp-interaction-mode-hook . my-emacs-lisp-mode-initialize)
             (lisp-interaction-mode-hook . my-lisp-interaction-mode-initialize))
      :init
      ;; 共通
      (defun my-emacs-lisp-mode-initialize ()
        "Initialize `emacs-lisp-mode' and `lisp-interaction-mode' before file load."
        (setq-local indent-tabs-mode nil)
        (setq-local tab-width 8)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab"))
                     (tab-width-number-data (gethash 'tab_width editorconfig-properties-hash))
                     (tab-width-number (if (and tab-width-number-data
                                                (stringp tab-width-number-data))
                                           (string-to-number tab-width-number-data)
                                         tab-width)))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style))
                (if (not (equal tab-width tab-width-number))
                    (setq-local tab-width tab-width-number))))))

      ;; `lisp-interaction-mode' ONLY
      (defun my-lisp-interaction-mode-initialize ()
        "Initialize `lisp-interaction-mode-initialize' before file load."
        ;; EMPTY
        ))


    ;; --------------------------------------------------------------------------
    ;; GraphQL
    ;; --------------------------------------------------------------------------
    (leaf graphql-mode
      :package t
      :hook ((graphql-mode-hook . my-haml-mode-initialize))
      :init
      (defun my-graphql-mode-initialize ()
        "Initialize `graphql-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; --------------------------------------------------------------------------
    ;; Haml
    ;; --------------------------------------------------------------------------
    (leaf haml-mode
      :package t
      :hook ((haml-mode-hook . my-haml-mode-initialize))
      :init
      (defun my-haml-mode-initialize ()
        "Initialize `haml-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; Emacs Lisp インタラクション
    ;; ------------------------------------------------------------------------
    (leaf ielm
      :hook ((ielm-mode-hook . my-ielm-mode-initialize))
      :init
      (defun my-ielm-mode-initialize ()
        "Initialize `ielm' major mode before file load."
        (setq-local indent-tabs-mode nil)
        (setq-local tab-width 8)))


    ;; ------------------------------------------------------------------------
    ;; JavaScript (Basic)
    ;; ------------------------------------------------------------------------
    (leaf js-mode
      :hook ((js-mode-hook . my-js-mode-initialize))
      :custom `((js-indent-level . 4)
                (js-expr-indent-offset . 0)
                (js-paren-indent-offset . 0)
                (js-square-indent-offset . 0)
                (js-curly-indent-offset . 0)
                (js-switch-indent-offset . 0)
                (js-flat-functions . nil)
                (js-indent-align-list-continuation . t)
                (js-js-switch-tabs . t)
                (js-indent-first-init . 'dynamic)
                (js-chain-indent . t))
      :init
      (defun my-js-mode-initialize ()
        "Initialize `js-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; JavaScript (Expert)
    ;; ------------------------------------------------------------------------
    (leaf js2-mode
      :package t
      :mode (("\\.es[0-9]\\'" . js2-mode)
             ("\\.js\\'" . js2-mode)
             ("\\.pac\\'" . js2-mode))
      :hook ((js2-mode-hook . my-js2-mode-initialize))
      :custom `((js2-highlight-level . 3) ; すべての構文強調を有効化
                (js2-bounce-indent-p . nil)
                (js2-idle-timer-delay . 0.25)
                (js2-dynamic-idle-timer-adjust . 0)
                (js2-concat-multiline-strings . t)
                ;; 文法チェック関連
                ;;
                ;; 他ツールに任せるため、すべて無効化
                (js2-mode-show-parse-errors . nil)
                (js2-mode-assume-strict . nil)
                (js2-mode-show-strict-warnings . nil)
                (js2-strict-trailing-comma-warning . nil)
                (js2-strict-missing-semi-warning . nil)
                (js2-missing-semi-one-line-override . nil)
                (js2-strict-inconsistent-return-warning . nil)
                (js2-strict-cond-assign-warning . nil)
                (js2-strict-var-redeclaration-warning . nil)
                (js2-strict-var-hides-function-arg-warning . nil)
                ;; その他
                (js2-skip-preprocessor-directives . t)
                (js2-language-version . 200)
                (js2-instanceof-has-side-effects . nil)
                (js2-move-point-on-right-click . nil) ; 使わない
                (js2-allow-rhino-new-expr-initializer . nil) ; 使わない
                (js2-allow-member-expr-as-function-name . nil)
                (js2-include-browser-externs . t)
                (js2-include-rhino-externs . nil)
                (js2-include-node-externs . nil)
                (js2-mode-indent-inhibit-undo . nil)
                (js2-mode-indent-ignore-first-tab . nil)
                (js2-highlight-external-variables . t)
                (js2-warn-about-unused-function-arguments . nil)
                ;; JSLint
                ;;
                ;; 他ツールに任せるため、すべて無効化
                (js2-include-jslint-globals . nil)
                (js2-include-jslint-declaration-externs . nil))
      :init
      (defun my-js2-mode-initialize ()
        "Initialize `js2-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; JSON
    ;; ------------------------------------------------------------------------
    (leaf json-mode
      :package t
      :mode (("\\.json\\'" . json-mode)
             ("\\.stylelintrc\\'" . json-mode))
      :hook ((json-mode-hook . my-json-mode-initialize))
      :init
      (defun my-json-mode-initialize ()
        "Initialize `json-mode' before file load."
        (setq-local indent-tabs-mode nil)
        (setq-local tab-width 2)
        (setq-local js-indent-level tab-width)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab"))
                     (tab-width-number-data (gethash 'tab_width editorconfig-properties-hash))
                     (tab-width-number (if (and tab-width-number-data
                                                (stringp tab-width-number-data))
                                           (string-to-number tab-width-number-data)
                                         tab-width)))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style))
                (if (not (equal tab-width tab-width-number))
                    (setq-local tab-width tab-width-number))
                (if (not (equal js-indent-level tab-width))
                    (setq-local js-indent-level tab-width)))))))


    ;; ------------------------------------------------------------------------
    ;; Lisp
    ;; ------------------------------------------------------------------------
    (leaf lisp-mode
      :hook ((lisp-mode-hook . my-lisp-mode-initialize))
      :init
      (defun my-lisp-mode-initialize ()
        "Itialize `lisp-mode' before file load."
        (setq-local indent-tabs-mode nil)
        (setq-local tab-width 8)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab"))
                     (tab-width-number-data (gethash 'tab_width editorconfig-properties-hash))
                     (tab-width-number (if (and tab-width-number-data
                                                (stringp tab-width-number-data))
                                           (string-to-number tab-width-number-data)
                                         tab-width)))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style))
                (if (not (equal tab-width tab-width-number))
                    (setq-local tab-width tab-width-number)))))))


    ;; ------------------------------------------------------------------------
    ;; Markdown
    ;; ------------------------------------------------------------------------
    (leaf markdown-mode
      :package t
      :hook ((markdown-mode-hook . my-markdown-mode-initialize))
      :custom `((markdown-command . ,(or (executable-find "github-markup")
                                         (executable-find "markdown")
                                         "markdown"))
                (markdown-command-needs-filename . ,(executable-find "github-markup"))
                (markdown-coding-system . 'utf-8-unix)
                ;; プレビュー用に生成した実 HTML ファイルの残存を防ぐ
                (markdown-live-preview-delete-export . 'delete-on-export))
      :init
      (defun my-markdown-mode-initialize ()
        "Initialize `markdown-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style))))))
      :config
      ;; プレーンテキストファイルは除外
      (setq auto-mode-alist
            (delete '("\\.text\\'" . markdown-mode) auto-mode-alist)))


    ;; ------------------------------------------------------------------------
    ;; Mustache
    ;;
    ;; See also:
    ;; https://mustache.github.io/
    ;; ------------------------------------------------------------------------
    (leaf mustache-mode
      :package t
      :hook ((mustache-mode-hook . my-mustache-mode-initialize))
      :init
      (defun my-mustache-mode-initialize ()
        "Initialize `mustache-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; PHP
    ;; ------------------------------------------------------------------------
    (leaf php-mode
      :package t
      :hook ((php-mode-hook . my-php-mode-initialize))
      :init
      (defun my-php-mode-initialize ()
        "Initialize `php-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; TypeScript
    ;; ------------------------------------------------------------------------
    (leaf typescript-mode
      :package t
      :mode (("\\.tsx?\\'" . typescript-mode))
      :hook ((typescript-mode-hook . my-typescript-mode-initialize))
      :init
      (defun my-typescript-mode-initialize ()
        "Initialize `typescript-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; Sass (extension: ".scss")
    ;; ------------------------------------------------------------------------
    (leaf scss-mode
      :package t
      :hook ((scss-mode-hook . my-scss-mode-initialize))
      :custom `(;; コンパイルは常に手動（保存時は何もしない）
                ;; 各種ツール経由でコンパイルされうるため
                (scss-compile-at-save . nil))
      :init
      (defun my-scss-mode-initialize ()
        "Initialize `scss-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; SGML, (X)HTML
    ;; ------------------------------------------------------------------------
    (leaf sgml-mode
      :mode (("\\.[sx]?html?\\'" . html-mode))
      :hook ((html-mode-hook . my-html-mode-initialize)
             (sgml-mode-hook . my-sgml-mode-initialize))
      :init
      ;; SGML
      (defun my-sgml-mode-initialize ()
        "Initialize `sgml-mode' before file load."
        (setq-local indent-tabs-mode nil)

        (when (featurep 'sgml-electric-tag-pair-mode)
          (declare-function sgml-electric-tag-pair-mode "sgml-mode")
          (sgml-electric-tag-pair-mode +1))

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style))))))

      ;; (X)HTML
      (defun my-html-mode-initialize ()
        "Initialize `html-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; TeX
    ;; ------------------------------------------------------------------------
    (leaf tex-mode
      :hook ((tex-mode-hook . my-tex-mode-initialize))
      :init
      (defun my-tex-mode-initialize ()
        "Initialize `tex-mode' before file load."
        (setq-local truncate-lines nil)))


    ;; ------------------------------------------------------------------------
    ;; Text
    ;; ------------------------------------------------------------------------
    (leaf text-mode
      :hook ((text-mode-hook . my-text-mode-initialize))
      :init
      (defun my-text-mode-initialize ()
        "Initialize `text-mode' before file load."
        (setq-local truncate-lines nil)))


    ;; ------------------------------------------------------------------------
    ;; Template Toolkit (tt, written by Perl)
    ;;
    ;; See also:
    ;; http://tt2.org/
    ;; ------------------------------------------------------------------------
    (leaf tt-mode
      :package t)


    ;; ------------------------------------------------------------------------
    ;; Web
    ;; ------------------------------------------------------------------------
    (leaf web-mode
      :package t
      :mode (("\\.njk\\'" . web-mode)
             ("\\.vue\\'" . web-mode))
      :hook ((web-mode-hook . my-web-mode-initialize))
      :custom `((web-mode-block-padding . nil)
                (web-mode-part-padding . nil)
                (web-mode-script-padding . nil)
                (web-mode-style-padding . nil)
                (web-mode-attr-indent-offset . nil)
                (web-mode-attr-value-indent-offset . nil)
                (web-mode-markup-indent-offset . 0)
                (web-mode-css-indent-offset . 0)
                (web-mode-code-indent-offset . 0)
                (web-mode-sql-indent-offset . 0)
                (web-mode-enable-css-colorization . ,(display-graphic-p))
                (web-mode-enable-comment-interpolation . nil)
                (web-mode-enable-comment-annotation . nil)
                (web-mode-enable-auto-indentation . nil)
                (web-mode-enable-auto-closing . nil)
                (web-mode-enable-auto-pairing . nil)
                (web-mode-enable-auto-opening . nil)
                (web-mode-enable-auto-quoting . nil)
                (web-mode-enable-auto-expanding . nil)
                (web-mode-enable-curly-brace-indentation . nil)
                (web-mode-enable-control-block-indentation . nil)
                (web-mode-enable-current-element-highlight . t)
                (web-mode-enable-current-column-highlight . t)
                (web-mode-enable-whitespace-fontification . nil)
                (web-mode-enable-html-entities-fontification . t)
                (web-mode-enable-block-face . t)
                (web-mode-enable-part-face . t)
                (web-mode-enable-inlays . t)
                (web-mode-enable-sexp-functions . t)
                (web-mode-enable-string-interpolation . t)
                (web-mode-enable-literal-interpolation . t)
                (web-mode-enable-sql-detection . t)
                (web-mode-enable-heredoc-fontification . t)
                (web-mode-enable-element-content-fontification . t)
                (web-mode-enable-element-tag-fontification . t)
                (web-mode-enable-front-matter-block . t)
                (web-mode-enable-engine-detection . t)
                (web-mode-enable-optional-tags . t)
                (web-mode-comment-style . 1)
                (web-mode-indent-style . 1)
                (web-mode-auto-close-style . 1)
                (web-mode-auto-quote-style . 1))
      :init
      (defun my-web-mode-initialize ()
        "Initialize `web-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; XML
    ;; ------------------------------------------------------------------------
    (leaf nxml-mode
      :mode (("\\.xml\\'" . nxml-mode)
             ("\\.plist\\'" . nxml-mode))
      :hook ((nxml-mode-hook . my-nxml-mode-initialize))
      :custom `((nxml-child-indent . 2)
                (nxml-attribute-indent . 0)
                (nxml-slash-auto-complete-flag . t)
                (nxml-bind-meta-tab-to-complete-flag . t)
                (nxml-sexp-element-flag . t)
                (nxml-char-ref-display-glyph-flag . t))
      :init
      (defun my-nxml-mode-initialize ()
        "Initialize `nxml-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))


    ;; ------------------------------------------------------------------------
    ;; YAML
    ;; ------------------------------------------------------------------------
    (leaf yaml-mode
      :package t
      :hook ((yaml-mode-hook . my-yaml-mode-initialize))
      :custom `((yaml-indent-offset . 2))
      :init
      (defun my-yaml-mode-initialize ()
        "Initialize `yaml-mode' before file load."
        (setq-local indent-tabs-mode nil)

        ;; EditorConfig 対応
        (with-eval-after-load 'editorconfig
          (if (hash-table-p editorconfig-properties-hash)
              (let* ((indent-style-data (gethash 'indent_style editorconfig-properties-hash))
                     (indent-style (equal indent-style-data "tab")))
                (if (not (equal indent-tabs-mode indent-style))
                    (setq-local indent-tabs-mode indent-style)))))))
    ) ; END
  )


;; ============================================================================
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
