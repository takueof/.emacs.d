;;; init.el --- "GNU Emacs" main config file -*- mode: Emacs-Lisp; coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Taku WATABE
;; Time-stamp: <2026-05-24T18:56:38+09:00>

;; Author: Taku WATABE <taku.eof@gmail.com>

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

;; This config file is for "GNU Emacs" ONLY.
;; Unsupported other "emacsen" ("XEmacs" and others).

;; This file is VERY LONG.
;; Therefore, I intentionally define file local variables in the first line.

;; Check initialization time: (emacs-init-time)

;;; Code:
(set-language-environment "Japanese")


;; ============================================================================
;; コーディングシステム
;; いくつかのデフォルトだけ決める（他は変えない）
;; ============================================================================
;;
;; WARNING: `prefer-coding-system' は絶対に使わないこと！
;;          例：(prefer-coding-system 'utf-8-unix)
;;          システムごとに最適化された、自動設定のデフォルト定義を破壊するため
;;
;; macOS ONLY
(when (member system-type '(darwin))
  (set-keyboard-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq-default default-process-coding-system '(utf-8 . utf-8)))

;; Windows ONLY
(when (member system-type '(ms-dos windows-nt))
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))


;; ------------------------------------
;; 「UTF-8（BOM 有）」のエイリアスを作成
;; デフォルト名は長いため
;; ------------------------------------
(define-coding-system-alias 'utf-8-bom 'utf-8-with-signature)


;; ------------------------------------
;; HACK: `japanese-cp932' を `shift_jis' として認識させる
;;       MIME を使用した自動判定を行うコード（`sgml-mode' など）でも
;;       例外が出ないようにする
;; ------------------------------------
(coding-system-put 'japanese-cp932
                   :mime-charset 'shift_jis)


;; ------------------------------------
;; HACK: `japanese-shift-jis' を `japanese-cp932' のエイリアスに変更する
;;       GNU Emacs における Shift_JIS 定義 `japanese-shift-jis' は、
;;       「JIS X 0208 附属書1」を厳格に実装している
;;       そのため一部文字（例：「～」(U+FF5E)）が未定義であるなどし、
;;       実用するうえで問題が出やすい
;;       そこで Microsoft の Shift_JIS 実装 `japanese-cp932' を、
;;       デフォルトの Shift_JIS 実装として認識させる
;;
;; See: `japanese.el'
;; ------------------------------------
(define-coding-system-alias 'japanese-shift-jis 'japanese-cp932)
(define-coding-system-alias 'shift_jis 'japanese-cp932)
(define-coding-system-alias 'sjis 'japanese-cp932)


;; ------------------------------------
;; 「〜」(U+301C) → 「～」(U+FF5E) 自動変換
;; ------------------------------------
(coding-system-put 'japanese-cp932 ; Shift_JIS
                   :encode-translation-table (get 'japanese-ucs-jis-to-cp932-map 'translation-table))


;; ------------------------------------
;; 「～」(U+FF5E) → 「〜」(U+301C) 自動変換
;; ------------------------------------
(coding-system-put 'japanese-iso-8bit ; EUC-JP
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
(coding-system-put 'iso-2022-jp ; JIS
                   :encode-translation-table (get 'japanese-ucs-cp932-to-jis-map 'translation-table))


;; ============================================================================
;; デフォルト値
;; ============================================================================
;;
;; フレームタイトルはカレントバッファ名を基準にする
;;
(setopt frame-title-format (format "%%b - GNU Emacs v%s" emacs-version))
;;
;; スタートアップ表示を無効にする
;;
(setopt inhibit-startup-screen t)
(setopt inhibit-startup-message t)
(setopt inhibit-startup-buffer-menu t)
;;
;; *scratch* バッファに何も表示しない
;;
(setopt initial-scratch-message nil)
;;
;; 入力時にポインターを自動で隠す
;;
(setopt make-pointer-invisible t)
;;
;; フリンジに空行を表示する
;;
(setopt indicate-empty-lines t)
;;
;; 右フリンジにファイル先頭＆末尾の状態を表示する
;;
(setopt indicate-buffer-boundaries 'right)
;;
;; 読取専用テキストの kill を許可する
;;
(setopt kill-read-only-ok t)
;;
;; `kill-line' で改行も含めて削除する
;;
(setopt kill-whole-line t)
;;
;; `kill-ring' で同一（重複）文字列を無視する
;;
(setopt kill-do-not-save-duplicates t)
;;
;; `undo' で `redo' 履歴を無視する
;;
(setopt undo-no-redo t)
;;
;; クリップボードと `kill-ring' を同期する
;;
(setopt select-enable-clipboard t)
;;
;; レジスタープレビューを表示しない
;;
(setopt register-preview-delay nil)
;;
;; ウインドウ上下端から10行目でスクロールを開始させる
;;
(setopt scroll-margin 10)
(setopt next-screen-context-lines 10)
;;
;; スクロール不能ならバッファ先頭／末尾文字にポイントを移動する
;;
(setopt scroll-error-top-bottom t)
;;
;; 行間調整をしない
;;
(setopt line-spacing nil)
;;
;; 行間移動に論理行を使う
;;
(setopt line-move-visual t)
;;
;; 折り返しなし行を使う
;;
(setopt truncate-lines t)
(setopt truncate-partial-width-windows nil) ; `truncate-lines' 値を尊重する
;;
;; 強制折り返しを事実上無効にする
;;
;; HACK: GNU Emacs が認識可能な整数の最大値を指定する
;;
(setopt fill-column most-positive-fixnum)
;;
;; <tab> キーはインデントのみ行う
;;
(setopt tab-always-indent t)
;;
;; 大文字／小文字の区別を無視する
;;
(setopt case-fold-search t)
(setopt read-buffer-completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)
;;
;; 新規ファイル／バッファ作成時の確認をしない
;;
(setopt confirm-nonexistent-file-or-buffer nil)
;;
;; 最終行への改行（空行）挿入を強制する
;;
(setopt require-final-newline t)
(setopt mode-require-final-newline t)
;;
;; `undo' 上限を引き上げる
;;
(setopt undo-limit 600000)
(setopt undo-strong-limit 900000) ; (= 1.5 (/ undo-strong-limit undo-limit)) を踏襲
;;
;; `eval-expression' 時の出力を制限しない
;;
(setopt eval-expression-print-level nil)
(setopt eval-expression-print-length nil)
;;
;; 補完表示を循環する
;;
(setopt completion-cycle-threshold t)
;;
;; 補完表示を縦にする
;;
(setopt completions-format 'vertical)
;;
;; ログバッファの上限を引き上げる
;;
(setopt message-log-max 2000)
;;
;; ミニバッファ内でミニバッファコマンドを使う
;;
(setopt enable-recursive-minibuffers t)
;;
;; ニーモニックを改行コードにちなんだ表現にする
;;
(setopt eol-mnemonic-dos "[CRLF]")
(setopt eol-mnemonic-mac "[CR]")
(setopt eol-mnemonic-unix "[LF]")
(setopt eol-mnemonic-undecided "")
;;
;; カーソルの行列表記を変更する
;;
(setopt mode-line-position-column-line-format '(" (%l:%c)"))
;;
;; Trash（「ごみ箱」など）を使う
;;
(setopt delete-by-moving-to-trash t)
;;
;; y/n 選択を使う
;;
(setopt use-short-answers t)
;;
;; <Option> を <Meta> とみなす (macOS GUI ONLY)
;;
(if (display-graphic-p)
    (setopt mac-option-modifier 'meta))
;;
;; <Command> に何もさせない (macOS GUI ONLY)
;;
(if (display-graphic-p)
    (setopt mac-command-modifier nil))
;;
;; 右 <Alt> + 左 <Ctrl> で <AltGr> が発送されないようにする (Windows ONLY)
;; <AltGr> は独自のキーコードであり、<C-M-> であるとみなされない
;;
;; See:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Keyboard.html
;;
(setopt w32-recognize-altgr nil)
;;
;; キャレットの視認性が悪くなる問題を回避する (Windows ONLY)
;;
;; See:
;; https://mementomori.social/@tml/116416045226298692
;;
(setopt w32-use-visible-system-caret nil)


;; ------------------------------------
;; リージョンの大文字／小文字変換で、実行の是非を問わせない
;; ------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ------------------------------------
;; モードラインを好みにする
;; ------------------------------------
(line-number-mode +1)
(column-number-mode +1)
(size-indication-mode +1)


;; ------------------------------------
;; CA 証明書を明示する (Windows ONLY)
;; ------------------------------------
(if (member system-type '(ms-dos windows-nt))
    (setopt gnutls-trustfiles (mapcar 'convert-standard-filename
                                      '("C:/programs/cygwin/etc/pki/tls/certs/ca-bundle.trust.crt"
                                        "C:/programs/cygwin/etc/pki/tls/certs/ca-bundle.crt"))))


;; ============================================================================
;; パッケージマネージャー
;; ============================================================================
;;
;; WARNING: `package' といったネットワークセキュリティを使うパッケージの
;;          実行前に `nsm-settings-file' を設定しなければならない
;;
;; ------------------------------------
;; Network Security Manager
;; ------------------------------------
(when (require 'nsm nil :noerror)
  ;;
  ;; HACK: 未 `require' だと `setopt' が効かない問題を回避する
  ;;
  ;; ローカル環境にのみ保存させる
  (setopt nsm-settings-file "~/.emacs-network-security.eld"))


;; ------------------------------------
;; ロード
;; ------------------------------------
(when (require 'package nil :noerror)
  ;; 確実に `package-archives' が定義済でなければならない
  (add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/"))
  ;; あらゆるパッケージロードに先んじて初期化する
  (package-initialize))


;; ------------------------------------
;; 設定補助
;; ------------------------------------
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))


;; ------------------------------------
;; キーワード定義
;; ------------------------------------
(leaf leaf-keywords
  :ensure t
  :config
  (leaf-keywords-init))


;; ============================================================================
;; 組み込みパッケージ
;; ============================================================================
;; ------------------------------------
;; サーバー化
;; ------------------------------------
;;
;; WARNING: サーバー化に依存するパッケージがあるため、なるはやで開始しておく
;;
(leaf server
  :custom (;; ローカル環境にのみ保存させる
           (server-auth-dir . "~/.emacs-server.eld"))
  :config
  (server-start t))


;; ------------------------------------
;; 認証 (macOS ONLY)
;; ------------------------------------
;;
;; WARNING: 認証を使うパッケージがあるため、なるはやで設定しておく
;;
(leaf auth-source
  :when (member system-type '(darwin))
  :defer-config
  (add-to-list 'auth-sources 'macos-keychain-internet)
  (add-to-list 'auth-sources 'macos-keychain-generic))


;; ------------------------------------
;; `comint-mode' と派生モードで ANSI エスケープシーケンスの解釈を開始する
;; ------------------------------------
(leaf ansi-color
  :config
  (ansi-color-for-comint-mode-on))


;; ------------------------------------
;; 自動バッファ再読込
;; ------------------------------------
(leaf autorevert
  :custom (;; ファイル監視（通知）を使わない
           ;;
           ;; GNU Emacs の仕様では 1024 - 50 = 974 個以上の
           ;; ファイル監視を登録できない
           ;; 少しでもファイル監視を減らすため無効化する
           ;;
           ;; See:
           ;; https://www.reddit.com/r/emacs/comments/mq2znn/comment/gugo0n4/?context=3
           (auto-revert-use-notify . nil)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)


;; ------------------------------------
;; ブックマーク
;; ------------------------------------
(leaf bookmark
  :custom ((bookmark-version-control . t)
           ;; ローカル環境にのみ保存させる
           (bookmark-default-file . "~/.emacs-bookmark.eld")))


;; ------------------------------------
;; 共通コマンドインタプリタ (Windows ONLY)
;; ------------------------------------
(leaf comint
  :when (member system-type '(ms-dos windows-nt))
  :hook ((comint-mode-hook . my-comint-mode-initialize))
  :custom ((comint-scroll-to-bottom-on-input . 'all)
           (comint-move-point-for-output . 'all)
           (comint-buffer-maximum-size . 5000)
           (comint-process-echoes . t)
           (comint-eol-on-send . t))
  :init
  (defun my-comint-mode-initialize ()
    "Initialize `comint-mode' before load."
    (setq-local comint-input-sender-no-newline t))
  ;; プロセスごとのコーディングシステム変換表を追加する
  ;;
  ;; See:
  ;; https://www.emacswiki.org/emacs/ShellMode#toc1
  (add-to-list 'process-coding-system-alist
               '("[bB][aA][sS][hH]" . (undecided-dos . undecided-unix))))


;; ------------------------------------
;; コンパイル
;; ------------------------------------
(leaf compile
  :bind (("C-c x" . compile))
  :hook ((compilation-filter-hook . ansi-color-compilation-filter))
  :custom ((compilation-window-height . 15)
           ;; ビルドツール／タスクランナーに依存させない
           (compile-command . "")
           (compilation-scroll-output . t)
           (compilation-always-kill . t)
           (compilation-context-lines . t))
  :init
  ;;
  ;; HACK: コンパイル完了後、モードラインにも状態を簡易表示
  ;;
  (defun my-compilation-message (cur-buffer msg)
    "Show status messages when compile done in `compilation-mode'."
    (let ((msg-text (string-trim msg)) ; 改行文字が含まれうる問題を回避する
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
  ;;
  ;; HACK: コンパイル完了後、正常に終了していれば自動でウインドウを閉じる
  ;;
  (defcustom my-compilation-auto-quit-window-enable-buffer-names '("*compilation*")
    "Created buffer names by `compile' command."
    :group 'compilation
    :type '(list (repeat string)))
  ;;
  ;; HACK: アドバイス経由で `process-status' と `exit-status' を得る
  ;;       `compilation-finish-functions' では `msg' しか参照できないため
  ;;
  (defun my-compilation-auto-quit-window (process-status exit-status msg)
    "Run `quit-window' when `compile' successed."
    (if (and (member (buffer-name)
                     my-compilation-auto-quit-window-enable-buffer-names)
             (or (and (equal process-status 'exit)
                      (zerop exit-status))
                 ;; 改行文字が含まれうる問題を回避する
                 (string-equal "finished" (string-trim msg))))
        (quit-window nil (get-buffer-window))))
  :advice (;;
           ;; HACK: アドバイス経由で `process-status' と `exit-status' を得る
           ;;
           (:after compilation-handle-exit my-compilation-auto-quit-window)))


;; ------------------------------------
;; CUA 標準キー（と矩形選択サポート）
;; ------------------------------------
(leaf cua-mode
  :custom ((cua-enable-cua-keys . nil)
           (cua-remap-control-v . nil)
           (cua-remap-control-z . nil)
           (cua-prefix-override-inhibit-delay . nil)
           (cua-enable-register-prefix . nil)
           (cua-check-pending-input . nil))
  :global-minor-mode cua-selection-mode)


;; ------------------------------------
;; バッファ内マッチ補完
;; ------------------------------------
(leaf dabbrev
  :custom (;; 補完時に大小文字を区別しない
           (dabbrev-case-fold-search . t)))


;; ------------------------------------
;; 行番号表示
;; ------------------------------------
(leaf display-line-numbers
  :bind (("C-c l" . display-line-numbers-mode)))


;; ------------------------------------
;; ディレクトリブラウジング
;; ------------------------------------
(leaf dired
  :hook ((dired-mode-hook . my-dired-mode-initialize))
  :bind ((:dired-mode-map
          ("r" . #'wdired-change-to-wdired-mode)))
  :custom ((dired-kill-when-opening-new-dired-buffer . t)
           (wdired-allow-to-change-permissions . 'advanced))
  :init
  (defun my-dired-mode-initialize ()
    "Initialize `dired-mode' before load."
    ;; 常にすべての情報を表示する（簡易モードにしない）
    (dired-hide-details-mode -1)
    ;; `windmove' を機能させる
    (keymap-local-unset "C-S-f" t)
    (keymap-local-unset "C-S-b" t)
    (keymap-local-unset "C-S-n" t)
    (keymap-local-unset "C-S-p" t)))


;; ------------------------------------
;; EditorConfig
;;
;; See:
;; https://editorconfig.org/
;; ------------------------------------
(leaf editorconfig
  :global-minor-mode t)


;; ------------------------------------
;; GNU Emacs Lisp ドキュメント表示
;; ------------------------------------
(leaf eldoc
  :hook ((emacs-lisp-mode-hook . eldoc-mode)
         (ielm-mode-hook . eldoc-mode)
         (lisp-interaction-mode-hook . eldoc-mode)
         (lisp-mode-hook . eldoc-mode))
  :custom ((eldoc-minor-mode-string . nil)
           (eldoc-idle-delay . 0.25)
           (eldoc-echo-area-use-multiline-p . 'truncate-sym-name-if-fit)))


;; ------------------------------------
;; 区切り自動挿入
;; ------------------------------------
(leaf elec-pair
  :custom ((electric-pair-preserve-balance . t)
           (electric-pair-delete-adjacent-pairs . t)
           (electric-pair-open-newline-between-pairs . t)
           (electric-pair-skip-whitespace . t))
  :global-minor-mode electric-pair-mode)


;; ------------------------------------
;; Emoji😊 挿入
;; ------------------------------------
(leaf emoji
  :bind (("C-c e i" . emoji-insert)
         ("C-c e r" . emoji-recent)
         ("C-c e s" . emoji-search)
         ("C-c e l" . emoji-list)
         ("C-c e d" . emoji-describe)))


;; ------------------------------------
;; EWW (Emacs Web Wowser, Web Browser)
;; ------------------------------------
(leaf eww
  :bind (("C-c C-e" . eww))
  :custom ((eww-search-prefix . "https://www.google.com/search?&q=")
           (eww-history-limit . nil)
           (eww-auto-rename-buffer . 'title)))


;; ------------------------------------
;; テキスト＆コード静的解析 (OLD)
;; ------------------------------------
(leaf flymake
  :custom ((flymake-run-in-place . nil)))


;; ------------------------------------
;; 自動スペルチェッカ
;; ------------------------------------
(leaf flyspell
  :bind (;;
         ;; HACK: `flyspell' のキーバインド横取り問題を回避する
         ;;
         (:flyspell-mode-map
          ("M-TAB" . 'ignore)
          ("C-;" . 'ignore)
          ("C-," . 'ignore)
          ("C-." . 'ignore)
          ("C-c $" . 'ignore)))
  :hook ((text-mode-hook . flyspell-mode)
         (prog-mode-hook . flyspell-prog-mode))
  :custom ((flyspell-delay . 1.0)))


;; ------------------------------------
;; 正規表現検索
;; ------------------------------------
(leaf grep
  :bind (("C-M-g" . rgrep)))


;; ------------------------------------
;; カレントカーソル行強調
;; ------------------------------------
(leaf hl-line
  :custom ((global-hl-line-sticky-flag . t))
  :global-minor-mode global-hl-line-mode)


;; ------------------------------------
;; 強化バッファ一覧
;; ------------------------------------
(leaf ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom ((ibuffer-default-sorting-mode . 'filename/process)
           (ibuffer-expert . t))
  :config
  ;; バッファ名の表示を30文字に変更する
  ;; カラム幅が揃わなくなるため `-1' は不可
  (let* (;; `customize-mark-to-save' の評価を t にするため明示的にコピーする
         (formats (copy-tree ibuffer-formats))
         (settings (assoc 'name (assoc 'mark formats))))
    ;; 該当する設定項目がなければ何もしない
    ;; 将来的に項目が変更された場合でも、例外を出さないため
    (when settings
      (setcdr settings '(30 30 :left :elide))
      ;;
      ;; WARNING: この `setopt' は `:custom' に移動できない
      ;;          変数 `settings' で加工を行った結果を入れるため
      ;;
      (setopt ibuffer-formats formats))))


;; ------------------------------------
;; ファイル操作の簡略化
;; ------------------------------------
(leaf ido
  ;;
  ;; HACK: デフォルト OFF だが、他機能から切り替え可能にしておく
  ;;
  :custom ((ido-enable-flex-matching . t)
           (ido-create-new-buffer . 'always)
           (ido-use-virtual-buffers . t)
           (ido-max-file-prompt-width . 0)
           (ido-use-filename-at-point . 'guess)
           (ido-unc-hosts . t)
           ;; ローカル環境にのみ保存させる
           (ido-save-directory-list-file . "~/.emacs-ido-save-directory-list.eld")))


;; ------------------------------------
;; 画像の直接表示
;; ------------------------------------
(leaf image-file
  :global-minor-mode auto-image-file-mode)


;; ------------------------------------
;; インクリメンタル検索
;; ------------------------------------
(leaf isearch
  :custom ((isearch-case-fold-search . t)
           (isearch-last-case-fold-search . t)))


;; ------------------------------------
;; スペルチェッカー
;; ------------------------------------
(leaf ispell
  :when (or (executable-find "aspell")
            (executable-find "ispell"))
  :custom ((ispell-dictionary . "english")
           (ispell-extra-args . '("--sug-mode=fast"
                                  "--run-together"
                                  "--run-together-limit=5"
                                  "--run-together-min=2"))))


;; ------------------------------------
;; アーカイブファイルの直接編集
;; ------------------------------------
(leaf jka-cmpr-hook
  :global-minor-mode auto-compression-mode)


;; ------------------------------------
;; 自動カラー表示
;; ------------------------------------
(leaf rainbow-mode
  :defer-config
  (add-to-list 'rainbow-html-colors-major-mode-list 'scss-mode))


;; ------------------------------------
;; ファイル履歴保存
;; ------------------------------------
(leaf recentf
  :custom (;; 履歴保存数は絞る
           (recentf-max-saved-items . 20)
           ;; ローカル環境にのみ保存させる
           (recentf-save-file . "~/.emacs-recentf.eld")))


;; ------------------------------------
;; ミニバッファの履歴
;; ------------------------------------
(leaf savehist
  :custom (;; 履歴保存数は絞る
           (history-length . 100)
           ;; ローカル環境にのみ保存させる
           (savehist-file . "~/.emacs-savehist.eld"))
  :global-minor-mode t)


;; ------------------------------------
;; ファイルごとのカーソル位置保存
;; ------------------------------------
(leaf saveplace
  :custom (;; ローカル環境にのみ保存させる
           (save-place-file . "~/.emacs-saveplace.eld"))
  :global-minor-mode save-place-mode)


;; ------------------------------------
;; 基礎編集コマンド集
;; ------------------------------------
(leaf simple
  :config
  ;; インデントにタブ文字 (U+0009) を使わない
  (indent-tabs-mode -1)
  ;; 暫定マークを使う
  (transient-mark-mode +1))


;; ------------------------------------
;; タイムスタンプ
;; ------------------------------------
(leaf time-stamp
  :hook ((before-save-hook . time-stamp))
  :custom `(;; ISO 8601 (JIS X 0301) 形式にする
            ;;
            ;; See:
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
            ;; Windows 環境（環境変数 %TZ% 未指定かつ +09:00 ゾーン）では
            ;; 次の値が使用されてしまう
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
            ;;
            (time-stamp-format . ,(concat "%:y-%02m-%02dT%02H:%02M:%02S"
                                          (replace-regexp-in-string
                                           ;; 強制的にコロンを付与する
                                           ;; コロンなし形式を返されるため
                                           ;; 厳密チェックで "±1259" のみ利用する
                                           ;;   → 他は無視する
                                           "\\`\\([\\+\\-]\\(?:0[0-9]\\|1[0-2]\\)\\)\\([0-5][0-9]\\)\\'"
                                           "\\1:\\2"
                                           ;; タイムゾーンが "+0000" を返す
                                           ;; あえて "Z" への変換はしない
                                           (format-time-string "%z"))))))


;; ------------------------------------
;; 自動生成されるバッファ名をユニーク性が高いものにする
;; ------------------------------------
(leaf uniquify
  :require t
  :custom ((uniquify-buffer-name-style . 'forward)
           (uniquify-ignore-buffers-re . "^*[^*]+*\\-")))


;; ------------------------------------
;; 空白文字強調
;; ------------------------------------
(leaf whitespace
  :hook ((after-change-major-mode-hook . my-whitespace-mode-initialize))
  :custom (;; `fill-column' を参照する
           (whitespace-line-column . nil)
           ;;
           ;; HACK: 全角空白 (U+3000) を SPACE カテゴリに含めて強調する
           ;;
           ;; U+00A0: " "
           ;; U+0020: " "
           ;; U+3000: "　"
           ;; U+0009: "	"
           ;;
           (whitespace-space-regexp . "\\([\u0020\u3000]+\\)")
           (whitespace-display-mappings . '(;; IDEOGRAPHIC SPACE (U+3000) → FULLWIDTH BROKEN BAR (U+FFE4)
                                            (space-mark ?\u3000 [?\uFFE4])
                                            ;; NO-BREAK SPACE (U+00A0) → CURRENCY SIGN (U+00A4)
                                            (space-mark ?\u00A0 [?\u00A4])
                                            ;; LF (U+000A) → DOWNWARDS ARROW WITH CORNER LEFTWARDS (U+21B5)
                                            (newline-mark ?\u000A [?\u21B5 ?\n]))))
  :init
  (defun my-whitespace-mode-initialize ()
    "Initialize `whitespace' before load."
    ;;
    ;; HACK: 一部メジャーモードでは無効にする
    ;;
    (with-eval-after-load 'whitespace
      (if (member major-mode '(;; 降順ソート
                               agent-shell-mode
                               ghostel-mode
                               lisp-interaction-mode
                               vterm-mode))
          (whitespace-mode -1))))
  :global-minor-mode global-whitespace-mode)


;; ------------------------------------
;; ウインドウ移動キーを直感的にする
;; ------------------------------------
(leaf windmove
  :bind (("C-S-b" . windmove-left)
         ("C-S-f" . windmove-right)
         ("C-S-n" . windmove-down)
         ("C-S-p" . windmove-up))
  :custom (;; フレーム端のウインドウでは無限スクロールするようにふるまう
           ;; 「マリオブラザーズ」左右画面端におけるループのような動き
           (windmove-wrap-around . t)))


;; ------------------------------------
;; ウインドウの状態履歴を undo/redo
;; ------------------------------------
(leaf winner
  :global-minor-mode t)


;; ============================================================================
;; 自作ユーティリティをロード
;; ============================================================================
(leaf my-utils
  :load-path* "utils"
  :require t)


;; ============================================================================
;; グローバルキーバインド
;; ============================================================================
(leaf *global-keybind
  :leaf-defer nil
  :after my-utils
  :bind (;; ヘルプ表示を割り当てなおす
         ("C-x ?" . help-command)
         ;; ウインドウ中央表示はもっともシンプルなものを使用する
         ;; `recenter-top-bottom' は使わない
         ("C-l" . recenter)
         ;; リージョン範囲をソートする
         ("C-c s" . sort-lines)
         ;; 1つ前のエラーを表示する
         ("C-x \\" . previous-error)
         ;; メジャーモードを再適用後に `revert-buffer-quick' 実行する
         ("C-c r" . my-revert-buffer-quick-with-normal-mode)
         ;; 行頭移動は物理行とする
         ("C-a" . my-beginning-of-smart-indented-line)
         ;; 前のウインドウに移動する
         ("C-x p" . my-other-window-reverse)
         ;; 前のフレームに移動する
         ("C-x 5 p" . my-other-frame-reverse)
         ;; 折り返し表示を強制的に切り替える
         ("C-x w" . my-toggle-truncate-lines-force)
         ;; カーソル位置に YEN SIGN (U+00A5) を挿入する
         ("C-c i \\" . my-insert-yen-sign)
         ;; カーソル位置にファイル名を挿入する
         ("C-c i f" . my-insert-file-name)
         ;; カーソル位置にファイルパスを挿入する
         ("C-c i p" . my-insert-file-path)
         ;; 一括でエンコーディング変換する
         ("C-c RET f" . my-change-files-coding-system)
         ;; フレーム背景の透明度を切り替える
         ("C-c w t" . my-toggle-frame-transparency))
  :config
  ;; <C-h> を <backspace> とみなす
  (keyboard-translate ?\C-h ?\C-?)
  ;; 誤字の元になる `transpose-chars' キーバインドを明示的に解除する
  (global-unset-key (kbd "C-t"))
  ;; 誤字の元になる `transpose-lines' キーバインドを明示的に解除する
  (global-unset-key (kbd "C-M-t"))
  ;; `suspend-frame' キーバインドを明示的に解除する
  ;; `ido-undo-merge-work-directory' を実行しやすくするため
  (global-unset-key (kbd "C-z")))


;; ============================================================================
;; カラーテーマ
;; ============================================================================
(leaf modus-themes
  :ensure t
  :require t ; 最新 MELPA 版を利用する
  :custom ((modus-themes-bold-constructs . t)
           (modus-themes-common-palette-overrides . '((comment yellow-faint)
                                                      (string green-faint)
                                                      (border-mode-line-active unspecified)
                                                      (border-mode-line-inactive unspecified)
                                                      (bg-mode-line-active bg-green-subtle))))
  :config
  (modus-themes-load-theme 'modus-vivendi))


;; ============================================================================
;; 動的 IME パッチ (Windows ONLY)
;; ============================================================================
;;
;; WARNING: 全 IM に影響するため、なるはやでインストールしておく
;;
(leaf tr-ime
  :when (member system-type '(ms-dos windows-nt))
  :ensure t
  :custom '(;;
            ;; NOTE: 識別名 "W32-IME" は `tr-ime' 未適用だと使えない
            ;;
            (default-input-method . "W32-IME")
            (w32-ime-buffer-switch-p . t)
            (w32-ime-mode-line-state-indicator . "[Aa]")
            (w32-ime-mode-line-state-indicator-list . '("[--]" "[あ]" "[Aa]")))
  :config
  (tr-ime-advanced-install t)
  (w32-ime-initialize))


;; ============================================================================
;; Input Method (IM)
;; ============================================================================
(leaf *input-method
  :when (and window-system ; GUI のみ
             ;;
             ;; WARNING: Windows ではカーソルのフェイスを変えると見えなくなる
             ;;
             (not (member system-type '(ms-dos windows-nt))))
  :after my-utils
  :hook (;; ウインドウ選択後、IM の状態に応じてフェイス `cursor' を変更する
         ;;
         ;; `cursor' はフレーム単位だが、`current-input-method' はバッファ単位
         ;; そのためバッファ間で `current-input-method' 値が異なると、
         ;; `cursor' が意図せぬ状態になる
         ;;
         ;; ゆえに、ウインドウ切替のタイミングで `cursor' を変更する必要がある
         ;; バッファ切替のタイミングでは何もしない
         ;;
         ;; `select-window' 実行後に起動するフックを利用する
         (buffer-list-update-hook . my-change-cursor-faces-by-current-input-method)
         ;; IM と連動させる
         (input-method-activate-hook . my-change-cursor-faces-by-current-input-method)
         (input-method-deactivate-hook . my-change-cursor-faces-by-current-input-method)
         ;; macOS ONLY
         (mac-selected-keyboard-input-source-change-hook . my-change-cursor-faces-by-current-input-method)
         (mac-enabled-keyboard-input-sources-change-hook . my-change-cursor-faces-by-current-input-method)))


;; ============================================================================
;; マイナーモード：外部パッケージ
;; ============================================================================
;; ------------------------------------
;; GNU/Linux, UNIX, macOS 環境変数 $PATH 自動取得＆設定
;; ------------------------------------
;;
;; WARNING: 環境変数を使うパッケージがあるため、なるはやでインストールしておく
;;
(leaf exec-path-from-shell
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; ------------------------------------
;; キーボード駆動メニュー実装ライブラリ
;; ------------------------------------
;;
;; WARNING: 依存する外部パッケージがあるため、なるはやでインストールしておく
;;
(leaf transient
  :ensure t
  :custom (;; ローカル環境にのみ保存させる
           (transient-levels-file . "~/.emacs-transient-levels.eld")
           (transient-values-file . "~/.emacs-transient-values.eld")
           (transient-history-file . "~/.emacs-transient-history.eld")))


;; ------------------------------------
;; Node.js モジュールパス解決
;; ------------------------------------
(leaf add-node-modules-path
  :ensure t
  :hook ((prog-mode-hook . add-node-modules-path)
         (text-mode-hook . add-node-modules-path)))


;; ------------------------------------
;; 非同期あいまい検索
;; ------------------------------------
(leaf affe
  :ensure t
  :bind (("C-M-S-f" . affe-find)
         ("C-M-S-g" . affe-grep)))


;; ------------------------------------
;; コードフォーマッター
;; ------------------------------------
(leaf apheleia
  :ensure t
  :hook ((apheleia-mode-on-hook . my-apheleia-initialize))
  :custom ((apheleia-mode-lighter . ""))
  :init
  (defun my-apheleia-initialize ()
    "Initialize `apheleia' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/ruff を自動検出
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (ruff-path (expand-file-name ".venv/bin/ruff" venv-dir)))
      (when (file-executable-p ruff-path)
        (with-eval-after-load 'apheleia-formatters
          (let ((local-apheleia-formatters (copy-tree apheleia-formatters)))
            (setcar (member "ruff" (assoc 'ruff local-apheleia-formatters)) ruff-path)
            (setcar (member "ruff" (assoc 'ruff-isort local-apheleia-formatters)) ruff-path)
            (setq-local apheleia-formatters local-apheleia-formatters)
            (message "Using project ruff: %s" ruff-path))))))
  :defer-config
  ;;
  ;; JavaScript
  ;;
  ;; パーサーを "babel-flow" から "typescript" に変更する
  ;;
  ;; See:
  ;; https://prettier.io/docs/options#parser
  (ignore-errors ; "--parser=babel-flow" がなければ何もしない
    (setcar (member "--parser=babel-flow" (assoc 'prettier-javascript apheleia-formatters)) "--parser=typescript"))
  ;;
  ;; Python
  ;;
  ;; フォーマッターを `black' から `ruff-isort' → `ruff' 実行に変更する
  ;;
  ;; See:
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
  (setcdr (assoc 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setcdr (assoc 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  :global-minor-mode apheleia-global-mode)


;; ------------------------------------
;; Markdown リーダー (macOS ONLY)
;; ------------------------------------
(leaf arto
  :when (executable-find "arto")
  :vc (:url "https://github.com/arto-app/arto.el" :rev :newest)
  ;; `markdown-open' で起動可能にする
  :custom ((markdown-open-command . #'arto-open)))


;; ------------------------------------
;; 他ウインドウ弱調化
;; ------------------------------------
(leaf auto-dim-other-buffers
  :ensure t
  :hook ((window-setup-hook . auto-dim-other-buffers-mode)))


;; ------------------------------------
;; 補完フレームワーク
;; ------------------------------------
(leaf company
  :ensure t
  :custom (;; `company'
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
                                           scss-mode))
           (company-dabbrev-code-other-buffers . t)
           (company-dabbrev-code-everywhere . t)
           (company-dabbrev-code-ignore-case . t))
  :global-minor-mode global-company-mode)


;; ------------------------------------
;; 補完フレームワーク：拡張（ポップアップ）
;; ------------------------------------
(leaf company-box
  :ensure t
  :hook ((company-mode-hook . company-box-mode)))


;; ------------------------------------
;; 補完フレームワーク：拡張（補完候補のソート）
;; ------------------------------------
(leaf company-statistics
  :ensure t
  :custom ((company-statistics-size . 500)
           ;; ローカル環境にのみ保存させる
           (company-statistics-file . "~/.emacs-company-statistics-cache.eld"))
  :global-minor-mode t)


;; ------------------------------------
;; 補完
;; ------------------------------------
(leaf consult
  :ensure t
  :bind (;; リマップ
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap switch-to-buffer] . consult-buffer)
         ;; 上書き
         ("C-s" . consult-line)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; ミニバッファ
         (minibuffer-local-map
          :package emacs
          ("C-r" . consult-history)))
  :hook ((completion-list-mode-hook . consult-preview-at-point-mode))
  :custom ((register-preview-function . #'consult-register-format)
           (xref-show-xrefs-function . #'consult-xref)
           (xref-show-definitions-function . #'consult-xref))
  :advice ((:override register-preview consult-register-window)))


;; ------------------------------------
;; GitHub Copilot (AI)
;; ------------------------------------
(leaf copilot
  :ensure t
  :bind (("C-c c c" . copilot-mode)
         ("C-TAB" . copilot-complete)
         ("C-<tab>" . copilot-complete)
         (:copilot-completion-map
          ("TAB" . copilot-accept-completion)
          ("<tab>" . copilot-accept-completion)))
  :hook ((post-command-hook . copilot-clear-overlay))
  :custom ((copilot-max-char . -1) ; No limit
           (copilot-indent-offset-warning-disable . t))
  :config
  (add-to-list 'copilot-major-mode-alist '("web" . "html")))


;; ------------------------------------
;; モードラインからモードの表示を消す
;; ------------------------------------
(leaf delight
  :ensure t
  :config
  (delight '(;; 降順ソート
             (auto-dim-other-buffers-mode nil "auto-dim-other-buffers")
             (company-mode nil "company")
             (company-box-mode nil "company-box")
             (easysession-save-mode nil "easysession")
             (editorconfig-mode nil "editorconfig")
             (eldoc-mode nil "eldoc")
             (flycheck-mode nil "flycheck")
             (flymake-mode nil "flymake")
             (flyspell-mode nil "flyspell")
             (flyspell-prog-mode nil "flyspell")
             (global-company-mode nil "company")
             (global-flycheck-mode nil "flycheck")
             (global-whitespace-mode nil "whitespace")
             (inhibit-mouse-mode nil "inhibit-mouse")
             (js2-minor-mode nil "js2-mode")
             (lsp-mode nil "lsp-mode")
             (projectile-mode nil "projectile")
             (text-scale-mode nil "face-remap")
             (whitespace-mode nil "whitespace")
             (yas-global-mode nil "yasnippet")
             (yas-minor-mode nil "yasnippet"))))


;; ------------------------------------
;; 未コミット diff 表示
;; ------------------------------------
(leaf diff-hl
  :ensure t
  :bind (("C-c v g" . diff-hl-diff-goto-hunk)
         ("C-c v n" . diff-hl-next-hunk)
         ("C-c v p" . diff-hl-previous-hunk)
         ("C-c v r" . diff-hl-revert-hunk)
         ("C-c v v" . diff-hl-show-hunk))
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom `((diff-hl-update-async . 'thread)
            (diff-hl-command-prefix . ,(kbd "C-c v")))
  :config
  (diff-hl-dired-mode +1)
  (diff-hl-margin-mode +1)
  :global-minor-mode (global-diff-hl-mode diff-hl-flydiff-mode))


;; ------------------------------------
;; ディレクトリブラウジング：拡張
;; ------------------------------------
(leaf dired+
  :after dired
  :vc (:url "https://github.com/emacsmirror/dired-plus" :rev :newest)
  :require t
  :custom ((diredp-hide-details-initially-flag . nil)
           (diredp-hide-details-propagate-flag . nil))
  :config
  (diredp-toggle-find-file-reuse-dir +1))


;; ------------------------------------
;; コードナビゲーション
;; ------------------------------------
(leaf dumber-jump
  :when (executable-find "rg")
  :ensure t
  :hook (xref-backend-functions . dumber-jump-xref-activate))


;; ------------------------------------
;; セッション管理
;; ------------------------------------
(leaf easysession
  :ensure t
  :leaf-defer nil
  :bind (("C-c d l" . easysession-switch-to)
         ("C-c d L" . easysession-switch-to-and-restore-geometry)
         ("C-c d s" . easysession-save)
         ("C-c d r" . easysession-rename)
         ("C-c d R" . easysession-reset)
         ("C-c d u" . easysession-unload)
         ("C-c d d" . easysession-delete))
  :custom ((easysession-directory . "~/.emacs-easysession"))
  :config
  (easysession-setup))


;; ------------------------------------
;; 補完候補へのアクション提供
;; ------------------------------------
(leaf embark-consult
  :ensure t
  :bind ((:minibuffer-mode-map
          :package emacs
          ("M-." . embark-dwim)
          ("C-." . embark-act)))
  :hook ((embark-collect-mode . consult-preview-at-point-mode))
  :custom ((prefix-help-command . #'embark-prefix-help-command)))


;; ------------------------------------
;; テキスト＆コード静的解析
;; ------------------------------------
(leaf flycheck
  :ensure t
  :bind (("C-c f" . flycheck-mode))
  :hook ((after-init-hook . global-flycheck-mode))
  :custom ((flycheck-checker-error-threshold . nil)
           (flycheck-display-errors-delay . 0.5)
           (flycheck-idle-change-delay . 0.25)
           (flycheck-disabled-checkers . '(javascript-jscs)))
  :config
  ;;
  ;; HACK: `flycheck-checker-error-threshold' 以上の項目が出現すると
  ;;       生成されうる警告バッファの出現を抑制する
  ;;
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-log-types '(flycheck syntax-checker)))
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
  ;;        強制的に UTF-8 (LF) で認識させる
  ;;
  (defun flycheck-save-buffer-to-file (file-name)
    "Save the contents of the current buffer to FILE-NAME."
    ;; 他の部分は元定義と一致させる
    (make-directory (file-name-directory file-name) t)
    ;;
    ;; FIXME: もっと柔軟に設定できるようにしたい
    ;;
    (let ((coding-system-for-write 'utf-8-unix) ; ここで決め打ちしている
          (jka-compr-inhibit t))
      (write-region nil nil file-name nil 0))))


;; ------------------------------------
;; テキスト＆コード静的解析：拡張（モードライン変更）
;; ------------------------------------
(leaf flycheck-color-mode-line
  :ensure t
  :hook ((flycheck-mode-hook . flycheck-color-mode-line-mode)))


;; ------------------------------------
;; Google 翻訳
;; ------------------------------------
(leaf google-translate
  :ensure t
  :bind (("C-c t t" . google-translate-at-point)
         ("C-c t RET" . google-translate-smooth-translate))
  :custom ((google-translate-output-destination . 'help)
           (google-translate-display-translation-phonetic . nil)
           (google-translate-translation-to-kill-ring . t)
           (google-translate-default-source-language . "en")
           (google-translate-default-target-language . "ja")
           (google-translate-translation-directions-alist . '(("ja" . "en")
                                                              ("en" . "ja")
                                                              ("ko" . "ja")
                                                              ("zh-TW" . "ja")
                                                              ("zh-CN" . "ja")))))


;; ------------------------------------
;; 特殊コメント強調
;; ------------------------------------
(leaf hl-todo
  :ensure t
  :custom ((hl-todo-keyword-faces . '(;; 追加
                                      ("PATCH" . "#ffcc00")
                                      ;; 既存
                                      ("HOLD" . "#99ff99")
                                      ("TODO" . "#99ff99")
                                      ("NEXT" . "#99ff99")
                                      ("THEM" . "#99ff99")
                                      ("PROG" . "#00ffff")
                                      ("OKAY" . "#00ffff")
                                      ("DONT" . "#ffffcc")
                                      ("FAIL" . "#ff0000")
                                      ("DONE" . "#00ff00")
                                      ("NOTE" . "#ffccff")
                                      ("KLUDGE" . "#ffccff")
                                      ("HACK" . "#ffccff")
                                      ("TEMP"  . "#ffccff")
                                      ("FIXME" . "#ff0000")
                                      ("XXX" . "#ffccff")
                                      ("CAUTION" . "#ffff00")
                                      ("WARNING" . "#ff0000"))))
  :global-minor-mode global-hl-todo-mode)


;; ------------------------------------
;; 強化バッファ一覧：拡張（`projectile' サポート）
;; ------------------------------------
(leaf ibuffer-projectile
  :ensure t
  :hook ((ibuffer-hook . ibuffer-projectile-set-filter-groups)))


;; ------------------------------------
;; 各種マウス操作無効化
;; ------------------------------------
(leaf inhibit-mouse
  :ensure t
  :global-minor-mode t)


;; ------------------------------------
;; Language Server Protocol (LSP) クライアント
;;
;; See:
;; https://emacs-lsp.github.io/lsp-mode/page/languages/
;; ------------------------------------
(leaf lsp-mode
  :ensure t
  :hook (;; 有効化は必要最小限にとどめる
         (css-mode-hook . lsp-deferred)
         (js-mode-hook . lsp-deferred)
         (json-mode-hook . lsp-deferred)
         (markdown-mode-hook . lsp-deferred)
         (python-mode-hook . lsp-deferred)
         (scss-mode-hook . lsp-deferred)
         (web-mode-hook . lsp-deferred)
         (yaml-mode-hook . lsp-deferred))
  :custom (;;
           ;; `lsp-mode'
           ;;
           (lsp-semantic-tokens-enable . t)
           (lsp-restart . 'auto-restart)
           ;; ローカル環境にのみ保存させる
           (lsp-session-file . "~/.emacs-lsp-session.eld")
           ;; LSP サーバからのファイル監視要求を無視する
           ;;
           ;; GNU Emacs の仕様で 1024 - 50 = 974 個以上のファイル監視が登録不可
           ;; LSP サーバによっては大量のファイル監視要求を行う → 意図的に無視する
           ;;
           ;; See:
           ;; https://www.reddit.com/r/emacs/comments/mq2znn/no_file_descriptors_left/
           ;; https://apple.stackexchange.com/a/418699
           ;; https://github.com/emacs-mirror/emacs/blob/0008003c3e466269074001d637cda872d6fee9be/src/kqueue.c#L387-L401
           (lsp-enable-file-watchers . nil)
           (lsp-eldoc-enable-hover . nil) ; Use `lsp-ui'
           (lsp-enable-indentation . nil) ; Use another formatter and each major-mode
           (lsp-enable-text-document-color . nil) ; Use major-mode
           (lsp-before-save-edits . nil) ; Use another formatter and each major-mode
           (lsp-modeline-diagnostics-enable . nil)
           (lsp-headerline-breadcrumb-enable . nil)
           (lsp-progress-function . 'ignore)
           (lsp-inlay-hint-enable . t)
           (lsp-trim-trailing-whitespace . nil) ; Use `whitespace'
           (lsp-insert-final-newline . nil) ; Use `editorconfig'
           (lsp-trim-final-newlines . nil) ; Use `editorconfig'
           (lsp-rename-use-prepare . nil)
           ;;
           ;; `lsp-javascript' (with TypeScript)
           ;;
           (lsp-javascript-format-enable . nil) ; Use another formatter and each major-mode
           (lsp-typescript-format-enable . nil) ; Use another formatter and each major-mode
           (lsp-typescript-surveys-enabled . nil)
           ;;
           ;; `lsp-json'
           ;;
           ;; NOTE: Secure Internet Gateway (SIG) 強制有効環境では、
           ;;       JSON schema が取得不能になる
           ;;       プロキシサーバー証明書を信頼済 CA 証明書群では
           ;;       検証「しない」設定に変更して回避するしかない
           ;;
           (lsp-http-proxyStrictSSL . nil)
           ;;
           ;; `lsp-html'
           ;;
           (lsp-html-format-enable . nil) ; Use another formatter and each major-mode
           (lsp-html-auto-closing-tags . nil) ; Use `web-mode'
           ;;
           ;; `lsp-eslint'
           ;;
           (lsp-eslint-experimental . '(;; Enable ESLint flat config
                                        ;;
                                        ;; See:
                                        ;; https://discord.com/channels/789885435026604033/1167077517157470278/1174364060712714310
                                        ;; https://github.com/microsoft/vscode-eslint/issues/1518#issuecomment-1319753092
                                        (useFlatConfig . true)))))


;; ------------------------------------
;; LSP: 拡張 (UI)
;; ------------------------------------
;;
;; NOTE: `lsp-mode' が自動ロードする
;;
(leaf lsp-ui
  :ensure t
  :bind ((:lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references)))
  :custom ((lsp-ui-doc-show-with-mouse . nil)
           (lsp-ui-doc-show-with-cursor . t)
           (lsp-ui-doc-header . t)
           (lsp-ui-doc-include-signature . t)
           (lsp-ui-doc-position . 'at-point)
           (lsp-ui-doc-alignment . 'window)
           (lsp-ui-doc-max-width . 200)
           (lsp-ui-doc-max-height . 50)
           (lsp-ui-doc-delay . 0.25)
           ;; `lsp-ui-doc' を有効にしているため、他は不要になる
           (lsp-ui-sideline-enable . nil)))


;; ------------------------------------
;; LSP: 拡張 (Tailwind CSS)
;; ------------------------------------
;;
;; NOTE: `lsp-mode' が自動ロードする
;;
(leaf lsp-tailwindcss
  :ensure t
  :custom ((lsp-tailwindcss-add-on-mode . t)))


;; ------------------------------------
;; LSP: 拡張 (ty)
;; ------------------------------------
;;
;; NOTE: `lsp-mode' が自動ロードする
;;
(leaf lsp-python-ty
  :hook ((python-mode-hook . my-lsp-python-ty-initialize))
  :init
  (defun my-lsp-python-ty-initialize ()
    "Initialize `lsp-python-ty' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/ty を自動検出
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (ty-path (expand-file-name ".venv/bin/ty" venv-dir)))
      (when (file-executable-p ty-path)
        (setq-local lsp-python-ty-clients-server-command `(,ty-path "server"))
        (message "Using project ty: %s" ty-path)))))


;; ------------------------------------
;; Git インターフェース
;; ------------------------------------
(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom ((auto-revert-buffer-list-filter . #'magit-auto-revert-buffer-p)))


;; ------------------------------------
;; Git インターフェース：拡張（GitHub CLI 対応）
;; ------------------------------------
(leaf magit-gh
  :ensure t)


;; ------------------------------------
;; 補完候補に拡張情報を追加
;; ------------------------------------
(leaf marginalia
  :ensure t
  :custom ((marginalia-field-width . 200)
           (marginalia-max-relative-age . 0))
  :global-minor-mode t)


;; ------------------------------------
;; 日本語インクリメンタル検索
;; ------------------------------------
(leaf migemo
  :when (executable-find "cmigemo")
  :leaf-defer nil
  :ensure t
  :require t
  :custom `(;; C/Migemo を利用する
            (migemo-command . ,(executable-find "cmigemo"))
            (migemo-options . '("-q" "--emacs"))
            ;; 空白文字の対象を拡大する
            (migemo-white-space-regexp . "[[:space:]\s-]*")
            ;; ユーザ別基礎ディレクトリは設定ディレクトリ内にまとめる
            (migemo-directory . ,(convert-standard-filename "~"))
            ;; `migemo' 側で定義済の `isearch' 関連キーバインドを使わせない
            ;; ミニバッファ内で `yank' できない現象が発生する問題の対策
            (migemo-use-default-isearch-keybinding . nil)
            ;; 辞書ファイルはデフォルトを利用する
            (migemo-dictionary . ,(catch 'founded
                                    (dolist (path '("/opt/homebrew/share/migemo/utf-8/migemo-dict"
                                                    "C:/programs/cmigemo/dict/utf-8/migemo-dic"))
                                      (if (file-readable-p path)
                                          (throw 'founded path)))))
            (migemo-user-dictionary . nil)
            (migemo-regex-dictionary . nil)
            ;; 辞書エンコーディングを明示する
            (migemo-coding-system . 'utf-8-unix)
            ;; キャッシュを利用する
            (migemo-use-pattern-alist . t)
            (migemo-use-frequent-pattern-alist . t)
            (migemo-pattern-alist-length . 1024)
            ;; ローカル環境にのみ保存させる
            (migemo-pattern-alist-file . "~/.emacs-migemo-pattern.eld")
            (migemo-frequent-pattern-alist-file . "~/.emacs-migemo-frequent.eld"))
  :config
  (migemo-init))


;; ------------------------------------
;; nvm 経由で Node.js を利用する
;; ------------------------------------
(leaf nvm
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :hook ((change-major-mode-after-body-hook . my-nvm-use-for-buffer))
  :init
  (defun my-nvm-use-for-buffer ()
    "Run `nvm-use-for-buffer', but crush the error."
    (ignore-errors (nvm-use-for-buffer)))
  :config
  ;; `~/.nvmrc' がなければ何もしない
  (ignore-errors (nvm-use-for)))


;; ------------------------------------
;; 正規表現を任意順でマッチさせる補完スタイル
;; ------------------------------------
(leaf orderless
  :ensure t
  :custom ((completion-styles . '(orderless))
           ;; `affe' と連携する
           (affe-regexp-function . #'orderless-pattern-compiler)
           (affe-highlight-function . #'orderless--highlight)))
;;
;; `migemo' が準備できたら使いはじめる
;;
;; See:
;; https://nyoho.jp/diary/?date=20210615
;;
(leaf orderless-migemo
  :after (migemo orderless)
  :config
  (defun my-orderless-matching-style-migemo (component)
    "Match COMPONENT as `migemo'."
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  (add-to-list 'orderless-matching-styles #'my-orderless-matching-style-migemo))


;; ------------------------------------
;; 汎用プロジェクト管理
;; ------------------------------------
(leaf projectile
  :ensure t
  :bind ((:projectile-mode-map
          ("C-;" . projectile-next-project-buffer)
          ("C-:" . projectile-previous-project-buffer)))
  :custom `((projectile-enable-caching . t)
            (projectile-completion-system . ',(cond ((featurep 'ido) 'ido)
                                                    (t 'default)))
            ;; ローカル環境にのみ保存させる
            (projectile-cache-file . "~/.emacs-projectile-cache.eld")
            (projectile-known-projects-file . "~/.emacs-projectile-bookmarks.eld"))
  :global-minor-mode t)


;; ------------------------------------
;; 同時置換
;; ------------------------------------
(leaf substitute
  :ensure t
  :bind (("C-M-r" . substitute-target-in-buffer))
  :custom ((substitute-highlight . t)))


;; ------------------------------------
;; `redo' を追加する
;; ------------------------------------
(leaf undo-fu
  :ensure t
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))


;; ------------------------------------
;; 垂直インタラクティブ補完
;; ------------------------------------
(leaf vertico
  :ensure t
  :bind ((:vertico-map
          ("<backspace>" . vertico-directory-delete-char)
          ("<DEL>" . vertico-directory-delete-char)
          ("<escape>" . minibuffer-keyboard-quit)
          ("RET" . vertico-directory-enter)))
  :hook ((minibuffer-setup . vertico-repeat-save))
  :custom ((vertico-count . 20)
           (vertico-cycle . t)
           (vertico-resize . t)
           (vertico-sort-function . #'vertico-sort-history-alpha))
  :global-minor-mode t)


;; ============================================================================
;; メジャーモード
;; ============================================================================
;; ------------------------------------
;; CSS
;; ------------------------------------
(leaf css-mode
  :custom ((css-indent-offset . 2)))


;; ------------------------------------
;; JavaScript (Main)
;; ------------------------------------
(leaf js-mode ; `js2-minor-mode' で拡張する
  :mode (("\\.pac\\'" . js-mode)
         ("\\.[cm]?js\\'" . js-mode)
         ("\\.es[0-9]\\'" . js-mode)
         ("\\.[jt]s\\'" . js-mode)
         ("\\.[jt]sx\\'" . js-jsx-mode))
  :custom ((js-indent-level . 2)
           (js-expr-indent-offset . 0)
           (js-paren-indent-offset . 0)
           (js-square-indent-offset . 0)
           (js-curly-indent-offset . 0)
           (js-switch-indent-offset . 0)
           (js-flat-functions . nil)
           (js-indent-align-list-continuation . t)
           (js-js-switch-tabs . t)
           (js-indent-first-init . 'dynamic)
           (js-chain-indent . t)))


;; ------------------------------------
;; JavaScript (Extend)
;; ------------------------------------
(leaf js2-mode
  :ensure t
  ;;
  ;; HACK: `js2-minor-mode' で `js-mode' を拡張し、`js2-mode' は使わない
  ;;
  :hook ((js-mode-hook . js2-minor-mode)
         (js-jsx-mode-hook . js2-minor-mode))
  :custom ((js2-highlight-level . 3) ; すべての構文強調を有効にする
           (js2-bounce-indent-p . t)
           (js2-idle-timer-delay . 0.25)
           (js2-dynamic-idle-timer-adjust . 0)
           (js2-concat-multiline-strings . t)
           ;; 文法チェック関連
           ;;
           ;; 他ツールに任せるため、すべて無効にする
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
           (js2-move-point-on-right-click . nil)
           (js2-allow-rhino-new-expr-initializer . nil)
           (js2-include-node-externs . t)
           ;; JSLint
           ;;
           ;; 他ツールに任せるため、すべて無効にする
           (js2-include-jslint-globals . nil)
           (js2-include-jslint-declaration-externs . nil)))


;; ------------------------------------
;; JSON
;; ------------------------------------
(leaf json-mode
  :ensure t
  :mode (("\\.ipynb\\'" . json-mode)))


;; ------------------------------------
;; Jupyter
;;
;; See:
;; https://docs.jupyter.org/en/latest/
;; https://github.com/emacs-jupyter/jupyter
;; https://docs.astral.sh/uv/guides/integration/jupyter/
;; ------------------------------------
(leaf jupyter
  :ensure t
  :hook ((python-mode-hook . my-jupyter-initialize)
         (jupyter-repl-mode-hook . my-jupyter-initialize))
  :custom (;; HACK: WebSocket を使う（ZMQ が不安定）
           (jupyter-use-zmq . nil))
  :init
  (defun my-jupyter-initialize ()
    "Initialize `jupyter' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/jupyter を自動検出する
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (jupyter-path (expand-file-name ".venv/bin/jupyter" venv-dir)))
      (when (file-executable-p jupyter-path)
        (setq-local jupyter-command jupyter-path)
        (message "Using project jupyter: %s" jupyter-path)))))


;; ------------------------------------
;; Markdown
;; ------------------------------------
;;
;; NOTE: `markdown-open' → <C-c C-c o>
;;
(leaf markdown-mode
  :ensure t
  :custom `((markdown-coding-system . 'utf-8-unix)
            (markdown-enable-highlighting-syntax . t)
            (markdown-enable-math . t)
            (markdown-enable-prefix-prompts . nil)
            (markdown-fontify-code-blocks-natively . t)
            (markdown-fontify-whole-heading-line . t)
            (markdown-use-pandoc-style-yaml-metadata . t)))


;; ------------------------------------
;; Org
;; ------------------------------------
(leaf org
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o r" . org-capture))
  :custom ((org-use-speed-commands . t))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((jupyter . t))))


;; ------------------------------------
;; Python
;; ------------------------------------
(leaf python-mode
  :hook ((python-mode-hook . my-python-mode-initialize)
         (jupyter-repl-mode-hook . my-python-mode-initialize))
  :init
  (defun my-python-mode-initialize ()
    "Initialize `python-mode' before load."
    ;; `uv' プロジェクトディレクトリで .venv/bin/python を自動検出する
    (when-let* ((venv-dir (locate-dominating-file default-directory ".venv"))
                (python-path (expand-file-name ".venv/bin/python" venv-dir)))
      (when (file-executable-p python-path)
        (setq-local python-shell-interpreter python-path)
        (message "Using project python: %s" python-path)))))


;; ------------------------------------
;; Sass
;; ------------------------------------
(leaf scss-mode
  :ensure t
  :custom (;; 保存時コンパイルを無効にする（他ツールに任せる）
           (scss-compile-at-save . nil)))


;; ------------------------------------
;; Text
;; ------------------------------------
(leaf text-mode
  :hook ((text-mode-hook . my-text-mode-initialize))
  :init
  (defun my-text-mode-initialize ()
    "Initialize `text-mode' before load."
    (setq-local truncate-lines nil)))


;; ------------------------------------
;; Web
;; ------------------------------------
(leaf web-mode
  :ensure t
  :mode (("\\.[sx]?html?\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :custom ((web-mode-enable-css-colorization . t)
           (web-mode-enable-auto-indentation . nil)
           (web-mode-enable-auto-closing . t)
           (web-mode-enable-auto-pairing . nil) ; Use `electric-pair-mode'
           (web-mode-enable-auto-opening . t)
           (web-mode-enable-auto-quoting . nil) ; Use `electric-pair-mode'
           (web-mode-enable-auto-expanding . t)
           (web-mode-enable-curly-brace-indentation . t)
           (web-mode-enable-current-element-highlight . t)
           (web-mode-enable-current-column-highlight . t)
           (web-mode-enable-html-entities-fontification . t)
           (web-mode-enable-block-face . t)
           (web-mode-enable-part-face . t)
           (web-mode-enable-inlays . t)
           (web-mode-enable-sql-detection . t)
           (web-mode-enable-element-content-fontification . t)
           (web-mode-enable-element-tag-fontification . t))
  :defer-config
  (add-to-list 'web-mode-comment-formats '("php" . "//"))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))


;; ------------------------------------
;; XML
;; ------------------------------------
(leaf nxml-mode
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.plist\\'" . nxml-mode))
  :custom ((nxml-child-indent . 2)
           (nxml-attribute-indent . 0)
           (nxml-slash-auto-complete-flag . t)
           (nxml-bind-meta-tab-to-complete-flag . t)
           (nxml-sexp-element-flag . t)
           (nxml-char-ref-display-glyph-flag . t)))


;; ------------------------------------
;; YAML
;; ------------------------------------
(leaf yaml-mode
  :ensure t
  :custom ((yaml-indent-offset . 2)))


;; ============================================================================
;; ターミナルエミュレーター
;; ============================================================================
(leaf ghostel
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :bind (("C-t C-t" . ghostel-project)
         (:ghostel-mode-map
          ("C-c M-d" . ignore)))
  :hook ((vterm-mode-hook . my-vterm-initialize))
  :custom ((ghostel-bold-color 'bright)
           (ghostel-module-auto-install . 'download))
  :init
  (defun my-ghostel-initialize ()
    "Initialize `ghostel' before load."
    ;; 干渉するマイナーモードを無効にする
    (setq-local cua-mode nil)
    (setq-local undo-fu-mode nil))
  :defer-config
  ;;
  ;; WARNING: 確実に `ghostel-keymap-exceptions' が存在する状態で
  ;;          リストを操作しないと他のキーバインドに影響が出る
  ;;
  ;; `windmove' 用の設定をする
  ;; 設定はリストの末尾に追加せねばならない
  (add-to-list 'ghostel-keymap-exceptions "C-S-b" t)
  (add-to-list 'ghostel-keymap-exceptions "C-S-f" t)
  (add-to-list 'ghostel-keymap-exceptions "C-S-n" t)
  (add-to-list 'ghostel-keymap-exceptions "C-S-p" t))

(leaf vterm
  :unless (member system-type '(ms-dos windows-nt))
  :ensure t
  :hook ((vterm-mode-hook . my-vterm-initialize))
  :custom ((vterm-buffer-name-string . "*vterm*")
           (vterm-copy-mode-remove-fake-newlines . t)
           (vterm-max-scrollback . 100000)
           (vterm-shell . "bash"))
  :init
  (defun my-vterm-initialize ()
    "Initialize `vterm' before load."
    ;; 干渉するマイナーモードを無効にする
    (setq-local cua-mode nil)
    (setq-local undo-fu-mode nil))
  :defer-config
  ;;
  ;; WARNING: 確実に `vterm-keymap-exceptions' が存在する状態で
  ;;          リストを操作しないと他のキーバインドに影響が出る
  ;;
  ;; `windmove' 用の設定をする
  ;; 設定はリストの末尾に追加せねばならない
  (add-to-list 'vterm-keymap-exceptions "C-S-b" t)
  (add-to-list 'vterm-keymap-exceptions "C-S-f" t)
  (add-to-list 'vterm-keymap-exceptions "C-S-n" t)
  (add-to-list 'vterm-keymap-exceptions "C-S-p" t))


;; ============================================================================
;; LLM と ACP (Agent Client Protocol) 経由で対話するインターフェース (AI)
;; ============================================================================
(leaf agent-shell
  ;;
  ;; FIXME: ヘッダーの ➤ を → に変えたい
  ;;        しかし `agent-shell.el' の L3227 をはじめ、
  ;;        複数部分で文字列 " ➤ " として直接記載されている
  ;;        PR 出すしかない……？
  ;;
  :when (executable-find "claude-agent-acp")
  :ensure t
  :bind (("C-c a i" . agent-shell)
         (:agent-shell-mode-map
          ("RET" . newline)
          ("C-j" . shell-maker-submit)))
  :custom `((agent-shell-anthropic-default-model-id . ,(getenv "ANTHROPIC_DEFAULT_SONNET_MODEL"))
            (agent-shell-busy-indicator-frames . 'dots-round)
            (agent-shell-context-sources . '(region))
            (agent-shell-embed-file-size-limit . 1048576) ; 1MB (unit: byte)
            (agent-shell-file-completion-enabled . nil)
            (agent-shell-header-style . 'text)
            (agent-shell-highlight-blocks . t)
            (agent-shell-permission-icon . "⚠️")
            (agent-shell-preferred-agent-config . 'claude-code)
            (agent-shell-session-strategy . 'latest)
            (agent-shell-show-welcome-message . nil))
  :init
  (defun my-agent-shell-initialize (f &rest args)
    "Initialize `agent-shell' between from package load to call `agent-shell' function.
F is inner function in `agent-shell', ARGS are F arguments."
    ;; 確実に `agent-shell-make-environment-variables' を定義させる
    (require 'agent-shell nil :noerror)
    ;; `agent-shell-make-environment-variables' 定義後に環境変数を渡す
    (customize-set-variable 'agent-shell-anthropic-claude-environment
                            (agent-shell-make-environment-variables :inherit-env t))
    (apply f args))
  :advice (;;
           ;; HACK: 適切な hook がないため `:advice' でしのぐ
           ;;
           (:around agent-shell my-agent-shell-initialize)))


;; ============================================================================
;; フォント (GUI ONLY)
;; ============================================================================
;;
;; 独自定義したフォント設定
;;
;; WARNING: フォントの定義は後勝ち（CSS と似ている）
;;
;; 文字幅調整テスト
;;   0O| ASCII
;;   Øø| `iso-8859-1'
;;   αı| `cp850'
;;   €€| `cp858'
;;   ⌐░| 記号
;;   ぱ| ひらがな
;;   バ| カタカナ
;;   漢| 漢字
;;   〇| 全角記号
;;   　| IDEOGRAPHIC SPACE (U+3000)
;;   ～| FULLWIDTH TILDE (U+FF5E)
;;   〜| WAVE DASH (U+301C) `cp932' ONLY
;;   😊| Emoji
;;
;; 関連コマンド
;;   文字拡大／縮小モードに入る：
;;     <C-x C-0>
;;   カーソルがポイントしている文字の「簡易」情報を表示する：
;;     <C-x =>
;;   カーソルがポイントしている文字の「詳細」情報を表示する：
;;     <C-u C-x =>
;;   定義済フォントセットを表示する：
;;     M-x describe-fontset
;;   利用可能なフォントのリストを取得する：
;;     (x-list-fonts "*")
;;   定義済フォントセットと別名（エイリアス）のリスト：
;;     fontset-alias-alist
;;   frame で使われているフォントを取得する：
;;     (frame-parameter nil 'font)
;;   カレントバッファで定義されている face のリストを取得する：
;;     (face-list)
;;
;; 関連パッケージ
;;   `mule': Basic commands for multilingual environment
;;   `faces': フェイス実装
;;   `my-utils': 独自サポート関数＆マクロ定義
;;
;; 文字セット
;;   ISO/IEC 8859-1 (`iso-8859-1') → `ascii' の拡張
;;     追加文字：
;;       ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ
;;     フォント側が考慮していないと判別しにくい文字：
;;       "Ø" (U+00d8: LATIN CAPITAL LETTER O WITH STROKE)
;;     See:
;;       https://en.wikipedia.org/wiki/ISO/IEC_8859-1
;;
;;   Code page 850 (`cp850') → `iso-8859-1' の拡張
;;     追加文字：
;;       ₧ƒ⌐¬░▒▓│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀αßΓπΣσµτΦΘΩδ∞φε∩≡≥≤⌠⌡≈∙√ⁿ■
;;     See:
;;       https://en.wikipedia.org/wiki/Code_page_850
;;
;;   Code page 858 (`cp858') → `cp850' から "ı" (U+0131) を "€" (U+20AC) に置換したもの
;;     See:
;;       https://en.wikipedia.org/wiki/Code_page_850#Code_page_858
;;
;;   Code page 932 (`cp932') → 通称「Microsoft Shift_JIS」
;;     `cp932' にしかない文字（JIS X 0213:2004 にない）：
;;       仼伹俍僴僘兤冾凬劜勀卲叝﨎坙坥墲奓奣妺峵巐弡恝悅惞愠愰戓敎昻昮晴朎櫢汯
;;       浯涖淸淲渹猤玽珒珵琩皂益硺礼靖精羡羽菶蕫蠇譓赶﨣軏逸遧釞鈆鉷﨧鋕﨨鎤鏸
;;       鐱鑈閒﨩靃靑飯飼餧館髙鮻鶴￢￤
;;     See:
;;       https://ja.wikipedia.org/wiki/Cp932
;;       https://internet.watch.impress.co.jp/www/column/ogata/news4.htm
;;       https://seiai.ed.jp/sys/text/csd/cf14/c14b050.html
;;
;;   JIS X 0213:2004（`japanese-jisx0213.2004-1' と `japanese-jisx0213-2'）
;;     2004 で字形が変更された文字：
;;       逢芦飴溢茨鰯淫迂厩噂餌襖迦牙廻恢晦蟹葛鞄釜翰翫徽祇汲灸笈卿饗僅喰櫛屑粂
;;       祁隙倦捲牽鍵諺巷梗膏鵠甑叉榊薩鯖錆鮫餐杓灼酋楯薯藷哨鞘杖蝕訊逗摺撰煎煽
;;       穿箭詮噌遡揃遜腿蛸辿樽歎註瀦捗槌鎚辻挺鄭擢溺兎堵屠賭瀞遁謎灘楢禰牌這秤
;;       駁箸叛挽誹樋稗逼謬豹廟瀕斧蔽瞥蔑篇娩鞭庖蓬鱒迄儲餅籾爺鑓愈猷漣煉簾榔屢
;;       冤叟咬嘲囀徘扁棘橙狡甕甦疼祟竈筵篝腱艘芒虔蜃蠅訝靄靱騙鴉
;;     追加文字：
;;       ゔヿヷヸヹヺㇰㇱㇲㇳㇴㇵㇶㇷㇸㇹㇺㇻㇼㇽ
;;     第3水準（追加、1面）：
;;       旧：倶剥叱呑嘘妍屏并痩繋
;;       新：俱剝𠮟吞噓姸屛幷瘦繫
;;     第4水準（一部、2面）：
;;       𠂉𪚲
;;
;; 参考文献
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lookup.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Selection.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Low_002dLevel-Font.html
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq/Alternate-character-sets.html
;;   https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Fonts-and-text-translation.html
;;   https://www.emacswiki.org/emacs/DisplayingNonAsciiCharacters
;;   https://www.emacswiki.org/emacs/FontSets
;;   https://emacs.g.hatena.ne.jp/sakito/20100127
;;
(leaf *font
  ;;
  ;; NOTE: GUI のみ設定する（ターミナルでは設定できないし、する意味もない）
  ;;
  :when window-system
  :after my-utils
  :custom (;; ガベージコレクト中にフォントキャッシュを圧縮しない
           ;; 主記憶が十分なマシンでは、キャッシュ圧縮にかかる時間のほうが遅い
           (inhibit-compacting-font-caches . t)
           ;; シンボルや句読点などを表示するフォントを、設定に応じて選択する
           ;;   → GNU Emacs 25 より前のふるまいに戻す
           (use-default-font-for-symbols . nil)
           ;; 人為的に italic/bold フォントを選択する (Windows ONLY)
           (w32-enable-synthesized-fonts . t))
  :config
  ;; ----------------------------------
  ;; スケール変換
  ;; ----------------------------------
  (cond
   (;; ユーザーカスタム
    (and (my-fallback-font-family "Moralerspace Neon HW")
         (my-fallback-font-family "Migu 1M"))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.785))
    (add-to-list 'face-font-rescale-alist '("-Consolas-" . 0.950))
    (add-to-list 'face-font-rescale-alist '("-Courier New-" . 0.950))
    (add-to-list 'face-font-rescale-alist '("-Menlo-" . 0.900))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.785)))
   (;; macOS プリインストール ONLY
    (and (equal window-system 'mac)
         (my-fallback-font-family "Menlo"))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.950))
    (add-to-list 'face-font-rescale-alist '("-Hiragino Sans-" . 1.300)))
   (;; Windows プリインストール ONLY
    (and (equal window-system 'w32)
         (my-fallback-font-family "Consolas"))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Emoji-" . 0.900))
    (add-to-list 'face-font-rescale-alist '("-Segoe UI Symbol-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-メイリオ-" . 1.200))
    (add-to-list 'face-font-rescale-alist '("-ＭＳ ゴシック-" . 1.200))))


  ;; ----------------------------------
  ;; フォントセット：プログラミング
  ;; ----------------------------------
  (let* (;; 名称
         (fontset-name "programming")
         ;; デフォルトフォントサイズ (pt)
         ;;
         ;; NOTE: pt → 浮動小数点型
         ;;       px → 整数型
         ;;
         (font-size (if (equal window-system 'w32) 10.0 14.0))
         ;; 基礎フォント
         (base-font-family (my-fallback-font-family "Moralerspace Neon HW"
                                                    "Menlo"
                                                    "Consolas"))
         ;; フォントセット生成
         (fontset (create-fontset-from-ascii-font base-font-family nil fontset-name)))
    ;;
    ;; 全文字
    ;;
    (my-set-fontset-font-safe fontset
                              'emacs ; GNU Emacs が認識可能な全ての文字
                              (font-spec :family base-font-family))
    ;;
    ;; 日本語
    ;;
    (my-set-fontset-font-safe fontset
                              'cp932-2-byte ; `ascii' と `katakana-sjis' は含めない
                              (font-spec :family (my-fallback-font-family "Moralerspace Neon HW"
                                                                          "Hiragino Sans"
                                                                          "メイリオ"
                                                                          "ＭＳ ゴシック")))
    (dolist (charset '(japanese-jisx0213.2004-1
                       japanese-jisx0213-2))
      (my-set-fontset-font-safe fontset
                                charset
                                (font-spec :family (my-fallback-font-family "Moralerspace Neon HW"
                                                                            "Hiragino Sans"
                                                                            "メイリオ"
                                                                            "ＭＳ ゴシック"))))
    (my-set-fontset-font-safe fontset
                              ;; 濁点＆半濁点文字が識別しやすいフォントがあれば差し替える
                              ;; 例：「ぱ」「バ」
                              ;; ひらがな＆カタカナ文字全域に適用する
                              `(,(string-to-char "ぁ") . ,(string-to-char "ㇿ"))
                              (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                          "Hiragino Sans"
                                                                          "メイリオ"
                                                                          "ＭＳ ゴシック")))

    (dolist (code (mapcar 'string-to-char
                          ;; WAVE DASH (U+301C), FULLWIDTH TILDE (U+FF5E)
                          (split-string "〜～" "" t)))
      ;;
      ;; HACK: フォントによっては「同字形」の別文字を「別字形」にする
      ;;
      (my-set-fontset-font-safe fontset
                                (cons code code)
                                (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                            "ＭＳ ゴシック"))))
    ;;
    ;; ラテン文字（特に記号類）
    ;;
    (my-set-fontset-font-safe fontset
                              'cp858
                              (font-spec :family (my-fallback-font-family "Menlo"
                                                                          "Courier New")))
    ;;
    ;; Emoji
    ;;
    (my-set-fontset-font-safe fontset
                              'emoji
                              (font-spec :family (my-fallback-font-family "Apple Color Emoji"
                                                                          "Symbola"
                                                                          "Segoe UI Emoji"
                                                                          "Segoe UI Symbol")))
    ;;
    ;; HACK: フォントサイズ変更「専用」の設定を行う
    ;;
    (my-set-fontset-font-safe fontset
                              'ascii ; ASCII 文字のみ
                              (font-spec :size font-size
                                         :family (my-fallback-font-family "Migu 1M"
                                                                          "Menlo"
                                                                          "Consolas")))
    ;;
    ;; 適用
    ;;
    (modify-all-frames-parameters `((font . ,fontset))) ; frame 全体
    ) ; End of "programming"

  ;; ----------------------------------
  ;; フォントセット：ターミナル
  ;; ----------------------------------
  (let* (;; 名称
         (fontset-name "terminal")
         ;; デフォルトフォントサイズ (pt)
         ;;
         ;; NOTE: pt → 浮動小数点型
         ;;       px → 整数型
         ;;
         (font-size (if (equal window-system 'w32) 12.0 14.0))
         ;; 基礎フォント
         (base-font-family (my-fallback-font-family "Moralerspace Neon HW"
                                                    "Menlo"
                                                    "Consolas"))
         ;; フォントセット生成
         (fontset (create-fontset-from-ascii-font base-font-family nil fontset-name)))
    ;;
    ;; 全文字
    ;;
    (my-set-fontset-font-safe fontset
                              'emacs ; GNU Emacs が認識可能な全ての文字
                              (font-spec :family base-font-family))
    ;;
    ;; 日本語
    ;;
    (my-set-fontset-font-safe fontset
                              ;; 濁点＆半濁点文字が識別しやすいフォントがあれば差し替える
                              ;; 例：「ぱ」「バ」
                              ;; ひらがな＆カタカナ文字全域に適用する
                              `(,(string-to-char "ぁ") . ,(string-to-char "ㇿ"))
                              (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                          "Hiragino Sans"
                                                                          "メイリオ"
                                                                          "ＭＳ ゴシック")))
    (dolist (code (mapcar 'string-to-char
                          ;; WAVE DASH (U+301C), FULLWIDTH TILDE (U+FF5E)
                          (split-string "〜～" "" t)))
      ;;
      ;; HACK: フォントによっては「同字形」の別文字を「別字形」にする
      ;;
      (my-set-fontset-font-safe fontset
                                (cons code code)
                                (font-spec :family (my-fallback-font-family "Migu 1M"
                                                                            "ＭＳ ゴシック"))))
    ;;
    ;; ラテン文字（特に記号類）
    ;;
    (my-set-fontset-font-safe fontset
                              'cp858
                              (font-spec :family (my-fallback-font-family "Menlo"
                                                                          "Courier New")))
    ;;
    ;; Emoji
    ;;
    (my-set-fontset-font-safe fontset
                              'emoji
                              (font-spec :family (my-fallback-font-family "Apple Color Emoji"
                                                                          "Symbola"
                                                                          "Segoe UI Emoji"
                                                                          "Segoe UI Symbol")))
    ;;
    ;; HACK: フォントサイズ変更「専用」の設定を行う
    ;;
    (my-set-fontset-font-safe fontset
                              'ascii ; ASCII 文字のみ
                              (font-spec :size font-size
                                         :family (my-fallback-font-family "Migu 1M"
                                                                          "Menlo"
                                                                          "Consolas")))
    ;;
    ;; 適用
    ;;
    (face-remap-set-base 'term :font fontset) ; ターミナルのみ
    ) ; END of "terminal"
  ) ; END of *font


;; ============================================================================
;; `early-init.el' での一時設定を復元
;; ============================================================================
(leaf *early-init-el-restore
  :custom ((gc-cons-threshold . 800000)))


;; ============================================================================
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
