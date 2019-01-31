;;; my-modeline.el --- 設定 - モードライン -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Taku Watabe
;; Time-stamp: <2019-01-31T18:19:51+09:00>

;;; Commentary:

;; 独自定義したモードライン設定の集合
;; `feature' 名 `my-modeline'
;;
;; 疑似名前空間プレフィクスは `my-'

;;; Code:


;; ============================================================================
;; 依存解決
;; ============================================================================
(require 'cl-lib nil :noerror)


;; ============================================================================
;; 表示整形
;; ============================================================================
(eval-after-load "bindings"
  '(progn
     ;; モードライン表示領域をスリムにする
     ;; 3以上の空白文字のみで構成された項目は、すべて U+0020 2文字に置換
     (let ((fmt (copy-sequence mode-line-format))
           (item)
           (result '()))
       (while (setq item (car-safe fmt))
         (setq fmt (cdr-safe fmt))
         (push
          (if (and (stringp item)
                   (numberp (string-match "\\`\\s-\\{3,\\}\\'" item)))
              "  "
            item)
          result))
       (setq mode-line-format (reverse result)))

     ;; 各種情報の非表示化
     (delete 'mode-line-client mode-line-format) ; Emacs Client
     (delete 'mode-line-remote mode-line-format) ; リモートバッファ
     (delete 'mode-line-frame-identification mode-line-format) ; フレーム名
     (delete 'mode-line-misc-info mode-line-format))) ;その他


;; ============================================================================
;; コーディングシステム表記の明示化（BOM 有無を含む）
;;
;; see also:
;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
;; ============================================================================
(eval-after-load "bindings"
  '(progn
     (defun my-coding-system-name-mnemonic (coding-system)
       "Specify coding system notation explicitly."
       (let* ((base (coding-system-base coding-system))
              (name (symbol-name base)))
         (cond ((string-prefix-p "utf-8" name) "UTF-8")
               ((string-prefix-p "utf-16" name) "UTF-16")
               ((string-prefix-p "utf-7" name) "UTF-7")
               ((string-prefix-p "japanese-shift-jis" name) "Shift_JIS")
               ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
               ((string-match "japanese-iso-8bit" name) "EUC-JP")
               (t "???"))))

     (defun my-coding-system-bom-mnemonic (coding-system)
       "Indicate the presence or absence of BOM."
       (let ((name (symbol-name coding-system)))
         (cond ((string-match "be-with-signature" name) "[BE]")
               ((string-match "le-with-signature" name) "[LE]")
               ((string-match "-with-signature" name) "[BOM]")
               (t ""))))

     (defun my-buffer-coding-system-mnemonic ()
       "Return a mnemonic for `buffer-file-coding-system'."
       (let* ((code buffer-file-coding-system)
              (name (my-coding-system-name-mnemonic code))
              (bom (my-coding-system-bom-mnemonic code)))
         (format "%s%s" name bom)))

     ;; `mode-line-mule-info' 差替
     (setq-default mode-line-mule-info
                   (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                                  "%z" mode-line-mule-info :test 'equal))))


;; ============================================================================
;; 各種位置情報
;; ============================================================================
(eval-after-load "bindings"
  '(progn
     ;; すべて独自定義にする
     ;; (line-number-mode t)
     ;; (column-number-mode t)
     ;; (size-indication-mode -1)
     ;;
     ;; defcustom 定義ではないため setq を利用
     ;;
     ;; FIXME: リージョン選択時に前・次行の同一カラムヘ直接移動すると、
     ;;        リージョン始点 (region-beginning) と終点 (region-end) の値が
     ;;        更新されない場合がある（視覚表現と一致しなくなる）
     (setq mode-line-position
           `(
             ;; カーソル位置情報
             (:eval (if (use-region-p)
                        ;; リージョン選択時
                        (propertize
                         (format "%d" (abs (- (region-end)
                                              (region-beginning))))
                         'local-map mode-line-column-line-number-mode-map
                         'mouse-face 'mode-line-highlight
                         'help-echo "リージョン文字数")
                      ;; 通常
                      ;;
                      ;; 行先頭にあるときは0になる（`current-column' と同仕様）
                      ;; 現状では %c + 1 にはしない
                      ,(propertize
                        "%c"
                        'local-map mode-line-column-line-number-mode-map
                        'mouse-face 'mode-line-highlight
                        'help-echo "行内カーソル位置")))
             ":"
             (:eval (if (use-region-p)
                        ;; リージョン選択時
                        (propertize
                         (format "%d" (count-lines (region-beginning)
                                                   (region-end)))
                         'local-map mode-line-column-line-number-mode-map
                         'mouse-face 'mode-line-highlight
                         'help-echo "リージョン行数")
                      ;; 通常
                      ,(propertize
                        "%l"
                        'local-map mode-line-column-line-number-mode-map
                        'mouse-face 'mode-line-highlight
                        'help-echo "現在行")))
             ;; セパレータ
             " "
             ;; バッファ情報
             "["
             (:eval (propertize
                     (format "%d"
                             (abs (- (line-end-position)
                                     (line-beginning-position))))
                     'local-map mode-line-column-line-number-mode-map
                     'mouse-face 'mode-line-highlight
                     'help-echo "全文字数（現在行）"))
             ":"
             (:eval (propertize
                     (format "%d" (line-number-at-pos (point-max)))
                     'local-map mode-line-column-line-number-mode-map
                     'mouse-face 'mode-line-highlight
                     'help-echo "全行数"))
             "]"
             ;; セパレータ
             " "
             ;; バッファサイズ
             ,(concat
               (propertize
                "%ic"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "全文字数")
               "/"
               (propertize
                "%I"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "ファイルサイズ"))
             ))
     ))


;; ============================================================================
;; 改行コード
;; ============================================================================
(custom-set-variables
 ;; ニーモニックを改行コードにちなんだ表現にする
 '(eol-mnemonic-dos "[CRLF]")
 '(eol-mnemonic-mac "[CR]")
 '(eol-mnemonic-unix "[LF]")
 '(eol-mnemonic-undecided ""))


(provide 'my-modeline)


;; ============================================================================
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; my-modeline.el ends here
