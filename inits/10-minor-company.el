;;; 10-minor-company.el --- 設定 - マイナーモード - 補完フレームワーク -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taku Watabe
;; Time-stamp: <2019-01-13T00:25:22+09:00>

;;; Commentary:

;;; Code:

(package-install 'company)
(package-install 'company-statistics)
(package-install 'company-quickhelp)


;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
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
 '(company-dabbrev-code-ignore-case t)
 ;;
 ;; `company-statistics'
 ;;
 '(company-statistics-size 500)
 ;; ローカル環境にのみ保存
 '(company-statistics-file
   (convert-standard-filename "~/.emacs.company-statistics-cache.el"))
 ;;
 ;; `company-quickhelp'
 ;;
 '(company-quickhelp-delay 0.25))


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'global-company-mode)
    (add-hook 'after-init-hook #'global-company-mode))

(eval-after-load 'company-mode
  '(progn
     (if (fboundp 'company-statistics-mode)
         (company-statistics-mode +1))
     (if (fboundp 'company-quickhelp-mode)
         (company-quickhelp-mode +1))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-company.el ends here
