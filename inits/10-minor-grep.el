;;; 10-minor-grep.el --- 設定 - マイナーモード - `grep'

;; Copyright (C) 2013-2017 Taku Watabe
;; Time-stamp: <2017-10-11T18:09:02+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(grep-scroll-output nil))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(defun my-grep-initialize ()
  "Initialize `grep'."
  ;; EMPTY
  )

(add-hook 'grep-setup-hook #'my-grep-initialize)


;; ----------------------------------------------------------------------------
;; 起動
;; ----------------------------------------------------------------------------
(if (fboundp 'rgrep)
    (global-set-key (kbd "C-M-g") #'rgrep))


;; ----------------------------------------------------------------------------
;; PATCH: grep 2.21 から環境変数 `GREP_OPTIONS' が deprecated になったため、
;;        24.x 系の GNU Emacs `grep' では結果に warning が出る問題を回避
;;        25.0 以降でパッチが適用される予定
;;
;; HACK: `add-to-list' を用いている部分は、元コードのままとする
;;
;; see also:
;; https://lists.gnu.org/archive/html/info-gnu/2014-11/msg00009.html
;; https://lists.gnu.org/archive/html/emacs-diffs/2014-09/msg00134.html
;; ----------------------------------------------------------------------------
(if (< emacs-major-version 25)
    (eval-after-load 'grep
      '(progn
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
                      (grep-highlight-matches ,grep-highlight-matches)))))))))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; 10-minor-grep.el ends here
