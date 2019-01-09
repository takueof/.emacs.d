;;; windows-20-major-tex.el --- 設定 - Windows - メジャーモード - TeX

;; Copyright (C) 2014-2019 Taku Watabe
;; Time-stamp: <2019-01-09T11:50:38+09:00>

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; デフォルト値
;; ----------------------------------------------------------------------------
(custom-set-variables
 '(tex-suscript-height-ratio 1.0)
 '(tex-run-command "ptex2pdf -u -e -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode\"")
 '(latex-run-command "ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode\"")
 '(tex-bibtex-command "latexmk -e \"$latex=q/uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/makeindex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
 '(tex-compile-commands '(("ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1 -interaction=nonstopmode\" %f" "%f" "%r.pdf"))))

(custom-set-faces
 '(tex-verbatim ((t
                  (:inherit font-lock-string-face)))))
(custom-set-faces
 '(tex-verbatim ((t
                  (:font (or (cdr-safe (assoc 'font default-frame-alist))
                             "Courier New"))))))


;; ----------------------------------------------------------------------------
;; 初期化
;; ----------------------------------------------------------------------------
(eval-after-load 'tex-mode
  '(progn
     (defcustom tex-pdf-print-command (purecopy "pdfopen --rxi --file")
       "Command used by \\[tex-pdf-print] to print a .pdf file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end."
       :type 'string
       :group 'tex-view)

     (defcustom tex-alt-pdf-print-command (purecopy "lpr -d")
       "Command used by \\[tex-pdf-print] with a prefix arg to print a .pdf file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

If two printers are not enough of a choice, you can set the variable
`tex-alt-pdf-print-command' to an expression that asks what you want;
for example,

    (setq tex-alt-pdf-print-command
         '(format \"lpr -P%s\" (read-string \"Use printer: \")))

would tell \\[tex-pdf-print] with a prefix argument to ask you which printer to
use."
       :type '(choice (string :tag "Command")
                      (sexp :tag "Expression"))
       :group 'tex-view)

     (defcustom tex-pdf-view-command (purecopy "rundll32 shell32,ShellExec_RunDLL SumatraPDF -reuse-instance")
       "Command used by \\[tex-pdf-view] to display a `.pdf' file.
If it is a string, that specifies the command directly.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by a space, is added at the end.

If the value is a form, it is evaluated to get the command to use."
       :type 'string
       :group 'tex-view)

     (defun tex-pdf-view ()
       "Preview the last `.pdf' file made by running TeX under Emacs.
This means, made using \\[tex-region], \\[tex-buffer] or \\[tex-file].
The variable `tex-pdf-view-command' specifies the shell command for preview.
You must set that variable yourself before using this command,
because there is no standard value that would generally work."
       (interactive)
       (or tex-pdf-view-command
           (error "You must set `tex-pdf-view-command'"))
       ;; Restart the TeX shell if necessary.
       (or (tex-shell-running)
           (tex-start-shell))
       (let ((tex-pdf-print-command (eval tex-pdf-view-command)))
         (tex-pdf-print)))

     (defun tex-pdf-print (&optional alt)
       "Print the .pdf file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-pdf-print-command'."
       (interactive "P")
       (let ((print-file-name-pdf (tex-append tex-print-file ".pdf"))
             test-name)
         (if (and (not (equal (current-buffer) tex-last-buffer-texed))
                  (buffer-file-name)
                  ;; Check that this buffer's printed file is up to date.
                  (file-newer-than-file-p
                   (setq test-name (tex-append (buffer-file-name) ".pdf"))
                   (buffer-file-name)))
             (setq print-file-name-pdf test-name))
         (if (not (file-exists-p print-file-name-pdf))
             (error "No appropriate `.pdf' file could be found")
           (if (tex-shell-running)
               (tex-kill-job)
             (tex-start-shell))
           (tex-send-command
            (if alt tex-alt-pdf-print-command tex-pdf-print-command)
            print-file-name-pdf
            t))))

     (define-key tex-mode-map (kbd "C-c C-v") #'tex-pdf-view)
     (define-key tex-mode-map (kbd "C-c C-p") #'tex-pdf-print)
     (define-key tex-mode-map (kbd "C-c <kanji>") #'latex-insert-block)))


;; ----------------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8-unix
;; mode: Emacs-Lisp
;; no-byte-compile: t
;; End:

;;; windows-20-major-tex.el ends here
