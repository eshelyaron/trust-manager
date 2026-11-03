;;; trust-manager.el --- Convenient trust management  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <me@eshelyaron.com>
;; Keywords: security trust files
;; URL: https://git.sr.ht/~eshel/trust-manager
;; Package-Version: 0.4.3
;; Package-Requires: ((emacs "30.1"))

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

;; This library provides the `trust-manager-mode' minor mode, which
;; helps you manage trusted directories with minimal configuration.
;; It is intended to streamline the management of `trusted-content',
;; added in Emacs 30 as a new security measure.
;;
;; Just enable `trust-manager-mode' in your init file and you should
;; be good to go.  The mode focuses on per-project trust designation.
;; It asks you whether you trust a project the first time you visit a
;; file in that project, and remembers your choices across sessions.
;; Your trusted/untrusted projects are stored in the user option
;; `trust-manager-trust-alist'.  If you change your mind about some
;; project, just customize this user option; you can do so directly
;; or via the utility command `trust-manager-customize'.  You may also
;; customize `trust-manager-trust-alist' to designate some directories
;; as trusted or untrusted before actually visiting files in them.
;;
;; Another utility command is `trust-manager-set-project-trust', which
;; lets you mark any project as trusted or untrusted, not necessarily
;; the current project.  The command `trust-manager-set-file-trust' is
;; similar, except that it supports arbitrary files/directories,
;; rather than just projects.
;;
;; In addition, `trust-manager-mode' integrates with the command
;; `project-forget-project' such that when you "forget" a project,
;; its directory stops being trusted and its entry in
;; `trust-manager-trust-alist' is cleared.
;;
;; `trust-manager-mode' also integrates with Dired: when the mode is
;; enabled, you can use C-c C-t in a Dired buffer to trust one or more
;; files/directories; similarly, C-c C-u can be used for untrusting.
;;
;; By default, `trust-manager-mode' also adds a mode line indicator in
;; untrusted buffers where risky features may have been disabled.
;; The default indicator is a `?' shown in red.  You can click on the
;; indicator to mark the buffer as trusted (it runs the command
;; `trust-manager-trust-this-buffer', which you can run directly too).
;; You can also customize or disable this indicator via the
;; `trust-manager-untrusted-indicator' user option, and the face with
;; the same name.
;;
;; Since only some features require trust, not every untrusted buffer
;; needs your attention, only those in which the lack of trust matters.
;; The user option `trust-manager-trust-indicator-buffer-condition'
;; controls in which untrusted buffers the indicator is shown.
;; By default it specifies only Emacs Lisp buffers, because several
;; Emacs Lisp editing features, including on-the-fly diagnostics,
;; require trust.
;;
;; When `trust-manager-mode' marks a previously untrusted buffer as
;; trusted, e.g. when you click on the untrusted buffer mode line
;; indicator, it runs the hook `trust-manager-now-trusted-hook'.
;; By default, `trust-manager-mode' uses this hook to re-enable the
;; Emacs Lisp Flymake backend for on-the-fly diagnostics.
;;
;; `trust-manager-mode' can also extend Emacs's trust system to secure
;; additional potentially risky features; the user option
;; `trust-manager-secure-additional-features' says which features
;; `trust-manager-mode' should hook into.  By default, this option is
;; set to integrate trust into Emacs's file-local variables feature,
;; such that file-specified modes and variable values are ignored in
;; untrusted buffers.  This includes `read-symbol-shorthands', which
;; Emacs normally honors even when other file-local variables are
;; disabled, and which an untrusted file could otherwise exploit to run
;; arbitrary code as soon as you visit it.
;; See https://yhetil.org/emacs/jwv4ihscare.fsf-monnier+emacs@gnu.org/

;;; Code:

(defgroup trust-manager nil
  "Trust management."
  :group 'files)

(defvar-local trust-manager--cached-trusted-content 'unset)

(defun trust-manager--cached-trusted-content-p ()
  "Like `trusted-content-p', but caches result in a buffer local variable.
Do not use this function, especially not for security-relevant trust checks.
It's only meant for UI use where stale values are acceptable."
  (if (eq trust-manager--cached-trusted-content 'unset)
      (setq trust-manager--cached-trusted-content (trusted-content-p))
    trust-manager--cached-trusted-content))

(defcustom trust-manager-now-trusted-hook nil
  "Hook `trust-manager-mode' runs when marking an untrusted buffer as trusted.
The newly trusted buffer is current when functions on this hook run."
  :type 'hook
  :package-version '(trust-manager . "0.3.0"))

(defcustom trust-manager-secure-additional-features '(file-local-variables)
  "Additional Emacs features to limit to trusted buffers.
This is a list of symbols, each symbol represents an Emacs feature that
`trust-manager-mode' should hook into and disable in untrusted buffers.

Currently, the only recognized symbol is `file-local-variables'.  If
this symbol is a member of this user option, then `trust-manager-mode'
disables file-specified variable values and modes (file-local variables)
in untrusted buffers, and synchronizes `safe-local-variable-directories'
with `trust-manager-trust-alist'."
  :type '(repeat
          (choice
           (const :tag "File-specified modes and variable values"
                  file-local-variables)))
  :risky t
  :package-version '(trust-manager . "0.4.0"))

(defun trust-manager--set-file-trust (file trust)
  "If TRUST is non-nil, trust FILE; otherwise untrust it."
  (let* ((exp (expand-file-name
               (if (unless (file-remote-p file)
                     ;; Don't open remote connection just to check if
                     ;; FILE is a directory.
                     (file-directory-p file))
                   (file-name-as-directory file)
                 file)))
         (file (abbreviate-file-name exp))
         (curr (delete file (default-value 'trusted-content))))
    (setq-default trusted-content (if trust (cons file curr) curr))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when-let* ((fbuf buffer-file-name)
                    (fbuf (expand-file-name fbuf)))
          (when (or (and (string-suffix-p "/" exp) (string-prefix-p exp fbuf))
                    (equal fbuf exp))
            (let ((old trust-manager--cached-trusted-content))
              (setq trust-manager--cached-trusted-content trust)
              (and trust (not old)
                   (run-hooks 'trust-manager-now-trusted-hook)))))))))

(defun trust-manager--set-files-trust (alist)
  "Apply trust values from ALIST."
  (let ((last-trusted-dir nil))
    (pcase-dolist (`(,file . ,trust)
                   ;; Handle files/subdirectories after a directory
                   ;; that contains them.
                   (sort (mapcar (pcase-lambda (`(,file . ,trust))
                                   (cons (expand-file-name file) trust))
                                 alist)))
      (let ((is-dir (string-suffix-p "/" file)))
        (when (and is-dir (memq 'file-local-variables
                                trust-manager-secure-additional-features))
          (if trust
              (unless (member file safe-local-variable-directories)
                (push file safe-local-variable-directories))
            (setq safe-local-variable-directories
                  (delete file safe-local-variable-directories))))
        (unless (and last-trusted-dir
                     ;; Skip FILE if it's a descendant of a trusted dir,
                     ;; and thus trusted too.
                     (string-prefix-p last-trusted-dir (expand-file-name file)))
          (when (and trust is-dir)
            (setq last-trusted-dir
                  (expand-file-name (file-name-as-directory file))))
          (trust-manager--set-file-trust file trust))))))

(defcustom trust-manager-trust-alist nil
  "Alist mapping file/directory names to boolean trust values.

When `trust-manager-mode' is enabled, it marks files and directories
that appear as keys in this alist as trusted or untrusted according to
their associated values.
This also happens when you customize this user option.

If an entry in this alist says that some directory is trusted, then any
other entry that specifies a file or subdirectory of the trusted
directory is ignored, since it is already implicitly trusted as well."
  :type '(alist :key-type (file :tag "File or Directory")
                :value-type (boolean :tag "Is Trusted"))
  :risky t
  :package-version '(trust-manager . "0.1.0")
  :set (lambda (symbol value)
         (trust-manager--set-files-trust value)
         (set-default-toplevel-value symbol (sort value))))

;;;###autoload
(defun trust-manager-customize ()
  "Customize trusted files and directories."
  (interactive)
  (customize-option 'trust-manager-trust-alist))

;;;###autoload
(defun trust-manager-set-file-trust (file &optional trust)
  "If TRUST is non-nil, trust FILE; otherwise untrust it.

Interactively, prompt for FILE, and set TRUST to non-nil.
With a prefix argument, set TRUST to nil instead."
  (interactive
   (let ((trust (not current-prefix-arg))
         (dir (if default-directory (abbreviate-file-name default-directory) "~/")))
     (list (read-file-name
            (format "%srust file/directory: " (if trust "T" "Unt")) dir dir)
           trust)))
  (let ((abbr (abbreviate-file-name
               (expand-file-name (if (file-directory-p file)
                                     (file-name-as-directory file)
                                   file)))))
    (setf (alist-get abbr trust-manager-trust-alist nil nil #'equal) trust)
    (customize-save-variable 'trust-manager-trust-alist trust-manager-trust-alist)
    (message "Marked `%s' as %strusted" abbr (if trust "" "un"))))

;;;###autoload
(defun trust-manager-set-project-trust (project &optional trust)
  "If TRUST is non-nil, trust PROJECT; otherwise untrust it.

Interactively, prompt for PROJECT, and set TRUST to non-nil.
With a prefix argument, set TRUST to nil instead."
  (interactive
   (let ((trust (not current-prefix-arg))
         (def (when-let* ((pr (project-current))) (project-root pr))))
     (list (completing-read
            (format-prompt "%srust project" def (if trust "T" "Unt"))
            (project-known-project-roots)
            (lambda (root)
              (if-let* ((ent (assoc root trust-manager-trust-alist)))
                  (xor trust (cdr ent))
                t)))
           trust)))
  (trust-manager-set-file-trust project trust))

(defun trust-manager--already-trusted-p (dir)
  "Return non-nil if DIR is trusted according to `trusted-content'."
  (let ((tc (default-value 'trusted-content)))
    (or (eq tc :all)
        (and (listp tc)
             (catch 'ball
               (dolist (par tc)
                 (when (file-in-directory-p dir par)
                   (throw 'ball t))))))))

(defun trust-manager--should-trust-p (pr)
  "Ask and return non-nil if project directory PR should be trusted."
  (let ((name (abbreviate-file-name pr))
        (true (abbreviate-file-name (file-truename pr))))
    (yes-or-no-p
     (substitute-quotes
      (concat
       "Trust project directory `" name "'"
       (unless (equal name true) (format " (truename: `%s')" true))
       "?")))))

(defvar trust-manager--trust-query-function #'trust-manager--should-trust-p)

(put 'trust-manager--trust-query-function 'risky-local-variable t)

(declare-function project-root "project" (project))

(defun trust-manager--check-file ()
  "Check if the current project should be marked as trusted."
  (when-let* ((pc (project-current)))
    (let ((pr (project-root pc)))
      (unless (or
               ;; Already asked about this project.
               (assoc pr trust-manager-trust-alist)
               ;; Already trusted in `trusted-content'.
               (trust-manager--already-trusted-p pr))
        (let ((trust (funcall trust-manager--trust-query-function pr)))
          (trust-manager--set-file-trust pr trust)
          (customize-save-variable
           'trust-manager-trust-alist
           `((,pr . ,trust) . ,trust-manager-trust-alist))
          (message "Marked project directory `%s' as %strusted"
                   pr (if trust "" "un")))))))

(defun trust-manager--set-buffer-trust (&optional buffer trust)
  "If TRUST is non-nil, trust BUFFER; otherwise untrust it."
  (with-current-buffer (or buffer (current-buffer))
    (let ((old (trust-manager--cached-trusted-content-p)))
      (setq-local trusted-content (when trust :all))
      (setq trust-manager--cached-trusted-content trust)
      (and trust (not old) (run-hooks 'trust-manager-now-trusted-hook)))))

(defun trust-manager-trust-this-buffer (&optional event)
  "Mark buffer at EVENT as trusted, defaulting to current buffer."
  (declare
   (completion
    ;; Exclude this command from M-x completions in trusted buffers.
    (lambda (_ buf &rest _)
      (not (eq t (buffer-local-value
                  'trust-manager--cached-trusted-content buf))))))
  (interactive (list last-nonmenu-event))
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (trust-manager--set-buffer-trust nil t)
    (message "Buffer `%s' is now trusted" (current-buffer))
    (force-mode-line-update)))

(defvar-keymap trust-manager--on-trust-indicator-mouse-map
  "<mode-line> <follow-link>" 'mouse-face
  "<mode-line> <mouse-2>"    #'trust-manager-trust-this-buffer)

(defcustom trust-manager-untrusted-indicator "?"
  "String to show in the mode line in untrusted buffers.
This can also be nil, in which case `trust-manager-mode' does not
display any indicator for untrusted buffers."
  :type '(choice string (const :tag "No indicator" nil))
  :risky t
  :package-version '(trust-manager . "0.2.0"))

(defcustom trust-manager-trust-indicator-buffer-condition
  '(derived-mode emacs-lisp-mode)
  "Condition determining in which buffers to show trust indicator.
See `buffer-match-p' for a description of the possible condition values."
  :type 'buffer-predicate
  :risky t
  :package-version '(trust-manager . "0.2.0"))

(defface trust-manager-untrusted-indicator '((t :inherit error))
  "Face for the indicator `trust-manager-mode' shows in untrusted buffers."
  :package-version '(trust-manager . "0.2.0"))

(defvar trust-manager--trust-indicator
  '(:eval
    (when (buffer-match-p
           trust-manager-trust-indicator-buffer-condition
           (current-buffer))
      (if (trust-manager--cached-trusted-content-p) ""
        (concat
         (propertize
          trust-manager-untrusted-indicator
          'face 'trust-manager-untrusted-indicator
          'help-echo "mouse-2: Trust this buffer\nBuffer is UNTRUSTED (some features may be disabled)"
          'mouse-face 'mode-line-highlight
          'follow-link t
          'keymap trust-manager--on-trust-indicator-mouse-map)
         " ")))))

(put 'trust-manager--trust-indicator 'risky-local-variable t)

(defun trust-manager--ensure-extensible-global-mode-string ()
  "Ensure `global-mode-string' can be extended."
  (unless global-mode-string (setq global-mode-string '("")))
  (when (or (atom global-mode-string)
            (when-let* ((sym (car global-mode-string))) (symbolp sym)))
    (setq global-mode-string `("" ,global-mode-string))))

(defun trust-manager--enable-elisp-flymake-backend ()
  "Re-enable `elisp-flymake-byte-compile', if it is disabled."
  (and (bound-and-true-p flymake-mode)
       (fboundp 'flymake-disabled-backends) ; Silence byte-compiler.
       (memq 'elisp-flymake-byte-compile (flymake-disabled-backends))
       (fboundp 'flymake--run-backend)      ; Likewise.
       (flymake--run-backend 'elisp-flymake-byte-compile)))

(defun trust-manager--set-up-for-elisp ()
  "Set up re-enabling `elisp-flymake-byte-compile' when the buffer is trusted."
  (add-hook
   'trust-manager-now-trusted-hook
   #'trust-manager--enable-elisp-flymake-backend nil t))

;;;###autoload
(defun trust-manager-dired (&optional arg)
  "Show currently trusted files and directories in a Dired buffer.
With prefix ARG, show untrusted files and directories instead."
  (interactive "P")
  (if-let* ((entries (seq-keep
                      (pcase-lambda (`(,file . ,curr))
                        (when (xor curr arg) file))
                      trust-manager-trust-alist)))
      (progn
        (let* ((dir (file-name-directory (try-completion "" entries)))
               (dir (if (member dir entries)
                        (file-name-directory (directory-file-name dir))
                      dir))
               (len (length dir)))
          (dired
           (cons dir
                 (mapcar (lambda (file) (substring file len)) entries)))))
    (message
     "No %strusted files/directories in `trust-manager-trust-alist'"
     (if arg "un" ""))))

(declare-function dired-get-marked-files "dired")
(declare-function dired-mark-pop-up      "dired")
(declare-function dired-mark-prompt      "dired")
(declare-function dired-goto-file        "dired")
(defvar dired-no-confirm)

(defun trust-manager--dired-set-trust (arg &optional trust)
  "If TRUST is non-nil, trust marked files; otherwise untrust them.

ARG should be the prefix argument of the command that calls this
function, it is passed to `dired-get-marked-files', which see."
  (let ((sym (if trust 'trust 'untrust))
        (files (dired-get-marked-files t arg nil nil t)))
    (when (or (eq dired-no-confirm t) (memq sym dired-no-confirm)
              (dired-mark-pop-up
               nil sym files #'yes-or-no-p
               (concat (capitalize (symbol-name sym))
                       " " (dired-mark-prompt arg files) "? ")))
      (save-excursion
        (dolist (file files)
          (trust-manager-set-file-trust file trust)
          (dired-goto-file (expand-file-name file))
          (let ((inhibit-read-only t))
            (beginning-of-line)
            (delete-char 1)
            (insert (if trust "T" "U")))))
      (when (cdr files)
        (message "Marked %d files/directories as %sed" (length files) sym)))))

(defun trust-manager-dired-do-trust (&optional n)
  "Trust marked files/directories.
With a numeric prefix argument N, trust the N files following point."
  (interactive "P" dired-mode)
  (trust-manager--dired-set-trust n t))

(defun trust-manager-dired-do-untrust (&optional n)
  "Untrust marked files/directories.
With a numeric prefix argument N, untrust the N files following point."
  (interactive "P" dired-mode)
  (trust-manager--dired-set-trust n))

(defvar-keymap trust-manager-dired-mode-map
  "C-c C-t" #'trust-manager-dired-do-trust
  "C-c C-u" #'trust-manager-dired-do-untrust)

(define-minor-mode trust-manager-dired-mode
  "Minor mode integrating `trust-manager' into Dired."
  :interactive (dired-mode))

(defun trust-manager--forget-project (root)
  "Remove trust setting for project ROOT from `trust-manager-trust-alist'."
  (setf (alist-get root trust-manager-trust-alist nil t #'equal) nil)
  (trust-manager--set-file-trust root nil)
  (customize-save-variable 'trust-manager-trust-alist trust-manager-trust-alist))

(defun trust-manager--normal-mode-wrapper
    (fun &optional respect-e-l-v &rest _)
  "Disable file-specified variables/modes in untrusted buffers.

Call FUN (the original `normal-mode' function) with
`enable-local-variables' set according to `trusted-content-p'.

In untrusted buffers, also remove `read-symbol-shorthands' from
`permanently-enabled-local-variables', to mitigate the arbitrary code
execution vulnerability demonstrated here:
https://yhetil.org/emacs/jwv4ihscare.fsf-monnier+emacs@gnu.org/

Argument RESPECT-E-L-V non-nil tells `normal-mode' to respect
`enable-local-variables'; if the buffer is untrusted, unconditionally
override RESPECT-E-L-V with non-nil."
  (let* ((trust (or
                 ;; `byte-compile-file' performs compilation in a work
                 ;; buffer, where it sets `buffer-file-name' but fails
                 ;; to set `buffer-file-truename'.  So in that context
                 ;; `trusted-content-p' is nil even for trusted files.
                 ;; We work around that incoherence by always treating
                 ;; the file being compiled as trusted, which is safe
                 ;; because compiling a file already implies trust.
                 ;; This is essential because we suppress symbol
                 ;; shorthands in untrusted buffers, which would break
                 ;; compilation.
                 (and (bound-and-true-p byte-compile-current-file)
                      (equal buffer-file-name byte-compile-current-file))
                 (trusted-content-p)))
         (enable-local-variables (and trust enable-local-variables))
         (permanently-enabled-local-variables
          (if trust permanently-enabled-local-variables
            ;; TODO: Consider validating/sanitizing untrusted symbol
            ;; shorthands, instead of disabling them altogether as we
            ;; do here.
            (remq 'read-symbol-shorthands permanently-enabled-local-variables))))
    (funcall fun (or (not trust) respect-e-l-v))))

(defun trust-manager--trust-scratch-buffer ()
  "Ensure the *scratch* buffer is trusted."
  (when-let* ((sb (get-buffer "*scratch*")))
    (with-current-buffer sb
      (and (derived-mode-p 'lisp-interaction-mode)
           (let ((str (buffer-string)))
             (or (string-empty-p str)
                 (equal (buffer-string)
                        (substitute-command-keys initial-scratch-message))))
           (not (trusted-content-p))
           (trust-manager--set-buffer-trust nil t)))))

;;;###autoload
(define-minor-mode trust-manager-mode
  "Toggle per-project trust management with Trust Manager minor mode.

When you enable Trust Manager mode, it marks some standard files as
trusted by adding them to `trusted-content', and sets up a hook that asks
you whether you trust a project the first time you visit a file in that
project.  Your answers are persisted in the `trust-manager-trust-alist'
user option, which you may also customize directly.

The standard files that this mode adds to `trusted-content' are your
`user-init-file', `early-init-file', `custom-file' and all directories
in your `load-path'.  In addition, it ensures that the *scratch* buffer
is trusted if its contents are unchanged.

If you later disable this mode, it removes the hook that asks you about
project trust, but it does not mark any file or directory as untrusted."
  :group 'files
  :global t
  :package-version '(trust-manager . "0.1.0")
  (when trust-manager-untrusted-indicator
    (trust-manager--ensure-extensible-global-mode-string))
  (if trust-manager-mode
      (progn
        (dolist (fn `(,user-init-file
                      ,early-init-file
                      ,custom-file
                      . ,load-path))
          (and fn (file-name-absolute-p fn)
               (trust-manager--set-file-trust fn t)))
        (trust-manager--set-files-trust trust-manager-trust-alist)
        (if after-init-time (trust-manager--trust-scratch-buffer)
          (add-hook 'emacs-startup-hook #'trust-manager--trust-scratch-buffer))
        (add-hook 'find-file-hook #'trust-manager--check-file)
        (add-hook 'emacs-lisp-mode-hook #'trust-manager--set-up-for-elisp)
        (add-hook 'dired-mode-hook #'trust-manager-dired-mode)
        (advice-add 'project-forget-project
                    :before #'trust-manager--forget-project)
        (when (memq 'file-local-variables
                    trust-manager-secure-additional-features)
          (advice-add 'normal-mode
                      :around #'trust-manager--normal-mode-wrapper))
        (or (null trust-manager-untrusted-indicator)
            (memq 'trust-manager--trust-indicator global-mode-string)
            (setq global-mode-string
                  (append global-mode-string '(trust-manager--trust-indicator)))))
    (remove-hook 'find-file-hook #'trust-manager--check-file)
    (remove-hook 'emacs-lisp-mode-hook #'trust-manager--set-up-for-elisp)
    (remove-hook 'dired-mode-hook #'trust-manager-dired-mode)
    (advice-remove 'project-forget-project #'trust-manager--forget-project)
    (advice-remove 'normal-mode #'trust-manager--normal-mode-wrapper)
    (setq global-mode-string
          (delq 'trust-manager--trust-indicator global-mode-string))))

(provide 'trust-manager)
;;; trust-manager.el ends here
