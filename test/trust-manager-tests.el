;;; trust-manager-tests.el --- Tests for trust-manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'project)
(require 'flymake)
(require 'trust-manager)

(cl-defstruct (trust-manager-tests--project
               (:constructor trust-manager-tests--project (root)))
  root)

(cl-defmethod project-root ((p trust-manager-tests--project))
  (trust-manager-tests--project-root p))

(ert-deftest trust-manager-tests--visit-file-say-yes ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.el" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content nil)
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (queried nil)
               (trust-manager--trust-query-function
                (lambda (_) (setq queried t)))
               (inhibit-message t))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (should queried)
            (should (with-current-buffer buf (trusted-content-p)))
            (kill-buffer buf)))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--visit-file-say-no ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.el" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content nil)
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (queried nil)
               (trust-manager--trust-query-function
                (lambda (_) (setq queried t) nil))
               (inhibit-message t))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (should queried)
            (should-not (with-current-buffer buf (trusted-content-p)))
            (kill-buffer buf)))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--visit-file-ancestor-already-trusted ()
  (let* ((parent (make-temp-file "trust-mgr-test" t))
         (d (expand-file-name "proj/" parent))
         (f (expand-file-name "foo.el" d)))
    (make-directory d)
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content
                (list (abbreviate-file-name
                       (file-name-as-directory (expand-file-name parent)))))
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (queried nil)
               (trust-manager--trust-query-function
                (lambda (_) (setq queried t)))
               (inhibit-message t))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (should-not queried)
            (should (with-current-buffer buf (trusted-content-p)))
            (kill-buffer buf)))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory parent t)))))

(ert-deftest trust-manager-tests--visit-say-yes-then-forget ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (p (expand-file-name "projects.eld" d))
         (f (expand-file-name "foo.el" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content nil)
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (trust-manager--trust-query-function #'always)
               (inhibit-message t)
               (project-list-file p))
          (should-not (file-exists-p project-list-file))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (should (trusted-content-p))
                  (project-forget-project d)
                  (should-not (trusted-content-p))
                  (should-not trust-manager--cached-trusted-content))
              (kill-buffer buf))))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t))
      (ignore-errors (delete-file p)))))

(ert-deftest trust-manager-tests--trust-this-buffer-enables-flymake ()
  (let* ((trusted-content nil)
         (trust-manager-trust-alist nil)
         (user-init-file nil)
         (inhibit-message t))
    (unwind-protect
        (with-temp-buffer
          (trust-manager-mode)
          (emacs-lisp-mode)
          (flymake-mode)
          (flymake-start)
          (should-not (trusted-content-p))
          (should (memq 'elisp-flymake-byte-compile (flymake-disabled-backends)))
          (trust-manager--set-buffer-trust nil t)
          (should (trusted-content-p))
          (should-not (memq 'elisp-flymake-byte-compile (flymake-disabled-backends)))
          (should (memq 'elisp-flymake-byte-compile (flymake-running-backends))))
      (trust-manager-mode -1))))

(ert-deftest trust-manager-tests--file-local-vars-untrusted ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.txt" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content nil)
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (trust-manager--trust-query-function (lambda (_) nil))
               (trust-manager-secure-additional-features '(file-local-variables))
               (enable-local-variables t)
               (inhibit-message t))
          (with-temp-file f
            (insert "hello\n\nLocal Variables:\nfill-column: 999\nEnd:\n"))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (should-not (trusted-content-p))
                  (should-not (= fill-column 999)))
              (kill-buffer buf))))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--file-local-vars-trusted ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.txt" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content (list (abbreviate-file-name
                                       (file-name-as-directory d))))
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (trust-manager-secure-additional-features '(file-local-variables))
               (enable-local-variables t)
               (inhibit-message t))
          (with-temp-file f
            (insert "hello\n\nLocal Variables:\nfill-column: 999\nEnd:\n"))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (should (trusted-content-p))
                  (should (= fill-column 999)))
              (kill-buffer buf))))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--read-symbol-shorthands-untrusted ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.el" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content nil)
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (trust-manager--trust-query-function (lambda (_) nil))
               (trust-manager-secure-additional-features '(file-local-variables))
               (enable-local-variables t)
               (inhibit-message t))
          (with-temp-file f
            (insert ";; -*- lexical-binding: t; -*-\n\n"
                    ";; Local Variables\:\n"
                    ";; read-symbol-shorthands: ((\"vc-cvs-registered\" . \"load\"))\n"
                    ";; End\:\n"))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (should-not (trusted-content-p))
                  (should-not read-symbol-shorthands)
                  ;; With the shorthand active this would be `eq' to `load'.
                  (should-not (eq (intern "vc-cvs-registered") 'load)))
              (kill-buffer buf))))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--read-symbol-shorthands-trusted ()
  "Trusted files still get `read-symbol-shorthands' applied."
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.el" d)))
    (unwind-protect
        (let* ((project-find-functions
                `(,(lambda (_) (trust-manager-tests--project d))))
               (trusted-content (list (abbreviate-file-name
                                       (file-name-as-directory d))))
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (trust-manager-secure-additional-features '(file-local-variables))
               (enable-local-variables t)
               (inhibit-message t))
          (with-temp-file f
            (insert ";; -*- lexical-binding: t; -*-\n\n"
                    ";; Local Variables\:\n"
                    ";; read-symbol-shorthands: ((\"foo-\" . \"bar-\"))\n"
                    ";; End\:\n"))
          (trust-manager-mode)
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (should (trusted-content-p))
                  (should (equal read-symbol-shorthands '(("foo-" . "bar-")))))
              (kill-buffer buf))))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--byte-compile-file-shorthands ()
  "`byte-compile-file' honors `read-symbol-shorthands' in trusted files."
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (f (expand-file-name "foo.el" d))
         (sym 'trust-manager-tests--sh-func))
    (unwind-protect
        (let* ((trusted-content (list (abbreviate-file-name
                                       (file-name-as-directory d))))
               (trust-manager-trust-alist nil)
               (user-init-file nil)
               (trust-manager-secure-additional-features '(file-local-variables))
               (inhibit-message t))
          (with-temp-file f
            (insert ";; -*- lexical-binding: t; -*-\n\n"
                    "(defun tmt-sh-func () 42)\n\n"
                    ";; Local Variables\:\n"
                    ";; read-symbol-shorthands: ((\"tmt-\" . \"trust-manager-tests--\"))\n"
                    ";; End\:\n"))
          (trust-manager-mode)
          (should (byte-compile-file f))
          (load (byte-compile-dest-file f) nil t)
          (should (fboundp sym))
          (should-not (fboundp (intern-soft "tmt-sh-func"))))
      (trust-manager-mode -1)
      (fmakunbound sym)
      (ignore-errors (delete-directory d t)))))

(ert-deftest trust-manager-tests--safe-local-variable-directories-trusted ()
  (let* ((d (make-temp-file "trust-mgr-test" t))
         (d/ (file-name-as-directory d)))
    (unwind-protect
        (let* ((trust-manager-trust-alist `((,d/ . t)))
               (trust-manager-secure-additional-features '(file-local-variables))
               (safe-local-variable-directories nil)
               (user-init-file nil)
               (inhibit-message t))
          (trust-manager-mode)
          (should (member (expand-file-name d/) safe-local-variable-directories)))
      (trust-manager-mode -1)
      (ignore-errors (delete-directory d t)))))

(provide 'trust-manager-tests)
;;; trust-manager-tests.el ends here
