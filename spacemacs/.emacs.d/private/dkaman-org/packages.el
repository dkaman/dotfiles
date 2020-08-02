;;; packages.el --- dkaman-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Dallas Kaman <dkaman@dkaman-box.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `dkaman-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `dkaman-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `dkaman-org/pre-init-PACKAGE' and/or
;;   `dkaman-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst dkaman-org-packages
  '(org)
  "The list of Lisp packages required by the dkaman-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun dkaman-org/post-init-org ()
  ;; Org locations
  (setq org-directory "~/org")

  (setq djk/manna-file (concat org-directory "/dallas.org"))

  (setq djk/manna-categories-directory (concat org-directory "/categories"))

  ;; helper function to allow me to write out capture
  ;; templates as a list of strings
  (defun djk/newline-template (string-list)
    (mapconcat 'identity string-list "\n"))

  ;; open the quarterly file
  (defun djk/find-manna-file ()
    (interactive)
    (find-file djk/manna-file))

  (defun djk/get-category-file ()
    (read-file-name "category-file: " djk/manna-categories-directory))

  (defun djk/build-index-list ()
    (mapcar
     (lambda (x) (format "[[%s][%s]]\n" x (file-relative-name x ".")))
     (directory-files-recursively org-directory "^[^.].*\.org$")))

  ;; --- variables ---
  ;; Default base task template
  (defvar djk/org-task-template
    (djk/newline-template
     '("* TODO %?")))

  (defvar djk/org-project-template
    (djk/newline-template
     '("* %? %^g"
       "%(org-clock-report)")))

  (defvar djk/org-category-gtd-template
    (djk/newline-template
     '("* inbox"
       "* projects"
       "* references")))

  (defvar djk/org-category-kb-template
    (djk/newline-template
     '("* notes"
       "* links")))

  (defvar djk/org-category-journal-template
    (djk/newline-template
     '("* %T %?")))

  (defvar djk/org-category-soc-template
    (djk/newline-template
     '("")))

  (setq org-agenda-files '("~/org"))

  ;; --- setq ---
  ;; todo states along with actions for each
  ;; currently, saving notes after cancelling and blocking a task
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "CANCELLED(c@)" "DONE(d)")))

  ;; colors set up for todo states
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("NEXT" . "cyan")
          ("WAITING" . "yellow")
          ("CANCELLED" . "red")
          ("DONE" . "green")))

  ;; if you use S-cursor, it will bypass logging for that state change
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; org refile targets
  (setq org-refile-targets
        '((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9)))

  ;; archiving target
  (setq org-archive-location "~/org/archive.org::datetree/")

  ;; capture templates
  (setq org-capture-templates
        `(("t" "task" entry (file+headline djk/manna-file "inbox")
           ,djk/org-task-template
           :kill-buffer t)
          ("p" "project" entry (file+headline djk/manna-file "projects")
           ,djk/org-project-template
           :kill-buffer t)
          ("c" "category files")
          ("cg" "gtd file" plain (file djk/find-category-file)
           ,djk/org-category-gtd-template
           :kill-buffer t)
          ("ck" "knowledge base" plain (file djk/find-category-file)
           ,djk/org-category-kb-template
           :kill-buffer t)
          ("cj" "journal" plain (file+olp+datetree djk/find-category-file)
           ,djk/org-category-journal-template
           :kill-buffer t)
          ("cs" "stream of consciousness" plain (file djk/find-category-file)
           ,djk/org-category-soc-template
           :kill-buffer t)))

  ;; --- hooks ---

  ;; Set up global capturing
  (evil-leader/set-key "oc" 'org-capture)

  ;; And global agenda-ing
  (evil-leader/set-key "oa" 'org-agenda)

  ;; globally open my todo file
  (evil-leader/set-key "of" 'djk/find-manna-file)

  ;; toggle columns with ,<TAB> or <SPC>m<TAB>
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "TAB" 'org-columns)

  ;; some org mode specific bindings to make
  (after 'org
    (define-key org-mode-map (kbd "C-j") 'org-forward-heading-same-level)
    (define-key org-mode-map (kbd "C-k") 'org-backward-heading-same-level)
    (define-key org-mode-map (kbd "C-h") 'outline-up-heading)
    (define-key org-mode-map (kbd "C-l") 'outline-next-visible-heading)))

;;; packages.el ends here
