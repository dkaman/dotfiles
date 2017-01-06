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
  (setq org-directory "~/org/")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "SCHEDULED(s)" "WAITING(w@/!)" "|" "DONE(d)")))

  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("SCHEDULED" . "cyan")
          ("WAITING" . "yellow")
          ("DONE" . "green")))

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (defun djk/newline-template (string-list)
    (mapconcat 'identity string-list "\n"))

  (defun djk/add-org-dir (file)
    (concat org-directory file))

  (defun djk/find-organizer ()
    (interactive)
    (find-file (concat org-directory "organizer.org")))

  (defun djk/schedule-task-on-state-change ()
    (when (string= org-state "SCHEDULED")
      (call-interactively 'org-schedule)))

  (add-hook 'org-after-todo-state-change-hook 'djk/schedule-task-on-state-change)

  ;; Default base task template
  (defvar djk/org-basic-task-template
    (djk/newline-template
     '("* TODO %?"
       ":PROPERTIES:"
       ":EFFORT: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00|8:00}"
       ":END:"
       "Captured %<%Y-%m-%d %H:%M>"
       ""
       "%i")))

  (defvar djk/org-basic-project-template
    (djk/newline-template
     '("* %?"
       ":PROPERTIES:"
       ":END:"
       "** inbox"
       "** blocked"
       "** recurring")))

  ;; Capture templates
  (setq org-capture-templates
        `(("t" "Task Entry" entry
           (file+headline (djk/add-org-dir "organizer.org") "Inbox")
           ,djk/org-basic-task-template
           :kill-buffer t)
          ("j" "Journal Entry" entry
           (file+datetree (djk/add-org-dir "journal.org"))
           "* %<%H:%M> %?"
           :kill-buffer t)
          ("p" "Project Entry" entry
           (file (djk/add-org-dir "projects.org"))
           ,djk/org-basic-project-template
           :kill-buffer t)))

  ;; Set up global capturing
  (evil-leader/set-key "oc" 'org-capture)
  ;; And global agenda-ing
  (evil-leader/set-key "oa" 'org-agenda)
  ;; Quick, get my top level file
  (evil-leader/set-key "oo" 'djk/find-organizer)

  ;; Agenda files
  (setq org-agenda-files
        (mapcar 'djk/add-org-dir '("organizer.org"
                                   "projects.org")))
  ;; Org Refile targets
  (setq org-refile-targets
        '((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9))))

;;; packages.el ends here
