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

  ;; returns 'q' + the number of the current quarter
  (defun djk/get-current-quarter ()
    (let ((month-number (string-to-int (format-time-string "%m"))))
      (cond ((and (>= month-number 1) (<= month-number 3))
             (concat "q" (int-to-string 1)))
            ((and (>= month-number 4) (<= month-number 6))
             (concat "q" (int-to-string 2)))
            ((and (>= month-number 7) (< month-number 9))
             (concat "q" (int-to-string 3)))
            ((and (>= month-number 10) (< month-number 12))
             (concat "q" (int-to-string 4))))))

  ;; function run by the state change hook to auto-schedule tasks
  ;; based on the change from any state to SCHEDULED
  (defun djk/schedule-task-on-state-change ()
    (when (string= org-state "SCHEDULED")
      (progn
        (call-interactively 'org-schedule)
        (call-interactively 'org-set-effort))))

  ;; helper function to allow me to write out capture
  ;; templates as a list of strings
  (defun djk/newline-template (string-list)
    (mapconcat 'identity string-list "\n"))

  ;; helper function to prepend any file/directory
  ;; name with my org directory
  (defun djk/add-org-dir (file)
    (concat org-directory file))

  ;; construct the filename of my quarterly org files, named yyyy-q<quarter>.org
  (defun djk/get-quarterly-filename ()
    (djk/add-org-dir
     (concat (format-time-string "%Y") "-" (djk/get-current-quarter) ".org")))

  ;; open the quarterly file
  (defun djk/find-quarterly-file ()
    (interactive)
    (find-file (djk/get-quarterly-filename)))

  ;; ":PROPERTIES:"
  ;; ":EFFORT: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00|8:00}"
  ;; ":END:"
  ;; --- variables ---
  ;; Default base task template
  (defvar djk/org-basic-task-template
    (djk/newline-template
     '("* TODO %?")))

  (defvar djk/org-basic-project-template
    (djk/newline-template
     '("* %? %^g"
       "%(org-clock-report)")))

  (setq org-agenda-files (list (djk/get-quarterly-filename)))

  ;; --- setq ---
  ;; todo states along with actions for each
  ;; currently, saving notes after cancelling and blocking a task
  (setq org-todo-keywords
        '((sequence "TODO(t)" "SCHEDULED(s)" "WAITING(w@/!)" "|" "CANCELLED(c@)" "DONE(d)")))

  ;; colors set up for todo states
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("SCHEDULED" . "cyan")
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
        `(("t" "Task" entry
           (file+headline (djk/get-quarterly-filename) "inbox")
           ,djk/org-basic-task-template
           :kill-buffer t)
          ("p" "Project" entry
           (file+headline (djk/get-quarterly-filename) "projects")
           ,djk/org-basic-project-template
           :kill-buffer t)))

  ;; --- hooks ---
  (add-hook 'org-after-todo-state-change-hook 'djk/schedule-task-on-state-change)

  ;; Set up global capturing
  (evil-leader/set-key "oc" 'org-capture)
  ;; And global agenda-ing
  (evil-leader/set-key "oa" 'org-agenda)
  ;; globally open my todo file
  (evil-leader/set-key "of" 'djk/find-quarterly-file)
  ;; toggle columns with ,<TAB> or <SPC>m<TAB>
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "TAB" 'org-columns)

  ;; some org mode specific bindings to make
  (after 'org
    (define-key org-mode-map (kbd "C-j") 'org-forward-heading-same-level)
    (define-key org-mode-map (kbd "C-k") 'org-backward-heading-same-level)
    (define-key org-mode-map (kbd "C-h") 'outline-up-heading)
    (define-key org-mode-map (kbd "C-l") 'outline-next-visible-heading)))

;;; packages.el ends here
