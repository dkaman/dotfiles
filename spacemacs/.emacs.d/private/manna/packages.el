;;; packages.el --- manna layer packages file for Spacemacs.
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
;; added to `manna-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `manna/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `manna/pre-init-PACKAGE' and/or
;;   `manna/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst manna-packages
  '(org)
  "The list of Lisp packages required by the manna layer.

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


(defun manna/post-init-org ()

  ;; locations
  (setq org-directory (file-name-as-directory manna/directory))
  (setq manna/top-level-file (f-expand manna/top-level-file-name org-directory))
  (setq manna/categories-directory (file-name-as-directory (f-expand "categories" org-directory)))
  (setq manna/templates-directory (file-name-as-directory (f-expand "templates" org-directory)))

  (setq org-agenda-files `(,org-directory
                           ,manna/categories-directory))

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
        `(("t" "task" entry (file+headline manna/top-level-file "inbox")
           "* TODO %?"
           :kill-buffer t)
          ("p" "project" entry (file+headline manna/top-level-file "projects")
           "* %? %^g\n%(org-clock-report)"
           :kill-buffer t)
          ("c" "category files")
          ("cg" "gtd file" plain (file manna/get-category-file)
           ,(manna/get-capture-template "gtd")
           :kill-buffer t)
          ("ck" "knowledge base" plain (file manna/get-category-file)
           ,(manna/get-capture-template "kb")
           :kill-buffer t)
          ("cj" "journal" entry (file+olp+datetree manna/get-category-file)
           ,(manna/get-capture-template "journal")
           :kill-buffer t)
          ("cs" "stream of consciousness" plain (file manna/get-category-file)
           ,(manna/get-capture-template "soc")
           :kill-buffer t)))

  ;; --- hooks ---

  ;; Set up global capturing
  (evil-leader/set-key "oc" 'org-capture)

  ;; And global agenda-ing
  (evil-leader/set-key "oa" 'org-agenda)

  ;; globally open my todo file
  (evil-leader/set-key "of" 'manna/find-top-level-file)

  ;; toggle columns with ,<TAB> or <SPC>m<TAB>
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "TAB" 'org-columns)

  ;; some org mode specific bindings to make
  (after 'org
    (define-key org-mode-map (kbd "C-j") 'org-forward-heading-same-level)
    (define-key org-mode-map (kbd "C-k") 'org-backward-heading-same-level)
    (define-key org-mode-map (kbd "C-h") 'outline-up-heading)
    (define-key org-mode-map (kbd "C-l") 'outline-next-visible-heading)))

;;; packages.el ends here
