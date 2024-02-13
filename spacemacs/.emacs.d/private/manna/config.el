;;; config.el --- manna configuration File for Spacemacs
;;
;; Author: dallas kaman (dallas.kaman@gmail.com)
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar manna/directory "~/org"
  "this directory will be used to store everything related to the system.
org-directory will be set to this value after being normalized")

(defvar manna/archive-directory (f-expand ".archive" org-directory))

(defvar manna/top-level-file-name "dallas.org"
  "this is the base name of the top-level manna file. this is where the
index and global inbox will be created.")

(defvar manna/org-custom-daily-agenda
  `((tags-todo "*"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                (org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "important tasks without a date\n")))
    (agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\ntoday's agenda\n")))
    (todo "WAIT"
          ((org-agenda-block-separator nil)
           (org-agenda-overriding-header "\nblocked tasks\n")))
    (todo "NEXT"
          ((org-agenda-block-separator nil)
           (org-agenda-overriding-header "\nnext actions\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nNext three days\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))
