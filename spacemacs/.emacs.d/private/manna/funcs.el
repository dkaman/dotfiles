;;; funcs.el --- manna Layer functions File for Spacemacs
;;
;;
;; Author: dallas kaman (dallas.kaman@gmail.com)
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun manna/find-top-level-file ()
  "interactive function that will jump directly to the top level manna file.
this function is meant to be bound to a key (bound to SPC o f by default)"
  (interactive)
  (find-file manna/top-level-file))

(defun manna/get-category-file ()
  "this function is invoked by the org capture system to decide where to
put a new category file. this function should almost certainly check for
name collisions but does not yet."
  (read-file-name "category-file: " manna/categories-directory))

(defun manna/create-file-link-with-basename (filepath)
  "this function takes a pathname, and creates an org link where the link
target is the file path, and the link text is the basename of the file without
its extention

example:
~/org/test.org => [[~/org/test.org][test]]\n"
  (format "[[%s][%s]]\n" filepath (f-base filepath)))

(defun manna/build-index-list ()
  "this function maps the link creation function over all org files in
the categories directory. this will create a newline separated list of links
to be used in the construction of the index heading of the top-level file."
  (mapcar
   'manna/create-file-link-with-basename
   (directory-files-recursively manna/categories-directory "^[^.].*\.org$")))

(defun manna/get-capture-template (type)
  (let ((template-file-name (concat type ".org.template")))
    (with-temp-buffer
      (insert-file-contents (f-join manna/templates-directory template-file-name))
      (buffer-string))))

(defun manna/insert-clock-table (heading)
  (org-element-adopt-elements heading '(paragraph (:raw-value "lol"))))

(defun manna/convert-todo-to-project ()
  (interactive)
  (let* ((heading (org-element-at-point))
         (end-of-heading (org-element-property :contents-end heading)))
    (princ end-of-heading)
    (org-todo "")
    (goto-char end-of-heading)
    (insert "\n")
    (manna/insert-clock-table heading)))

(defun manna/last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

(defun  manna/current-date-to-quarter ()
  "for use in archive file naming, this function returns a date in the form
'YYYY-q#' corresponding to the current year and quarter."
  (format-time-string "%Y-q%q" (current-time)))

(defun manna/process-children ()
  "Non-interactive function to process direct children of the current Org subtree.
For each child, ask the user whether to refile, archive, or skip it.
After processing all children, return to the parent subtree."
  (let ((parent-pos (point))) ; Save the position of the parent node
    (save-excursion
      (condition-case err
          (progn
            (goto-char parent-pos)
            (org-narrow-to-subtree) ; Narrow to the current subtree for direct child processing
            (goto-char (point-min)) ; Move to the beginning of the subtree
            (outline-next-heading) ; Move to the first child
            (while (and (not (eobp)) (org-at-heading-p)) ; Iterate over all headings
              (let ((action (read-char-choice
                             (format "Process \"%s\": [r]efile, [a]rchive, [s]kip? "
                                     (org-get-heading t t t t))
                             '(?r ?a ?s))))
                (cond
                 ((eq action ?r)
                  (call-interactively #'org-refile))
                 ((eq action ?a)
                  (call-interactively #'org-archive-subtree-default))
                 ((eq action ?s)
                  (outline-next-heading))))) ; Move to the next child only if skipping
            (widen)) ; Restore the original buffer visibility
        (error
         (widen) ; Ensure the buffer is widened if an error occurs
         (signal (car err) (cdr err))))) ; Rethrow the error after widening
    ;; Return point to the parent node
    (goto-char parent-pos)
    (org-show-entry))) ; Ensure the parent node is visible

(defun manna/process-inbox (file)
  "Process the 'inbox' heading in the specified Org file.
FILE is the path to the Org file. This function locates the 'inbox' heading
and processes its children using `manna/process-children`."
  (let ((absolute-file (expand-file-name file))) ; Resolve the absolute path
    (with-current-buffer (find-file-noselect absolute-file)
      (goto-char (point-min)) ; Go to the beginning of the file
      (if (re-search-forward "^\\*+ inbox\\b" nil t) ; Locate the "inbox" heading
          (progn
            (goto-char (match-beginning 0)) ; Move to the start of the "inbox" heading
            (manna/process-children)) ; Process the children
        (message "No 'inbox' heading found in the file.")))))

(defun manna/process-inbox-top-level ()
  (interactive)
  (manna/process-inbox manna/top-level-file))
