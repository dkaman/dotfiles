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
