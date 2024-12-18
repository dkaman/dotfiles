
(mapc #'(lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode
        tool-bar-mode
        scroll-bar-mode))

(let ((default-directory "~/.emacs.d/"))       ;for easy
  (normal-top-level-add-to-load-path '("."))   ;recursive
  (normal-top-level-add-subdirs-to-load-path)) ;loading

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defmacro autoload-from-package (package functions)
  "From PACKAGE (string), autoload FUNCTIONS (list)."
  (declare (indent defun))
  `(mapc #'(lambda (fn) (autoload fn ,package nil t))
         ,functions))

(defmacro require-package (packages)
  "Require PACKAGES (list) quietly."
  (declare (indent defun))
  `(mapc #'(lambda (package) (require package nil 'noerror))
         ,packages))

(defmacro djk/toggle-fullscreen-buffer (win-register toggled-mode-test toggle-command
                                                     &optional
                                                     toggle-command-test
                                                     clear-command)
  "Bring up a temporary buffer in fullscreen mode, or restore the
previous window configuration.

WIN-REGISTER         is the register to store the old window configuration in.

TOGGLED-MODE-TEST    is the major mode of the toggled state, in other words a
                     test to determine which way to toggle the buffers.

TOGGLE-COMMAND       is the command to run when toggling into the temporary
                     state.

CLEAR-COMMAND        is an optional command to run when reverting back to the
                     original state; i.e. toggle a flag"
  (declare (indent defun))
  `(progn
     (if ,toggled-mode-test
         (progn (jump-to-register ,win-register)
                (when (not (equal nil ,clear-command))
                  ,clear-command))
       (window-configuration-to-register ,win-register)
       ,toggle-command
       (delete-other-windows))))

(when (require 'package nil 'noerror)
  (setq package-user-dir "~/.emacs.d/elpa/")
  (mapc #'(lambda (source)
            (add-to-list 'package-archives source) t)
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(setq compile-command "make")
(load-theme 'wombat)
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(set-buffer-file-coding-system 'unix)   ;Unix mode. Always
(setq c-default-style "linux"
      c-basic-offset 2                  ;Fix the GNU tabbing default
      ido-create-new-buffer 'always
      require-final-newline 'visit-save ;compliance
      indent-tabs-mode nil
      comment-style 'indent)
(push '("." . "~/.emacs.d/.emacs-backups") backup-directory-alist)
(setq backup-by-copying-when-linked t
    backup-by-copying-when-mismatch t)

(after 'ido
  (setq ido-everywhere t                             ;always Ido
        ido-enable-flex-matching t                   ;smarter Ido
        ido-create-new-buffer 'always                ;quieter Ido
        ido-file-extensions-order '(".org" ".txt"))) ;precedence

(autoload-from-package "ido-config"
  '(ido-recentf-open
    ido-goto-symbol))
(ido-mode t)

(require 'ido)      
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a function in the buffer using
   ido. Functions are defined by the active minor mode."
  (interactive)
  (defvar symbol-names)
  (defvar name-and-pos)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          name-and-pos symbol-names position)
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (push-mark nil t nil)         ;set the mark
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(defun djk/raise-magit-status ()
  "Bring up a full-screen magit-status or restore previous
window configuration."
  (interactive)
  (djk/toggle-fullscreen-buffer
    :magit-fullscreen
    (string= "magit-status-mode" major-mode)
    (progn (if (not (buffer-file-name))
               (message "Buffer not associated with a file")
             (save-buffer)
             (magit-status (file-name-directory (buffer-file-name)))))))

(defun djk/bury-compilation-buffer-if-successful (buffer string)
  "Bury the compilation buffer after a successful compile."
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not (search-forward "warning" nil t)))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)
    (message "Compilation successful.")))
(add-hook 'compilation-finish-functions
          'djk/bury-compilation-buffer-if-successful)

(after 'midnight                        ;we gonna let it all hang out
  (midnight-delay-set 'midnight-delay "5:00am"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(setq org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-hierarchical-todo-statistics     nil
      org-hierarchical-checkbox-statistics nil
      org-src-fontify-natively t
      org-directory "~/org") 

(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "/home/dallas/Documents/EE460N/res/plantuml.jar"))

(setq org-default-notes-file (concat org-directory "capture.org")) 
(evil-leader/set-key "oc" 'org-capture) 
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+datetree (concat org-directory "/todo.org") "Tasks")
         "* TODO %?\n  %a")

        ("j" "Journal" entry
         (file+datetree (concat org-directory "/journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")

        ("c" "Comedy" entry
         (file+headline (concat org-directory "/comedy.org"))
         "* %?\n  %i\n  %a")

        ("s" "School" entry
         (file+datetree (concat org-directory "/school.org"))
         "* %?\n  %i\n  %a")

        ("m" "Music" entry
         (file+headline (concat org-directory "/music.org") "Notes")
         "* %?\n")))

(eval-after-load "org-exp-blocks"
 '(progn
   (add-to-list 'org-export-blocks '(uml iy/org-export-blocks-format-plantuml nil))
   (add-to-list 'org-protecting-blocks "uml")))

 (defvar iy/org-plantuml-jar-path (expand-file-name "~/Dropbox/java-libs/plantuml.jar")
   "Path to the plantuml jar executable.")
 (defun iy/org-export-blocks-format-plantuml (body &rest headers)
   "Pass block BODY to the plantuml utility creating an image.
   Specify the path at which the image should be saved as the first
   element of headers, any additional elements of headers will be
   passed to the plantuml utility as command line arguments."
   (message "plantuml-formatting...")
   (let* ((args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
          (data-file (make-temp-file "org-plantuml"))
          (hash (progn
                  (set-text-properties 0 (length body) nil body)
                  (sha1 (prin1-to-string (list body args)))))
          (raw-out-file (if headers (car headers)))
          (out-file-parts (if (string-match "\\(.+\\)\\.\\([^\\.]+\\)$" raw-out-file)
                              (cons (match-string 1 raw-out-file)
                                    (match-string 2 raw-out-file))
                            (cons raw-out-file "png")))
          (out-file (concat (car out-file-parts) "_" hash "." (cdr out-file-parts))))
     (unless (file-exists-p iy/org-plantuml-jar-path)
       (error (format "Could not find plantuml.jar at %s" iy/org-plantuml-jar-path)))
     (setq body (if (string-match "^\\([^:\\|:[^ ]\\)" body)
                    body
                  (mapconcat (lambda (x) (substring x (if (> (length x) 1) 2 1)))
                             (org-split-string body "\n")
                             "\n")))
     (cond
      ((or htmlp latexp docbookp)
       (unless (file-exists-p out-file)
         (mapc ;; remove old hashed versions of this file
          (lambda (file)
            (when (and (string-match (concat (regexp-quote (car out-file-parts))
                                             "_\\([[:alnum:]]+\\)\\."
                                             (regexp-quote (cdr out-file-parts)))
                                     file)
                       (= (length (match-string 1 out-file)) 40))
              (delete-file (expand-file-name file
                                             (file-name-directory out-file)))))
          (directory-files (or (file-name-directory out-file)
                               default-directory)))
         (with-temp-file data-file (insert (concat "@startuml\n" body "\n@enduml")))
         (message (concat "java -jar " iy/org-plantuml-jar-path " -pipe " args))
         (with-temp-buffer
           (call-process-shell-command
            (concat "java -jar " iy/org-plantuml-jar-path " -pipe " args)
            data-file
            '(t nil))
           (write-region nil nil out-file)))
       (format "\n[[file:%s]]\n" out-file))
      (t (concat
          "\n#+BEGIN_EXAMPLE\n"
          body (if (string-match "\n$" body) "" "\n")
          "#+END_EXAMPLE\n")))))

(require-package 
'(evil
  evil-leader
  evil-surround
  evil-extra-operator
  evil-args
  key-chord
  evil-lisp-state))
(global-evil-extra-operator-mode t)
(global-evil-leader-mode t)
(global-evil-surround-mode t)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; make my macros work dammit!
(define-key evil-normal-state-map "Q" (kbd "@q"))

;; I love me a good jj escape :)  
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map (kbd "jj") 'evil-normal-state)
(key-chord-mode t)

;; leader bindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "ef" 'ido-find-file
  "bl" 'switch-to-buffer
  "kb" 'kill-buffer
  "d"  'dired
  "vs" 'split-window-right
  "cw" 'delete-window
  "ms" 'djk/raise-magit-status
  "cm" 'compile
  "gs" 'ido-goto-symbol
  "rf" 'ido-recentf-open
  "%"  'insert-file-name
  "ff" 'ff-find-other-file
  "wc" 'count-words
  "oc" 'org-capture
  "ec" (lambda () (interactive)(find-file "~/.emacs.d/init.org")))

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

(define-key evil-normal-state-map "L" 'evil-lisp-state)

(after 'evil-lisp-state 
    (define-key evil-lisp-state-map "K"   'evil-lisp-state-previous-sexp)
    (define-key evil-lisp-state-map "L"   'evil-lisp-state-next-sexp-down)
    (define-key evil-lisp-state-map "H"   'sp-backward-up-sexp)
    (define-key evil-lisp-state-map "J"   'sp-next-sexp))

(global-auto-revert-mode t)
(winner-mode t)
(evil-mode t)
(put 'downcase-region 'disabled nil)
(global-hl-line-mode t)
(icomplete-mode t)
(auto-revert-mode t)

(require-package 
'(midnight
  diminish 
  multiple-cursors)) 

(mapc #'(lambda (dim) (after (car dim) (diminish (cdr dim))))
          '((undo-tree     . undo-tree-mode)))

;; multiple cursors map
(evil-leader/set-key
  "cn" 'mc/mark-next-like-this
  "cp" 'mc/mark-previous-like-this
  "ca" 'mc/mark-all-like-this)

(defun insert-file-name ()
  "Insert the full path file name into the current buffer."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; Help++
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-l") 'find-library)
