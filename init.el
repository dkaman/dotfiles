
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

(setq org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-hierarchical-todo-statistics     nil
      org-hierarchical-checkbox-statistics nil
      org-src-fontify-natively t)

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

(require 'midnight)
(require 'diminish) 

(mapc #'(lambda (dim) (after (car dim) (diminish (cdr dim))))
          '((undo-tree     . undo-tree-mode)))

(defun insert-file-name ()
  "Insert the full path file name into the current buffer."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; Help++
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-l") 'find-library)
