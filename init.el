;; Thanks ericcrosson on github
;; Bustin' out the rat posion
(mapc #'(lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode
	tool-bar-mode
	scroll-bar-mode))

;; ido-config.el Library
;; Configuration functions for using interactive-do.
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
      (push-mark nil t nil)		;set the mark
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

(after 'ido
  (setq ido-everywhere t                             ;always Ido
        ido-enable-flex-matching t                   ;smarter Ido
        ido-create-new-buffer 'always                ;quieter Ido
        ido-file-extensions-order '(".org" ".txt"))) ;precedence

(autoload-from-package "ido-config"
  '(ido-recentf-open
    ido-goto-symbol))
(ido-mode t)

(defmacro esc/toggle-fullscreen-buffer (win-register toggled-mode-test toggle-command
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

(defun esc/raise-magit-status ()
  "Bring up a full-screen magit-status or restore previous
window configuration."
  (interactive)
  (esc/toggle-fullscreen-buffer
    :magit-fullscreen
    (string= "magit-status-mode" major-mode)
    (progn (if (not (buffer-file-name))
               (message "Buffer not associated with a file")
             (save-buffer)
             (magit-status (file-name-directory (buffer-file-name)))))))

(defun esc/bury-compilation-buffer-if-successful (buffer string)
  "Bury the compilation buffer after a successful compile."
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not (search-forward "warning" nil t)))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)
    (message "Compilation successful.")))

(add-hook 'compilation-finish-functions
          'esc/bury-compilation-buffer-if-successful)
;; end thanks

(defun insert-file-name ()
  "Insert the full path file name into the current buffer."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; dallas hates the start screen
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
;; Help++
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-l") 'find-library)

;; color theme
(load-theme 'wombat)


;; I just wanna be evil
(require-package 
  '(evil
    evil-leader
    evil-surround
    evil-extra-operator
    evil-args
    key-chord))
(global-evil-extra-operator-mode t)
(global-evil-leader-mode t)
(global-evil-surround-mode t)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

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
  "ms" 'esc/raise-magit-status
  "cm" 'compile
  "gs" 'ido-goto-symbol
  "rf" 'ido-recentf-open
  "%"  'insert-file-name
  "ff" 'ff-find-other-file
  "ei" (lambda () (interactive)(find-file "~/.emacs.d/init.el")))

;; icomplete for M-x completion
(icomplete-mode t)
(auto-revert-mode t)
;; TODO: ido for C-x b completion

;; package management
(when (require 'package nil 'noerror)
  (setq package-user-dir "~/.emacs.d/elpa/")
  (mapc #'(lambda (source)
	    (add-to-list 'package-archives source) t)
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("marmalade" . "http://marmalade-repo.org/packages/")
	  ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	  ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

;; esc keybindings
					; Super-q : toggle magit status
(global-set-key (kbd "s-q") 'esc/raise-magit-status)
(global-set-key (kbd "C-c C-m") 'compile)
(setq compile-command "make")

					; ido-config.el Library
;; Configuration functions for using interactive-do.

;;;###autoload
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;###autoload
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
      (push-mark nil t nil)		;set the mark
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

(global-auto-revert-mode t)

(winner-mode t)
(evil-mode t)
(put 'downcase-region 'disabled nil)
(global-hl-line-mode t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
