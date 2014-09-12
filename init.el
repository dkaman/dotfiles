;; Thanks ericcrosson on github
(let ((default-directory "~/.emacs.d/"))       ;for easy
  (normal-top-level-add-to-load-path '("."))   ;recursive
  (normal-top-level-add-subdirs-to-load-path)) ;loading

;; Bustin' out the rat posion
(mapc #'(lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode
	tool-bar-mode
	scroll-bar-mode))
;; end thanks

;; dallas hates the start screen
;; f-u stallman
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
;; Help++
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-l") 'find-library)

;; color theme
(load-theme 'wombat)

;; EVIL mode is the shit
(when (require 'evil nil 'noerror)
  (evil-mode t)
  (global-evil-leader-mode t))

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
