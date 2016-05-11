(setq url-proxy-services '(("http" . "localhost:3128")
			   ("https" . "localhost:3128")
			   ))

;; Setup package control
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; Don't load installed packages after init completed,
;; instead do it now
(setq package-enable-at-startup nil)
(package-initialize)

;; Needed to enable the use-package function below
(require 'use-package)

;; Specifies local directory to load packages from
;; This seems to only be so that we can manually add things there.
;; These directories are not managed via package control.
; (add-to-list 'load-path "~/.emacs.d/lisp/")
; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
; (let ((default-directory  "~/.emacs.d/packages/"))
;   (normal-top-level-add-subdirs-to-load-path))

;; Set the default font, try a series of options in turn.
;; Could probably be cleaned up somewhat.
(condition-case nil
    (set-face-attribute 'default nil :font "Deja Vu Sans Mono-12")
  (error
   (condition-case nil
       (set-face-attribute 'default nil :font "Consolas-12")
     (error
      (condition-case nil
	  (set-face-attribute 'default nil :font "Courier New-12")
	(error nil))))))

;; Essential settings.
;; Review. These are "essential" only in the sense that the person I picked this
;; section up from thought they were...
(setq
      ; From the help, the first two do the same thing.
      inhibit-splash-screen t
      inhibit-startup-message t
      ; For the next one, see the help. This probably doesn't work!
      inhibit-startup-echo-area-message t)
(setq make-backup-files nil) ; No backup files ending in ~
(setq auto-save-default nil) ; No autosave files #...#
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; Hide scrollbars
(menu-bar-mode -1) ; Hide menu bar
(show-paren-mode t) ; Highlights matching parenthesis
(electric-pair-mode t) ; Inserts close-paren when open is typed
; (setq initial-scratch-message "") ; No scratch text
(setq-default show-trailing-whitespace t) ; Shows all trailing whitespace

(use-package sublime-themes
  :ensure t
  :config
  (load-theme 'spolsky t)) ; Color theme

;; Base evil package
(use-package evil
  :ensure t
  :init
  ;; Unbind <C-u> for evil mode's use
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  ;; Move up and down through wrapped lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t))

;; evil leader key
(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-key
    "w"  'save-buffer ; w(rite)
    "so" 'eval-buffer ; so(urce)
    "S" 'eval-defun ; S(ource)
    "bb" 'mode-line-other-buffer ; b(ack) b(buffer)
    "bn" 'next-buffer ; b(uffer) n(ext)
    "bp" 'previous-buffer ; b(uffer) p(revious)
    "bd" 'kill-buffer ; b(uffer) d(elete)
    "bl" 'helm-buffers-list ; b(uffer) l(ist)
    "init" (lambda() (interactive) (evil-buffer-new nil "~/.emacs.d/init.el"))))

;; Tpope's surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Narrowing completion engine
(use-package helm
  :ensure t
  :config
  (helm-autoresize-mode 1)
  (global-set-key (kbd "M-x")     'undefined)
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'undefined)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (helm-mode t))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (global-linum-mode t)
  (linum-relative-mode))
