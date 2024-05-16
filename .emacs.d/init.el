;;; init.el ---  My minimalist emacs configuration

;;; Commentary:

;; I am trying to stick to the default Emacs bindings and to use a
;; minimal number of packages.
;; Supports MacOS and GNU

;;; Code:

;; No bell sound
(setq ring-bell-function 'ignore)
(setq visible-bell t)

(setq inhibit-splash-screen t)
(menu-bar-mode -1) ;; Do not display the menu bar
(tool-bar-mode -1) ;; No scrollbar
(tooltip-mode -1) ;; No tooltips
(if (display-graphic-p) ;; No scroll bar in GUI mode
    (scroll-bar-mode -1))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 165))

;; Package system initializations
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

;; Use extended dabbrev completion (hippie)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Use FIDO mode (better to navigate across buffers and files)
(fido-mode)

;; Display the point column in the Mode line
(setq column-number-mode t)

;; Always add a newline character at the end of the file
(setq require-final-newline t)

;; Automatically highlight the current line
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Show trailing whitespaces
(add-hook 'prog-mode-hook (lambda() (setq show-trailing-whitespace t)))

;; Display line number in any prod mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Forbid tabs for indentation
(setq-default indent-tabs-mode nil)

;; Save history
(setq history-length 25)
(savehist-mode 1)

;; Packages
(use-package magit)
(use-package yaml-mode)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook (lambda() (set-fill-column 80))))

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode))

(use-package ruby-end)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ruby-end yasnippet yaml-mode markdown-mode magit flycheck)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
