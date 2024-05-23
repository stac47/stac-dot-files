;;; init.el ---  My minimalist emacs configuration

;;; Commentary:

;; I am trying to stick to the default Emacs bindings and to use a
;; minimal number of packages.
;; Supports MacOS and GNU

;;; Code:

;; Default modus-themes theme
(load-theme 'modus-operandi t)

;; No bell sound
(setq ring-bell-function 'ignore)
(setq visible-bell t)

(setq inhibit-splash-screen t)
(menu-bar-mode -1) ;; Do not display the menu bar
(tool-bar-mode -1) ;; No scrollbar
(tooltip-mode -1) ;; No tooltips
(if (display-graphic-p) ;; No scroll bar in GUI mode
    (scroll-bar-mode -1))
(setq use-dialog-box nil)

;; Readable definition on Apple Retina screen
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 165))

;; Package system initializations
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

;; Use extended dabbrev completion (hippie)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

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

;; Remember the last place in a visited file
(save-place-mode 1)

;; Do not mix customize with my config
(setq  custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file)

;; Refresh buffer when underlying file has changed
(global-auto-revert-mode 1)

;; Refresh other buffer if needed (for instance Dired buffers)
(setq global-auto-revert-non-file-buffers t)

;; I-search config
;; Display the number of matches
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;; Allow simple search like 'a b' to match 'a.c b'
(setq search-whitespace-regexp ".*?")

;; Packages
(use-package magit)
(use-package yaml-mode)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save)))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'markdown-mode-hook (lambda() (set-fill-column 80))))

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode))

(use-package ruby-end)

(use-package dockerfile-mode)

;;; init.el ends here
