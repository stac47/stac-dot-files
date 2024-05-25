(load-theme 'modus-operandi t)

(setq ring-bell-function 'ignore)
(setq visible-bell t)

(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(if (display-graphic-p) (scroll-bar-mode -1))
(setq use-dialog-box nil)

(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 165))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq column-number-mode t)

(setq require-final-newline t)

(add-hook 'prog-mode-hook (lambda() (setq show-trailing-whitespace t)))

(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default indent-tabs-mode nil)

(setq custom-file (make-temp-file "emacs-custom-"))

;; Save history
(setq history-length 25)
(savehist-mode 1)

;; Remember the last place in a visited file
(save-place-mode 1)

;; Refresh buffer when underlying file has changed
(global-auto-revert-mode 1)

;; Refresh other buffer if needed (for instance Dired buffers)
(setq global-auto-revert-non-file-buffers t)

(use-package project
  :init
  (defun stac/project-tags ()
    "When in a project, visit the tags file at the root of the project."
    (interactive)
    (if (project-current)
        (let* ((proj-root (expand-file-name (project-root (project-current))))
               (old-tags-file tags-file-name)
               (new-tags-file (concat proj-root "TAGS")))
          (if (equal old-tags-file new-tags-file)
              (message "Tags file not changed: %s" old-tags-file)
            (visit-tags-table new-tags-file)
            (message "Tags %s -> %s" old-tags-file new-tags-file)))
      (message "No current project"))))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-whitespace-regexp ".*?"
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq delete-by-moving-to-trash t))

(use-package magit)

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

(use-package yaml-mode)
