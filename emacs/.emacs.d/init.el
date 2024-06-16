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

(setq stac/monitors
      '(((mm-size 301 195) (font-size 135))
        ((mm-size 599 329) (font-size 160))))

(defun stac/adapt-font-size (&optional frame)
  "Adapt the default font size depending on the monitor that FRAME is
displayed on. If FRAME is nil, the monitor the current frame is
displayed on is used to set the desired font size."
  (interactive)
  (let* ((current-monitor (frame-monitor-attributes frame))
         (size (alist-get 'mm-size current-monitor))
         selected-font-size)
    (dolist (my-monitor stac/monitors)
      (when (equal (alist-get 'mm-size my-monitor) size)
        (setq selected-font-size (car (alist-get 'font-size my-monitor)))))
    (unless selected-font-size
      (setq selected-font-size 100))
    (set-face-attribute 'default nil :height selected-font-size)))

(stac/adapt-font-size)

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

;; The cursor will blink forever
(setq blink-cursor-blinks 0)

(setq column-number-mode t)

(setq pulse-delay 0.08)

(setq require-final-newline t)

(add-hook 'prog-mode-hook (lambda() (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook (lambda() (setq show-trailing-whitespace t)))

(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'hl-line-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default indent-tabs-mode nil)

(setq custom-file (make-temp-file "emacs-custom-"))

;; Save history
(use-package savehist
  :init
  (setq history-length 25)
  (savehist-mode))

;; Remember the last place in a visited file
(use-package saveplace
  :init
  (save-place-mode))

;; Refresh buffer when underlying file has changed
(global-auto-revert-mode 1)

;; Refresh other buffer if needed (for instance Dired buffers)
(setq global-auto-revert-non-file-buffers t)

;; Prompt for passphrase in Emacs
(setq epg-pinentry-mode 'loopback)

(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil))

(use-package battery
  :ensure nil
  :config
  (when (and battery-status-function
             (not (string-match-p "N/A"
                                  (battery-format "%B"
                                                  (funcall battery-status-function)))))
    (display-battery-mode 1)))

(defun stac-mode-line-major-mode-name ()
  "Display the capitalized '-mode' truncated major mode."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defvar stac-mode-line-major-mode
  '(:eval
    (concat "(" (stac-mode-line-major-mode-name) ")"))
  "My display of mode in the mode-line")

(defvar stac-mode-line-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Only display misc info (like the current time) on the
currently selected window.")

(dolist (construct '(stac-mode-line-major-mode
                     stac-mode-line-misc-info))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
      '("%e" mode-line-front-space
        (:propertize
         ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
         display
         (min-width
          (5.0)))
        mode-line-frame-identification
        mode-line-buffer-identification
        "   "
        mode-line-position
        (vc-mode vc-mode)
        "  "
        stac-mode-line-major-mode
        " "

        stac-mode-line-misc-info
        mode-line-end-spaces))

(use-package project
  :config
  (assq-delete-all 'project-vc-dir project-switch-commands)
  (add-to-list 'project-switch-commands '(project-switch-to-buffer "Buffer") t))

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
            (message "Tags file changed: %s -> %s" old-tags-file new-tags-file)))
      (message "No current project")))
  :bind
  (:map project-prefix-map
        ("t" . stac/project-tags)))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-whitespace-regexp ".*?"
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil))

(use-package xref
  :ensure nil
  :commands (xref-find-definitions xref-go-back)
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package grep
  :ensure nil
  :commands (grep lgrep rgrep)
  :config
  (let* ((executable (or (executable-find "rg") "grep"))
         (rgp (string-match-p "rg" executable)))
    (when rgp
      (setq grep-program executable)
      (setq grep-template "rg -nH --null -e <R> <F>")
      (setq xref-search-program 'ripgrep))))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq delete-by-moving-to-trash t))

(setq user-mail-address "laurent.stacul@gmail.com"
    user-full-name "Laurent Stacul")

(setq stac/mail-accounts
      '(("laurent.stacul@gmail.com" . ((smtp-server . "smtp.gmail.com")
                                       (smtp-port . 465)
                                       (smtp-stream . ssl)))
        ("laurent.stacul@protonmail.com" . ((smtp-server . "localhost")
                                            (smtp-port . 1025)
                                            (smtp-stream . starttls)))))

(defun stac/setup-mail-account (&optional from)
  "Set mail account"
  (interactive
   (list
    (completing-read
     "Select account: "
     (map-keys stac/mail-accounts))))
  (let ((config (alist-get from stac/mail-accounts user-mail-address nil #'string=)))
    (message "Selected %s" (alist-get 'smtp-server config))
    (setq
     user-mail-address from
     smtpmail-smtp-server (alist-get 'smtp-server config)
     smtpmail-smtp-service (alist-get 'smtp-port config)
     smtpmail-stream-type (alist-get 'smtp-stream config))))

(use-package gnus
  :config
  (setq gnus-select-method '(nnml ""))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "gmail"
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl)
                        (nnir-search-engine imap)
                        (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                        (nnmail-expiry-wait 90)))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "protonmail"
                        (nnimap-address "localhost")
                        (nnimap-server-port 1143)
                        (nnimap-stream plain)
                        (nnir-search-engine imap)))
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))
  (setq gnus-use-cache t)
  )

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

(use-package bongo
  :config
  (setq bongo-default-directory "~/Music"))
