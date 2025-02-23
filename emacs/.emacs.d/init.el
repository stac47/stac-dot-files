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

(setq default-input-method "french-prefix")

(setq stac/monitors
      '(((mm-size 301 195) (font-size 120))
        ((mm-size 599 329) (font-size 150))))

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

(setq package-archive-priorities
      '(("melpa-stable" . 1)
        ("gnu" . 1)
        ("nongnu" . 1)
        ("melpa" . 0)))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(when (< emacs-major-version 30)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package))

(defun stac/recompile-packages ()
  "Recompile all the packages in the user's package directory."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force)
  (message "Compilation done. Consider restarting emacs."))

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

;; Refresh buffer when underlying file has changed
(global-auto-revert-mode 1)

;; Refresh other buffer if needed (for instance Dired buffers)
(setq global-auto-revert-non-file-buffers t)

;; Prompt for passphrase in Emacs
(setq epg-pinentry-mode 'loopback)

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

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile.git")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml.git")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby.git")))

(setq major-mode-remap-alist
      '((shell-script-mode . bash-ts-mode)
        (json-mode . json-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (ruby-mode . ruby-ts-mode)))

(defun stac/treesit-install-all-grammars ()
  "Install the tree-sitter grammars I configured."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (treesit-install-language-grammar (car grammar))))

(setenv "PAGER" "cat")

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "ll" "ls -AlohG --color=always"))))

(use-package project
  :config
  (assq-delete-all 'project-vc-dir project-switch-commands)
  (assq-delete-all 'project-eshell project-switch-commands)
  (add-to-list 'project-switch-commands '(project-switch-to-buffer "Buffer") t)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

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
        ("t" . stac/project-tags)
        ("m" . magit-project-status)))

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

(use-package man
  :config
  (when (eq system-type 'darwin)
    (setq manual-program "gman")))

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  (setq delete-by-moving-to-trash t))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ruby . t)
     (python . t))))

(use-package ruby-mode
  :config
  (setq ruby-align-to-stmt-keywords t)
  (setq ruby-align-chained-calls nil)
  (setq ruby-method-params-indent nil)
  (setq ruby-block-indent nil)
  (setq ruby-method-call-indent nil))

(use-package smtpmail
  :ensure nil
  :config
  (setq smtpmail-debug-info t)
  (setq send-mail-function #'smtpmail-send-it))

;; Save history
(use-package savehist
  :init
  (setq history-length 25)
  (savehist-mode))

;; Remember the last place in a visited file
(use-package saveplace
  :init
  (save-place-mode))

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

(use-package mu4e
  :ensure nil
  :if (locate-library "mu4e.el")
  :config
  (setq mu4e-confirm-quit nil)
  (setq mu4e-hide-index-messages t)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-headers-fields '((:human-date . 20)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from . 22)
                              (:subject)))
  (setq mu4e-headers-date-format "%F")
  (setq mu4e-attachment-dir "~/Downloads")
  (setq stac/emacs-mailing-lists
        (list "help-gnu-emacs.gnu.org"
              "emacs-devel.gnu.org"
              "info-gnu-emacs.gnu.org"
              "bug-gnu-emacs.gnu.org"))
  (setq stac/development-mailing-lists
        (append stac/emacs-mailing-lists))
  (defun stac/mu4e-bookmark-mailing-list-query (mailing-lists)
    (format "(%s)"
            (mapconcat (lambda (s) (format "list:%s" s)) mailing-lists " or ")))
  (setq mu4e-bookmarks
        '(
          (
           :name "Unread messages"
           :query "flag:unread AND NOT flag:trashed AND NOT flag:list"
           :key ?u
           )
          (
           :name "From Emacs Lists"
           :query (lambda () (concat "flag:unread AND "
                                     "NOT flag:trashed AND "
                                     "flag:list AND "
                                     (stac/mu4e-bookmark-mailing-list-query stac/emacs-mailing-lists)))
           :key ?e
           )
          (
           :name "From Other Lists"
           :query (lambda () (concat "flag:unread AND "
                                     "NOT flag:trashed AND "
                                     "flag:list AND NOT "
                                     (stac/mu4e-bookmark-mailing-list-query stac/development-mailing-lists)))
           :key ?l
           )
          (
           :name "Today's messages"
           :query "date:today..now"
           :key ?t)
          )
        )

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "laurent.stacul@gmail.com"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/laurent.stacul@gmail.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "laurent.stacul@gmail.com")
                    (user-full-name    . "Laurent Stacul")
                    (smtpmail-smtp-server  . "smtp.gmail.com")
                    (smtpmail-smtp-service . 465)
                    (smtpmail-stream-type  . ssl)
                    (mu4e-drafts-folder  . "/laurent.stacul@gmail.com/[Gmail]/Brouillons")
                    (mu4e-sent-folder  . "/laurent.stacul@gmail.com/[Gmail]/Messages envoy&AOk-s")
                    (mu4e-refile-folder  . "/laurent.stacul@gmail.com/[Gmail]/Tous les messages")
                    (mu4e-trash-folder  . "/laurent.stacul@gmail.com/[Gmail]/Corbeille")
                    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                    (mu4e-sent-messages-behavior . delete)
                    (mu4e-maildir-shortcuts . ((:maildir "/laurent.stacul@gmail.com/Inbox" :key ?i)))))
          ,(make-mu4e-context
            :name "captain.stac@gmail.com"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/captain.stac@gmail.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "captain.stac@gmail.com")
                    (user-full-name    . "Laurent Stacul")
                    (smtpmail-smtp-server  . "smtp.gmail.com")
                    (smtpmail-smtp-service . 465)
                    (smtpmail-stream-type  . ssl)
                    (mu4e-drafts-folder  . "/captain.stac@gmail.com/[Gmail]/Brouillons")
                    (mu4e-sent-folder  . "/captain.stac@gmail.com/[Gmail]/Messages envoy&AOk-s")
                    (mu4e-refile-folder  . "/captain.stac@gmail.com/[Gmail]/Tous les messages")
                    (mu4e-trash-folder  . "/captain.stac@gmail.com/[Gmail]/Corbeille")
                    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                    (mu4e-sent-messages-behavior . delete)
                    (mu4e-maildir-shortcuts . ((:maildir "/captain.stac@gmail.com/Inbox" :key ?i)))))
          ,(make-mu4e-context
            :name "La Poste"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/laurent.stacul@laposte.net" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "laurent.stacul@laposte.net")
                    (user-full-name    . "Laurent Stacul")
                    (smtpmail-smtp-server  . "smtp.laposte.net")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-stream-type  . ssl)
                    (mu4e-drafts-folder  . "/laurent.stacul@laposte.net/DRAFT")
                    (mu4e-sent-folder  . "/laurent.stacul@laposte.net/OUTBOX")
                    (mu4e-refile-folder  . "/laurent.stacul@laposte.net/Inbox")
                    (mu4e-trash-folder  . "/laurent.stacul@laposte.net/TRASH")
                    (mu4e-maildir-shortcuts . ((:maildir "/laurent.stacul@laposte.net/Inbox" :key ?i)))))
          ,(make-mu4e-context
            :name "Proton"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/laurent.stacul@protonmail.com" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "laurent.stacul@protonmail.com")
                    (user-full-name    . "Laurent Stacul")
                    (smtpmail-smtp-server  . "localhost")
                    (smtpmail-smtp-service . 1025)
                    (smtpmail-stream-type  . starttls)
                    (mu4e-drafts-folder  . "/laurent.stacul@protonmail.com/Drafts")
                    (mu4e-sent-folder  . "/laurent.stacul@protonmail.com/Sent")
                    (mu4e-refile-folder  . "/laurent.stacul@protonmail.com/Archive")
                    (mu4e-trash-folder  . "/laurent.stacul@protonmail.com/Trash")
                    (mu4e-maildir-shortcuts . ((:maildir "/laurent.stacul@protonmail.com/Inbox" :key ?i))))))))

(use-package magit)

(use-package vertico
  :custom
  (setq vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package markdown-mode
  :hook
  ((markdown-mode . auto-fill-mode)
   (markdown-mode . (lambda() (set-fill-column 80)))))

(use-package chruby
  :vc (:fetcher github :repo stac47/chruby.el :rev "main"))

(use-package inf-ruby)

(use-package ruby-end)

(use-package yaml-mode)

(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 4
                indent-tabs-mode nil))

(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode)))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1))

(use-package bongo
  :config
  (setq bongo-default-directory "~/Music"))

(use-package eradio
  :config
  (setq eradio-channels
        '(("France Inter" . "http://direct.franceinter.fr/live/franceinter-midfi.mp3")
          ("France Info" . "http://direct.franceinfo.fr/live/franceinfo-midfi.mp3")
          ("France Culture" . "https://direct.franceculture.fr/live/franceculture-midfi.mp3")
          ("France Musique" . "https://direct.francemusique.fr/live/francemusique-midfi.mp3")
          ("FIP" . "http://direct.fipradio.fr/live/fip-midfi.mp3")
          ("Radio Classique" . "http://icepe6.infomaniak.ch/radioclassique-high.mp3"))))

(use-package org-drill)

(use-package simple-httpd)

(use-package terraform-mode)

(use-package kubed
  :bind-keymap ("C-c k" . kubed-prefix-map))
