(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(yasnippet yaml-mode markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Do not display the menu bar
(menu-bar-mode -1)

;; Automatically refresh file on change
(global-auto-revert-mode t)

;; In programming language mode, display the line number
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Configure yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
