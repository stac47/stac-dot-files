;;; early-init.el --- My early-init.el -*- lexical-binding: t -*-

(setq ring-bell-function 'ignore
      visible-bell t)

(setq inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      use-file-dialog nil
      use-dialog-box nil
      use-short-answers t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
