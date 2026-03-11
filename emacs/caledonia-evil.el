;;; caledonia-evil.el --- Evil bindings for Caledonia -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ryan Gibb
;;
;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Version: 0.5.0
;; Keywords: calendar
;; Package-Requires: ((emacs "24.3") (evil))
;; URL: https://ryan.freumh.org/caledonia.html
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides Evil bindings for Caledonia.
;;
;;; Code:

(require 'caledonia)

;; Only load evil integration if evil is available
(when (require 'evil nil t)

  (defun caledonia-evil--setup-bindings ()
    "Set up Evil keybindings for `caledonia-agenda-mode`."
    (evil-define-key* 'normal caledonia-agenda-mode-map
      (kbd "RET") 'caledonia-show-event
      (kbd "M-RET") 'caledonia-open-event-file
      "r" 'caledonia-refresh
      "a" 'caledonia-add-event
      "e" 'caledonia-edit-event
      "d" 'caledonia-delete-event
      "s" 'caledonia-search
      "q" 'quit-window))

  (defun caledonia-evil--setup-integration ()
    "Set up Evil integration for Caledonia agenda mode."
    (when (bound-and-true-p evil-mode)
      (evil-make-overriding-map caledonia-agenda-mode-map 'normal)
      (evil-normalize-keymaps)
      (caledonia-evil--setup-bindings)))

  (add-hook 'caledonia-agenda-mode-hook 'caledonia-evil--setup-integration))

(provide 'caledonia-evil)
;;; caledonia-evil.el ends here
