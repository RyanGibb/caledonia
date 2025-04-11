;;; caledonia-evil.el --- Evil bindings for Caledonia -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ryan Gibb
;;
;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Version: 0.4.0
;; Keywords: calendar
;; Package-Requires: ((emacs "24.3"))
;; URL: https://ryan.freumh.org/caledonia.html
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides Evil bindings for Caledonia.
;;
;;; Code:

(require 'evil)
(require 'caledonia)

(defvar caledonia-evil-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'caledonia-query-date-range)
    (define-key map "c" 'caledonia-query-calendars)
    (define-key map "t" 'caledonia-query-text)
    (define-key map "i" 'caledonia-query-id)
    (define-key map "r" 'caledonia-query-recurring)
    (define-key map "l" 'caledonia-query-limit)
    (define-key map "z" 'caledonia-query-timezone)
    map)
  "Evil mode keymap for filter commands in Caledonia mode.")

(defun caledonia-evil--setup-evil-bindings ()
  "Set up Evil keybindings for `caledonia-mode`."
  (evil-define-key 'normal caledonia-mode-map
    (kbd "RET") 'caledonia-show-event
    (kbd "M-RET") 'caledonia-open-event-file
    "l" 'caledonia-list
    "s" 'caledonia-search
    "r" 'caledonia-refresh
    "q" 'quit-window
    "f" caledonia-evil-filter-map))

(defun caledonia-evil--setup-evil-integration ()
  "Set up Evil integration for Caledonia mode."
  (when (bound-and-true-p evil-mode)
    (evil-make-overriding-map caledonia-mode-map 'normal)
    (evil-normalize-keymaps)
    (caledonia-evil--setup-evil-bindings)))

(add-hook 'caledonia-mode-hook 'caledonia-evil--setup-evil-integration)

(provide 'caledonia-evil)
;;; caledonia-evil.el ends here
