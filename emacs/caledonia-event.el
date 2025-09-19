;;; caledonia-event.el --- Caledonia Event Mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ryan Gibb
;;
;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Created: April 11, 2025
;; Modified: April 11, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/ryan/caledonia-event
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'cl-lib)

(cl-defstruct caldav-event
  title date start end location attendees notes calendar)

(define-derived-mode caldav-event-mode special-mode "CalDAV-Event"
  "Major mode for editing CalDAV events."
  (setq buffer-read-only nil)
  (use-local-map caldav-event-mode-map))

(defvar caldav-event-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'caldav-event-save)
    (define-key map (kbd "C-c C-k") #'kill-buffer)
    map))

(defun caldav--insert-field (label value)
  (insert (propertize (format "%-11s" (concat label ":")) 'face 'bold))
  (let ((start (point)))
    (insert (or value "") "\n")
    (let ((ov (make-overlay start (point))))
      (overlay-put ov 'caldav-editable t)
      (overlay-put ov 'face '(:background "#222" :foreground "#fff")))))

(defun caldav--insert-notes (notes)
  (insert "\nNotes:\n")
  (let ((start (point)))
    (insert (or notes "") "\n")
    (let ((ov (make-overlay start (point))))
      (overlay-put ov 'caldav-editable t)
      (overlay-put ov 'face '(:background "#111" :foreground "#eee")))))

(defun caldav-create-event ()
  (interactive)
  (let ((buf (get-buffer-create "*CalDAV Event*")))
    (with-current-buffer buf
      (erase-buffer)
      (caldav-event-mode)
      (setq-local caldav--event-overlays nil)
      (caldav--insert-field "Title" "")
      (caldav--insert-field "Date" "2025-04-10")
      (caldav--insert-field "Start Time" "14:00")
      (caldav--insert-field "End Time" "15:00")
      (caldav--insert-field "Location" "")
      (caldav--insert-field "Attendees" "")
      (caldav--insert-field "Calendar" "Work")
      (caldav--insert-notes "")
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun caldav-create-event ()
  (interactive)
  (let ((buf (get-buffer-create "*CalDAV Event*")))
    (with-current-buffer buf
      (erase-buffer)
      (caldav-event-mode)
      (setq-local caldav--event-overlays nil)
      (caldav--insert-field "Title" "")
      (caldav--insert-field "Date" "2025-04-10")
      (caldav--insert-field "Start Time" "14:00")
      (caldav--insert-field "End Time" "15:00")
      (caldav--insert-field "Location" "")
      (caldav--insert-field "Attendees" "")
      (caldav--insert-field "Calendar" "Work")
      (caldav--insert-notes "")
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun caldav-event-save ()
  (interactive)
  (let (fields)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:]+\\):" nil t)
        (let* ((label (match-string 1))
               (value (buffer-substring-no-properties
                       (line-beginning-position 2)
                       (line-end-position))))
          (setq fields (plist-put fields (intern (downcase label)) (string-trim value))))))
    (let ((event (make-caldav-event
                  :title (plist-get fields :title)
                  :date (plist-get fields :date)
                  :start (plist-get fields :start)
                  :end (plist-get fields :end)
                  :location (plist-get fields :location)
                  :attendees (plist-get fields :attendees)
                  :notes (plist-get fields :notes)
                  :calendar (plist-get fields :calendar))))
      (message "Saved event: %S" event)
      ;; TODO: serialize and push via CalDAV
      (kill-buffer))))

(defun caldav-next-field ()
  "Jump to next editable field."
  (interactive)
  (let ((pos (point)))
    (catch 'found
      (dolist (ov (overlays-in (point) (point-max)))
        (when (overlay-get ov 'caldav-editable)
          (goto-char (overlay-start ov))
          (throw 'found t)))
      (message "No more fields."))))

(define-key caldav-event-mode-map (kbd "TAB") #'caldav-next-field)

(provide 'caledonia-event)
;;; caledonia-event.el ends here
