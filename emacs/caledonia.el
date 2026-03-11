;;; caledonia.el --- Emacs integration for Caledonia -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ryan Gibb

;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Version: 0.5.0
;; Keywords: calendar
;; Package-Requires: ((emacs "24.4"))
;; URL: https://ryan.freumh.org/caledonia.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an Emacs interface to the Caledonia calendar CLI.
;; It communicates with Caledonia using S-expressions for data exchange.
;; The primary view is an org-agenda style agenda grouped by date.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'pulse nil t)
(require 'org)

(defgroup caledonia nil
  "Interface to Caledonia calendar client."
  :group 'calendar
  :prefix "caledonia-")

(defcustom caledonia-executable (executable-find "caled")
  "Path to the Caledonia executable."
  :type 'string
  :group 'caledonia)

(defface caledonia-calendar-name-face
  '((t :inherit font-lock-function-name-face))
  "Face used for calendar names in the events view."
  :group 'caledonia)

(defface caledonia-date-face
  '((t :inherit font-lock-string-face))
  "Face used for dates in the events view."
  :group 'caledonia)

(defface caledonia-summary-face
  '((t :inherit default))
  "Face used for event summaries in the events view."
  :group 'caledonia)

(defface caledonia-location-face
  '((t :inherit font-lock-comment-face))
  "Face used for event locations in the events view."
  :group 'caledonia)

(defface caledonia-agenda-date-face
  '((t :inherit org-agenda-date :weight bold))
  "Face used for date headers in the agenda view."
  :group 'caledonia)

(defface caledonia-agenda-time-face
  '((t :inherit font-lock-string-face))
  "Face used for times in the agenda view."
  :group 'caledonia)

(defcustom caledonia-from-date "today"
  "Default start date for calendar view."
  :type 'string
  :group 'caledonia)

(defcustom caledonia-to-date "+3m"
  "Default end date for calendar view (3 months from today)."
  :type 'string
  :group 'caledonia)

;; Define histories for input fields

(defvar caledonia-from-history nil "History for from date inputs.")
(defvar caledonia-to-history nil "History for to date inputs.")
(defvar caledonia-timezone-history nil "History for timezone inputs.")
(defvar caledonia-calendars-history nil "History for calendar inputs.")
(defvar caledonia-text-history nil "History for search text inputs.")
(defvar caledonia-id-history nil "History for event ID inputs.")
(defvar caledonia-limit-history nil "History for limit inputs.")
(defvar caledonia-summary-history nil "History for event summary inputs.")
(defvar caledonia-location-history nil "History for event location inputs.")

;; Internal variables

(defvar caledonia--agenda-buffer "*Caledonia Agenda*"
  "Buffer name for the agenda view.")
(defvar caledonia--details-buffer "*Caledonia Event Details*"
  "Buffer name for displaying Caledonia event details.")
(defvar caledonia--server-process nil
  "The persistent Caledonia server process.")
(defvar caledonia--server-buffer-name "*caledonia-server-io*"
  "Buffer for server process I/O.")
(defvar caledonia--response-line nil
  "Last response line received.")
(defvar caledonia--response-flag nil
  "Non-nil means a response has been received.")

;; Server communication

(defvar caledonia--server-line-buffer "")

(defun caledonia--server-filter (process output)
  "Filter PROCESS OUTPUT."
  ;; Append to the ongoing buffer for logging/debugging
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)))
  ;; Append new output to line buffer
  (setq caledonia--server-line-buffer (concat caledonia--server-line-buffer output))
  ;; Extract full lines
  (let ((lines (split-string caledonia--server-line-buffer "\n")))
    ;; Keep the last line (possibly incomplete) for next round
    (setq caledonia--server-line-buffer (car (last lines)))
    ;; Process all complete lines
    (dolist (line (butlast lines))
      (when (and (not caledonia--response-flag)
                 (not (string-empty-p line)))
        (setq caledonia--response-line line)
        (setq caledonia--response-flag t)))))

(defun caledonia--server-sentinel (process event)
  "Listen on PROCESS for an EVENT."
  (message "Caledonia Server process event: %s (%s)" process event)
  (setq caledonia--server-process nil))

(defun caledonia--ensure-server-running ()
  "Run the caledonia binary in server mode."
  (unless (and caledonia--server-process (process-live-p caledonia--server-process))
    (message "Caledonia  Starting server...")
    (setq caledonia--server-process
          (start-process "caledonia-server"
                         (get-buffer-create caledonia--server-buffer-name)
                         caledonia-executable
                         "server"))
    (unless (and caledonia--server-process (process-live-p caledonia--server-process))
      (error "Caledonia  Failed to start server process"))
    (set-process-filter caledonia--server-process #'caledonia--server-filter)
    (set-process-sentinel caledonia--server-process #'caledonia--server-sentinel)
    (message "Caledonia  Server started.")))

(defun caledonia--send-request (request-str)
  "Send REQUEST-STR and get response back."
  (caledonia--ensure-server-running)
  (setq caledonia--response-line nil)
  (setq caledonia--response-flag nil)
  (process-send-string caledonia--server-process (concat request-str "\n"))
  ;; Wait for response
  (let ((start-time (current-time)))
    (while (and (not caledonia--response-flag)
                (< (time-to-seconds (time-since start-time)) 5) ; 5 sec timeout
                (process-live-p caledonia--server-process))
      (accept-process-output caledonia--server-process 0 100000))) ; Wait 100ms
  (unless caledonia--response-flag
    (error "Caledonia  Timeout or server died waiting for response"))
  (condition-case err
      (let ((response-sexp (read caledonia--response-line)))
        (unless (and (listp response-sexp) (memq (car response-sexp) '(Ok Error)))
          (error "Caledonia  Invalid response format: %S" response-sexp))
        (if (eq (car response-sexp) 'Error)
            (error "Caledonia Server Error: %s" (cadr response-sexp))
          ;; Return the (Ok ...) payload
          (cadr response-sexp)))
    (error "Caledonia Failed to parse response line: %s"
           caledonia--response-line (error-message-string err))))

(defun caledonia--get-events (event-payload)
  "Parse EVENT-PAYLOAD of structure (Events (events...))."
  (if (and (listp event-payload) (eq (car event-payload) 'Events))
      (let ((event-list (cadr event-payload)))
        event-list)
    (error
     (message "Failed to parse Caledonia output: %s" (error-message-string err))
     nil)))

;; Helper functions

(defun caledonia--format-timestamp (iso-string &optional format)
  "Format ISO-8601 time string ISO-STRING to human-readable format.
FORMAT defaults to \"%Y-%m-%d %H:%M\" if not specified."
  (let* ((parsed (parse-time-string iso-string))
         (time (apply #'encode-time
                      (append (cl-subseq parsed 0 6) (list nil -1)))))
    (format-time-string (or format "%Y-%m-%d %H:%M") time)))

(defun caledonia--get-key (key event)
  "Get KEY from EVENT as a string."
  (let ((value (cadr (assoc key event))))
    (cond
     ((null value) nil)
     ((stringp value) value)
     ((symbolp value) (symbol-name value)))))

(defun caledonia--find-and-highlight-event-in-file (file event-id)
  "Find EVENT-ID in FILE, position cursor, and highlight the event.
Return non-nil if the event was found."
  (when (and file event-id)
    (let ((id-str (format "%s" event-id))
          (found nil))
      ;; Try to find and highlight iCalendar VEVENT block
      (goto-char (point-min))
      (when (and (string-match-p "\\.ics$" file)
                 (search-forward (format "UID:%s" id-str) nil t))
        ;; Found the UID in an ICS file, try to highlight the VEVENT block
        (let ((uid-pos (match-beginning 0))
              (vevent-start nil)
              (vevent-end nil))
          ;; Find start of the VEVENT block
          (save-excursion
            (goto-char uid-pos)
            (if (search-backward "BEGIN:VEVENT" nil t)
                (setq vevent-start (match-beginning 0))
              (setq vevent-start uid-pos)))
          ;; Find end of the VEVENT block
          (save-excursion
            (goto-char uid-pos)
            (if (search-forward "END:VEVENT" nil t)
                (setq vevent-end (match-end 0))
              (setq vevent-end (line-end-position))))
          ;; Highlight the whole VEVENT block if found
          (when (and vevent-start vevent-end)
            (goto-char vevent-start)
            (caledonia--highlight-region vevent-start vevent-end)
            (recenter)
            (setq found t))))
      (unless found
        (message "Event ID not found in file"))
      found)))

(defun caledonia--display-event-details (event)
  "Display details for EVENT in a separate buffer."
  (let ((buf (get-buffer-create caledonia--details-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (let* ((id (caledonia--get-key 'id event))
               (summary (caledonia--get-key 'summary event))
               (description (caledonia--get-key 'description event))
               (start (caledonia--get-key 'start event))
               (end (caledonia--get-key 'end event))
               (location (caledonia--get-key 'location event))
               (calendar (caledonia--get-key 'calendar event))
               (file (caledonia--get-key 'file event))
               (start-tz (caledonia--get-key 'start_tz event))
               (end-tz (caledonia--get-key 'end_tz event))
               (start-str (when start (caledonia--format-timestamp start)))
               (end-str (when end (caledonia--format-timestamp end))))
          (when id
            (insert (propertize "Summary: " 'face 'bold) summary "\n"))
          (when id
            (insert (propertize "ID: " 'face 'bold) id "\n"))
          (when calendar
            (insert (propertize "Calendar: " 'face 'bold) calendar "\n"))
          (when start-str
            (insert (propertize "Start: " 'face 'bold) start-str
                    (if start-tz (format " (%s)" start-tz) "")
                    "\n"))
          (when end-str
            (insert (propertize "End: " 'face 'bold) end-str
                    (if end-tz (format " (%s)" end-tz) "")
                    "\n"))
          (when location
            (insert (propertize "Location: " 'face 'bold) location "\n"))
          (when file
            (insert (propertize "File: " 'face 'bold)
                    (propertize file 'face 'link
                                'mouse-face 'highlight
                                'help-echo "Click to open file with highlighting"
                                'keymap (let ((map (make-sparse-keymap))
                                              (event-copy event))
                                          (define-key map [mouse-1]
                                                      (lambda ()
                                                        (interactive)
                                                        (let ((file-path file)
                                                              (id-val (caledonia--get-key 'id event-copy)))
                                                          (find-file file-path)
                                                          (caledonia--find-and-highlight-event-in-file
                                                           file-path id-val))))
                                          (define-key map (kbd "RET")
                                                      (lambda ()
                                                        (interactive)
                                                        (let ((file-path file)
                                                              (id-val (caledonia--get-key 'id event-copy)))
                                                          (find-file file-path)
                                                          (caledonia--find-and-highlight-event-in-file
                                                           file-path id-val))))
                                          map))
                    "\n"))
          (when description
            (insert "\n" (propertize "Description:" 'face 'bold) "\n"
                    (propertize "------------" 'face 'bold) "\n"
                    description "\n")))))
    (switch-to-buffer-other-window buf)))

(defun caledonia--highlight-region (start end)
  "Highlight the region between START and END."
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region start end))
  ;; Fallback for when pulse is not available
  (unless (fboundp 'pulse-momentary-highlight-region)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face 'highlight)
      (run-with-timer 0.5 nil (lambda () (delete-overlay overlay))))))

;; Input helpers

(defun caledonia--read-date-range ()
  "Read a date range from the user with `org-mode' date picker integration.
Returns a cons cell (from-date . to-date).
The from-date can be nil to indicate no start date constraint."
  (let (from to)
    (setq from
          (if (y-or-n-p "Set a start date? ")
              (org-read-date nil nil nil "From date: " nil nil t)
                                        ; empty string differentiates from nil for optional args later on
            ""))
    ;; Use org-mode's date picker for To date (must have a value)
    (setq to (org-read-date nil nil nil "To date: " nil nil t))
    (cons from to)))

(defun caledonia--read-datetime (prompt)
  "Read a date+time with PROMPT using org-read-date.
Returns (date . time) cons where date is \"YYYY-MM-DD\" and time is
\"HH:MM\" or nil if no time was given."
  (let* ((input (org-read-date nil nil nil prompt nil nil t))
         (parts (split-string input " ")))
    (cons (car parts)
          (when (and (cdr parts) (string-match-p "^[0-9][0-9]:[0-9][0-9]" (cadr parts)))
            (cadr parts)))))

(defun caledonia--read-datetime-with-default (prompt default)
  "Read a date+time with PROMPT using org-read-date, with DEFAULT pre-filled.
DEFAULT should be \"YYYY-MM-DD HH:MM\".  Returns (date . time) cons."
  (let* ((input (org-read-date nil nil nil prompt nil default t))
         (parts (split-string input " ")))
    (cons (car parts)
          (when (and (cdr parts) (string-match-p "^[0-9][0-9]:[0-9][0-9]" (cadr parts)))
            (cadr parts)))))

(defun caledonia--get-available-calendars ()
  "Get list of available calendar names from server."
  (let ((response (caledonia--send-request "ListCalendars")))
    (if (and (listp response) (eq (car response) 'Calendars))
        (cadr response)
      nil)))

(defun caledonia--sexp-field (key value)
  "Format KEY VALUE pair as sexp field string, or empty string if VALUE is nil."
  (cond
   ((null value) "")
   ((listp value)
    (format "(%s (%s))" key (mapconcat (lambda (s) (format "%S" s)) value " ")))
   (t (format "(%s %S)" key value))))

(defun caledonia--build-sexp-fields (fields)
  "Build sexp string from alist FIELDS, omitting nil values."
  (let ((parts (cl-remove-if #'string-empty-p
                              (mapcar (lambda (pair)
                                        (caledonia--sexp-field (car pair) (cdr pair)))
                                      fields))))
    (mapconcat #'identity parts " ")))

;; Agenda view

(defvar caledonia-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'caledonia-show-event)
    (define-key map (kbd "M-RET") 'caledonia-open-event-file)
    (define-key map (kbd "r") 'caledonia-refresh)
    (define-key map (kbd "a") 'caledonia-add-event)
    (define-key map (kbd "e") 'caledonia-edit-event)
    (define-key map (kbd "d") 'caledonia-delete-event)
    (define-key map (kbd "s") 'caledonia-search)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'caledonia-agenda-help)
    map)
  "Keymap for Caledonia agenda mode.")

(defvar caledonia-agenda--help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'caledonia-show-event)
    (define-key map (kbd "M-RET") 'caledonia-open-event-file)
    (define-key map (kbd "r") 'caledonia-refresh)
    (define-key map (kbd "a") 'caledonia-add-event)
    (define-key map (kbd "e") 'caledonia-edit-event)
    (define-key map (kbd "d") 'caledonia-delete-event)
    (define-key map (kbd "s") 'caledonia-search)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Clean keymap for which-key help display (no inherited bindings).")

(defun caledonia-agenda-help ()
  "Show available keybindings."
  (interactive)
  (if (fboundp 'which-key-show-keymap)
      (which-key-show-keymap 'caledonia-agenda--help-map)
    (describe-mode)))

(define-derived-mode caledonia-agenda-mode special-mode "Caledonia-Agenda"
  "Major mode for displaying calendar events in an agenda view.")

(defvar-local caledonia-agenda--query nil
  "Current query used by this agenda buffer.")

(defvar-local caledonia-agenda--from-date nil
  "Start date as (year month day) for the agenda range.")

(defvar-local caledonia-agenda--to-date nil
  "End date as (year month day) for the agenda range.")

(defun caledonia--parse-iso-date (iso-string)
  "Parse ISO-STRING and return (year month day hour minute)."
  (let ((parsed (parse-time-string iso-string)))
    (list (nth 5 parsed) (nth 4 parsed) (nth 3 parsed)
          (or (nth 2 parsed) 0) (or (nth 1 parsed) 0))))

(defun caledonia--format-day-header (year month day)
  "Format a day header like \"Monday     10 March 2025\" from YEAR, MONTH, DAY."
  (let* ((time (encode-time 0 0 12 day month year nil -1))
         (dow (format-time-string "%A" time))
         (month-name (format-time-string "%B" time)))
    (format "%-10s %2d %s %d" dow day month-name year)))

(defun caledonia--date-to-absolute (year month day)
  "Convert YEAR MONTH DAY to an absolute day number for iteration."
  (calendar-absolute-from-gregorian (list month day year)))

(defun caledonia--absolute-to-date (abs)
  "Convert absolute day number ABS to (year month day)."
  (let ((greg (calendar-gregorian-from-absolute abs)))
    (list (nth 2 greg) (nth 0 greg) (nth 1 greg))))

(defun caledonia--render-agenda (events &optional from-date to-date)
  "Render EVENTS in agenda format, grouped by date.
Shows all days between FROM-DATE and TO-DATE, including empty days.
FROM-DATE and TO-DATE are (year month day) lists.  When nil, derived from events."
  (let ((day-groups (make-hash-table :test 'equal)))
    ;; Group events by date
    (dolist (event events)
      (let* ((start (caledonia--get-key 'start event))
             (parsed (caledonia--parse-iso-date start))
             (date-key (list (nth 0 parsed) (nth 1 parsed) (nth 2 parsed))))
        (puthash date-key
                 (append (gethash date-key day-groups) (list event))
                 day-groups)))
    ;; Find date range
    (when events
      (let* ((first-event (car events))
             (last-event (car (last events)))
             (first-parsed (caledonia--parse-iso-date (caledonia--get-key 'start first-event)))
             (last-parsed (caledonia--parse-iso-date (caledonia--get-key 'start last-event)))
             (range-start (or from-date (list (nth 0 first-parsed) (nth 1 first-parsed) (nth 2 first-parsed))))
             (range-end (or to-date (list (nth 0 last-parsed) (nth 1 last-parsed) (nth 2 last-parsed))))
             (start-abs (caledonia--date-to-absolute (nth 0 range-start) (nth 1 range-start) (nth 2 range-start)))
             (end-abs (caledonia--date-to-absolute (nth 0 range-end) (nth 1 range-end) (nth 2 range-end))))
        ;; Iterate over every day in the range
        (cl-loop for abs from start-abs to end-abs
                 for date-key = (caledonia--absolute-to-date abs)
                 for year = (nth 0 date-key)
                 for month = (nth 1 date-key)
                 for day = (nth 2 date-key)
                 for day-events = (gethash date-key day-groups)
                 do
        ;; Insert date header
        (insert (propertize (caledonia--format-day-header year month day)
                            'face 'caledonia-agenda-date-face)
                "\n")
        ;; Insert events for this day (all-day events first, then by time)
        (dolist (event (sort (copy-sequence day-events)
                             (lambda (a b)
                               (let ((a-date (caledonia--get-key 'is_date a))
                                     (b-date (caledonia--get-key 'is_date b)))
                                 (cond
                                  ((and a-date (not b-date)) t)
                                  ((and (not a-date) b-date) nil)
                                  (t (string< (or (caledonia--get-key 'start a) "")
                                              (or (caledonia--get-key 'start b) ""))))))))
          (let* ((start (caledonia--get-key 'start event))
                 (end-val (caledonia--get-key 'end event))
                 (summary (or (caledonia--get-key 'summary event) "(no summary)"))
                 (calendar (or (caledonia--get-key 'calendar event) ""))
                 (location (caledonia--get-key 'location event))
                 (is-date (caledonia--get-key 'is_date event))
                 (start-parsed (caledonia--parse-iso-date start))
                 (start-time-str (if is-date
                                     "          "
                                   (format "%02d:%02d" (nth 3 start-parsed) (nth 4 start-parsed))))
                 (end-time-str (when (and end-val (not is-date))
                                 (let ((end-parsed (caledonia--parse-iso-date end-val)))
                                   (format "%02d:%02d" (nth 3 end-parsed) (nth 4 end-parsed)))))
                 (time-str (if is-date
                               "           "
                             (if end-time-str
                                 (format "%s-%s" start-time-str end-time-str)
                               (format "%s     " start-time-str))))
                 (location-str (if location (format " @ %s" location) ""))
                 (line (format "  %-12s %s  %s%s\n"
                               (propertize (concat calendar ":") 'face 'caledonia-calendar-name-face)
                               (propertize time-str 'face 'caledonia-agenda-time-face)
                               (propertize summary 'face 'caledonia-summary-face)
                               (propertize location-str 'face 'caledonia-location-face))))
            (insert (propertize line 'event-data event)))))))))

;; Agenda commands

(defun caledonia-show-event ()
  "Show details for the event on the current line."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (if event
        (caledonia--display-event-details event)
      (message "No event on this line"))))

(defun caledonia-open-event-file ()
  "Open the file for the event on the current line."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (if event
        (let ((file (caledonia--get-key 'file event))
              (event-id (caledonia--get-key 'id event)))
          (cond
           ((not file) (message "No file associated with this event"))
           ((not (file-exists-p file)) (message "File does not exist: %s" file))
           (t (find-file file)
              (caledonia--find-and-highlight-event-in-file file event-id))))
      (message "No event on this line"))))

(defun caledonia-refresh ()
  "Refresh the agenda view, reloading events from disk."
  (interactive)
  (when (eq major-mode 'caledonia-agenda-mode)
    (caledonia--send-request "Refresh")
    (let* ((query caledonia-agenda--query)
           (request-str (format "(Query %s)" (prin1-to-string query)))
           (payload (caledonia--send-request request-str))
           (events (caledonia--get-events payload))
           (from-date caledonia-agenda--from-date)
           (to-date caledonia-agenda--to-date))
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (caledonia--render-agenda events from-date to-date)
        (goto-char (min pos (point-max)))))))

(defun caledonia--resolve-date-to-ymd (date-str)
  "Resolve DATE-STR (like \"today\", \"+3m\", \"2025-04-01\") to (year month day).
Uses current time for relative dates."
  (when (and date-str (not (string-empty-p date-str)))
    (let* ((time (org-read-date nil t date-str))
           (decoded (decode-time time)))
      (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)))))

;; Event CRUD operations

(defun caledonia-add-event ()
  "Add a new event via the server.
Prompts for calendar, summary, start and end (using org-read-date).
End defaults to start + 1 hour. Location is optional."
  (interactive)
  (let* ((calendars (caledonia--get-available-calendars))
         (calendar (if (= (length calendars) 1)
                       (car calendars)
                     (completing-read "Calendar: " calendars nil t)))
         (summary (read-string "Summary: " nil 'caledonia-summary-history))
         (start (caledonia--read-datetime "Start"))
         (start-date (car start))
         (start-time (cdr start))
         ;; Default end: start + 1h if timed, same day if all-day
         (end-default (if start-time
                         (let* ((parsed (parse-time-string
                                         (format "%s %s" start-date start-time)))
                                (time (apply #'encode-time
                                             (append (cl-subseq parsed 0 6)
                                                     (list nil -1))))
                                (end-time (time-add time 3600)))
                           (format-time-string "%Y-%m-%d %H:%M" end-time))
                       start-date))
         (end (caledonia--read-datetime-with-default "End" end-default))
         (end-date (when end (car end)))
         (end-time (when end (cdr end)))
         (timezone (read-string "Timezone (blank for system default): "
                               nil 'caledonia-timezone-history))
         (location (read-string "Location: " nil 'caledonia-location-history))
         (fields `(("calendar" . ,calendar)
                   ("summary" . ,summary)
                   ("start_date" . ,start-date)
                   ("start_time" . ,start-time)
                   ("timezone" . ,(when (and timezone (not (string-empty-p timezone)))
                                    timezone))
                   ("end_date" . ,(when (and end-date (not (string= end-date start-date)))
                                    end-date))
                   ("end_time" . ,end-time)
                   ("location" . ,(when (not (string-empty-p location)) location))))
         (request-str (format "(CreateEvent (%s))"
                              (caledonia--build-sexp-fields fields))))
    (caledonia--send-request request-str)
    (message "Event created: %s" summary)
    (when (eq major-mode 'caledonia-agenda-mode)
      (caledonia-refresh))))

(defun caledonia-edit-event ()
  "Edit the event at point.
Prompts for summary, start, end, timezone, and location with current values as defaults."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (unless event
      (user-error "No event at point"))
    (let* ((id (caledonia--get-key 'id event))
           (cur-summary (or (caledonia--get-key 'summary event) ""))
           (cur-location (or (caledonia--get-key 'location event) ""))
           (cur-start (caledonia--get-key 'start event))
           (cur-end (caledonia--get-key 'end event))
           (cur-start-tz (caledonia--get-key 'start_tz event))
           (is-date (caledonia--get-key 'is_date event))
           ;; Format current values as defaults for org-read-date
           (start-default (when cur-start
                            (if is-date
                                (caledonia--format-timestamp cur-start "%Y-%m-%d")
                              (caledonia--format-timestamp cur-start "%Y-%m-%d %H:%M"))))
           (end-default (when cur-end
                          (if is-date
                              (caledonia--format-timestamp cur-end "%Y-%m-%d")
                            (caledonia--format-timestamp cur-end "%Y-%m-%d %H:%M"))))
           ;; Prompt for fields
           (summary (read-string (format "Summary [%s]: " cur-summary)
                                 nil 'caledonia-summary-history cur-summary))
           (start (caledonia--read-datetime-with-default "Start" start-default))
           (start-date (car start))
           (start-time (cdr start))
           (end (when cur-end
                  (caledonia--read-datetime-with-default "End" end-default)))
           (end-date (when end (car end)))
           (end-time (when end (cdr end)))
           (timezone (read-string
                      (format "Timezone [%s]: " (or cur-start-tz ""))
                      nil 'caledonia-timezone-history cur-start-tz))
           (location (read-string (format "Location [%s]: " cur-location)
                                  nil 'caledonia-location-history cur-location))
           ;; Build request
           (fields `(("id" . ,id)
                     ("summary" . ,summary)
                     ("start_date" . ,start-date)
                     ("start_time" . ,start-time)
                     ("end_date" . ,end-date)
                     ("end_time" . ,end-time)
                     ("timezone" . ,(when (and timezone (not (string-empty-p timezone)))
                                     timezone))
                     ("location" . ,(when (and location (not (string-empty-p location)))
                                     location))))
           (request-str (format "(EditEvent (%s))" (caledonia--build-sexp-fields fields))))
      (caledonia--send-request request-str)
      (message "Event updated: %s" summary)
      (when (eq major-mode 'caledonia-agenda-mode)
        (caledonia-refresh)))))

(defun caledonia-delete-event ()
  "Delete the event at point."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (unless event
      (user-error "No event at point"))
    (let ((id (caledonia--get-key 'id event))
          (summary (or (caledonia--get-key 'summary event) "(no summary)")))
      (when (y-or-n-p (format "Delete event '%s'? " summary))
        (caledonia--send-request (format "(DeleteEvent %S)" id))
        (message "Event deleted: %s" summary)
        (when (eq major-mode 'caledonia-agenda-mode)
          (caledonia-refresh))))))

;; Entry points

;;;###autoload
(defun caledonia-agenda (&optional from-date to-date)
  "Show an org-agenda style view of calendar events.
FROM-DATE and TO-DATE override defaults. With prefix arg, prompts for dates."
  (interactive
   (when current-prefix-arg
     (let ((dates (caledonia--read-date-range)))
       (list (car dates) (cdr dates)))))
  (let* ((from (or from-date caledonia-from-date))
         (to (or (and to-date (not (string-empty-p to-date)) to-date)
                 caledonia-to-date))
         (from-ymd (caledonia--resolve-date-to-ymd from))
         (to-ymd (caledonia--resolve-date-to-ymd to))
         (query `((to ,to)))
         (buffer (get-buffer-create caledonia--agenda-buffer)))
    (when (and from (not (string-empty-p from)))
      (setq query (append query `((from ,from)))))
    (let* ((request-str (format "(Query %s)" (prin1-to-string query)))
           (payload (caledonia--send-request request-str))
           (events (caledonia--get-events payload)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (caledonia-agenda-mode)
          (setq-local caledonia-agenda--query query)
          (setq-local caledonia-agenda--from-date from-ymd)
          (setq-local caledonia-agenda--to-date to-ymd)
          (caledonia--render-agenda events from-ymd to-ymd))
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

;;;###autoload
(defun caledonia-search (text)
  "Search for TEXT in calendar events, showing results in agenda view."
  (interactive
   (list (read-string "Search for: " nil 'caledonia-text-history)))
  (let* ((from caledonia-from-date)
         (to caledonia-to-date)
         (from-ymd (caledonia--resolve-date-to-ymd from))
         (to-ymd (caledonia--resolve-date-to-ymd to))
         (query `((text ,text) (to ,to)))
         (buffer (get-buffer-create caledonia--agenda-buffer)))
    (when (and from (not (string-empty-p from)))
      (setq query (append query `((from ,from)))))
    (let* ((request-str (format "(Query %s)" (prin1-to-string query)))
           (payload (caledonia--send-request request-str))
           (events (caledonia--get-events payload)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (caledonia-agenda-mode)
          (setq-local caledonia-agenda--query query)
          (setq-local caledonia-agenda--from-date from-ymd)
          (setq-local caledonia-agenda--to-date to-ymd)
          (caledonia--render-agenda events from-ymd to-ymd))
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

(provide 'caledonia)
;;; caledonia.el ends here
