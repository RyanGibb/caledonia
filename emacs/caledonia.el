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
  (let ((response-sexp (condition-case nil
                           (read caledonia--response-line)
                         (error (error "Caledonia: failed to parse response: %s"
                                       caledonia--response-line)))))
    (unless (and (listp response-sexp) (memq (car response-sexp) '(Ok Error)))
      (error "Caledonia: invalid response format: %S" response-sexp))
    (if (eq (car response-sexp) 'Error)
        (error "Caledonia: %s" (cadr response-sexp))
      (cadr response-sexp))))

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
        (let* ((summary (caledonia--get-key 'summary event))
               (calendar (caledonia--get-key 'calendar event))
               (start (caledonia--get-key 'start event))
               (end (caledonia--get-key 'end event))
               (start-tz (caledonia--get-key 'start_tz event))
               (end-tz (caledonia--get-key 'end_tz event))
               (is-date (caledonia--get-key 'is_date event))
               (recurring (caledonia--get-key 'recurring event))
               (alarms (caledonia--get-key 'alarms event))
               (location (caledonia--get-key 'location event))
               (description (caledonia--get-key 'description event))
               (file (caledonia--get-key 'file event))
               (start-fmt (when start
                            (if is-date
                                (caledonia--format-timestamp start "%Y-%m-%d")
                              (caledonia--format-timestamp start "%Y-%m-%d %H:%M"))))
               (end-fmt (when end
                          (if is-date
                              (caledonia--format-timestamp end "%Y-%m-%d")
                            (caledonia--format-timestamp end "%Y-%m-%d %H:%M")))))
          (when calendar
            (insert (propertize "Calendar: " 'face 'bold) calendar "\n"))
          (when summary
            (insert (propertize "Summary: " 'face 'bold) summary "\n"))
          (when start-fmt
            (insert (propertize "Start: " 'face 'bold) start-fmt
                    (if start-tz (format " (%s)" start-tz) "") "\n"))
          (when end-fmt
            (insert (propertize "End: " 'face 'bold) end-fmt
                    (if end-tz (format " (%s)" end-tz) "") "\n"))
          (when recurring
            (insert (propertize "Recurring: " 'face 'bold) "yes\n"))
          (when alarms
            (insert (propertize "Alarms: " 'face 'bold) alarms "\n"))
          (when location
            (insert (propertize "Location: " 'face 'bold) location "\n"))
          (when description
            (insert (propertize "Description: " 'face 'bold) description "\n"))
          (when file
            (insert "\n" (propertize "File: " 'face 'bold)
                    (propertize file 'face 'link
                                'mouse-face 'highlight
                                'help-echo "Click to open file"
                                'keymap (let ((map (make-sparse-keymap))
                                              (event-copy event))
                                          (define-key map [mouse-1]
                                                      (lambda ()
                                                        (interactive)
                                                        (find-file file)
                                                        (caledonia--find-and-highlight-event-in-file
                                                         file (caledonia--get-key 'id event-copy))))
                                          (define-key map (kbd "RET")
                                                      (lambda ()
                                                        (interactive)
                                                        (find-file file)
                                                        (caledonia--find-and-highlight-event-in-file
                                                         file (caledonia--get-key 'id event-copy))))
                                          map))
                    "\n")))))
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

(defun caledonia--get-available-calendars ()
  "Get list of available calendar names from server."
  (let ((response (caledonia--send-request "ListCalendars")))
    (if (and (listp response) (eq (car response) 'Calendars))
        (cadr response)
      nil)))

(defun caledonia--sexp-escape-string (s)
  "Escape S for sexp serialization, handling newlines and backslashes."
  (let ((escaped (replace-regexp-in-string "\\\\" "\\\\\\\\" s)))
    (setq escaped (replace-regexp-in-string "\n" "\\\\n" escaped))
    (setq escaped (replace-regexp-in-string "\"" "\\\\\"" escaped))
    (format "\"%s\"" escaped)))

(defun caledonia--sexp-field (key value)
  "Format KEY VALUE pair as sexp field string, or empty string if VALUE is nil."
  (cond
   ((null value) "")
   ((listp value)
    (format "(%s (%s))" key (mapconcat (lambda (s) (caledonia--sexp-escape-string s)) value " ")))
   (t (format "(%s %s)" key (caledonia--sexp-escape-string value)))))

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
    ;; Group events by date (multi-day events appear on each day they span)
    (dolist (event events)
      (let* ((start (caledonia--get-key 'start event))
             (end-val (caledonia--get-key 'end event))
             (start-parsed (caledonia--parse-iso-date start))
             (start-abs (caledonia--date-to-absolute (nth 0 start-parsed) (nth 1 start-parsed) (nth 2 start-parsed)))
             (end-abs (if end-val
                          (let ((end-parsed (caledonia--parse-iso-date end-val)))
                            (caledonia--date-to-absolute (nth 0 end-parsed) (nth 1 end-parsed) (nth 2 end-parsed)))
                        start-abs)))
        ;; For date events, end is exclusive (e.g. Mar 16-17 means just Mar 16)
        ;; For timed events, include the end day
        (let ((last-abs (if (caledonia--get-key 'is_date event)
                            (1- end-abs)
                          end-abs)))
          (cl-loop for abs from start-abs to (max start-abs last-abs)
                   for date-key = (caledonia--absolute-to-date abs)
                   do (puthash date-key
                               (append (gethash date-key day-groups) (list event))
                               day-groups)))))
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
           (from-date (caledonia--resolve-date-to-ymd (cadr (assq 'from query))))
           (to-date (caledonia--resolve-date-to-ymd (cadr (assq 'to query)))))
      (setq-local caledonia-agenda--from-date from-date)
      (setq-local caledonia-agenda--to-date to-date)
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

;; Timezone completion

(defvar caledonia--timezone-list nil
  "Cached list of IANA timezone names.")

(defun caledonia--timezone-list ()
  "Return a list of IANA timezone names from the system zoneinfo database."
  (or caledonia--timezone-list
      (setq caledonia--timezone-list
            (let ((zoneinfo-dir (cl-find-if #'file-directory-p
                                            '("/etc/zoneinfo"
                                              "/usr/share/zoneinfo"
                                              "/usr/lib/zoneinfo"
                                              "/usr/share/lib/zoneinfo"))))
              (when zoneinfo-dir
                (sort
                 (cl-remove-if-not
                  (lambda (s) (string-match-p "/" s))
                  (mapcar (lambda (f)
                            (string-remove-prefix (concat zoneinfo-dir "/") f))
                          (directory-files-recursively
                           zoneinfo-dir ""
                           nil
                           (lambda (dir)
                             (let ((name (file-name-nondirectory dir)))
                               (member name '("Africa" "America" "Antarctica" "Arctic"
                                              "Asia" "Atlantic" "Australia" "Europe"
                                              "Indian" "Pacific" "Etc")))))))
                 #'string<))))))

;; Event form buffer

(defvar caledonia-event-form-buffer "*Caledonia Event*"
  "Buffer name for the event form.")

(defvar-local caledonia-event-form--type nil
  "Type of form: `create' or `edit'.")

(defvar-local caledonia-event-form--id nil
  "Event ID when editing.")

(defvar-local caledonia-event-form--return-buffer nil
  "Buffer to return to after form submission.")

(defvar-local caledonia-event-form--occurrence-start nil
  "When editing a single occurrence, the RFC 3339 start_utc of that occurrence.")

(defvar caledonia-event-form--date-fields '("Start" "End")
  "Field names that should use org-read-date.")

(defvar caledonia-event-form--completing-fields '("Timezone" "End Timezone")
  "Field names that should use completing-read.")

(defvar caledonia-event-form-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") 'caledonia-event-form-submit)
    (define-key map (kbd "C-c C-k") 'caledonia-event-form-cancel)
    (define-key map (kbd "C-c C-d") 'caledonia-event-form-pick-date)
    (define-key map (kbd "TAB") 'caledonia-event-form-next-field)
    (define-key map (kbd "<backtab>") 'caledonia-event-form-prev-field)
    (define-key map (kbd "RET") 'caledonia-event-form-newline)
    map)
  "Keymap for Caledonia event form mode.")

(defun caledonia-event-form-newline ()
  "Insert a newline in the Description field, or move to next field otherwise."
  (interactive)
  (if (string= (caledonia-event-form--current-field) "Description")
      (newline)
    (caledonia-event-form-next-field)))

(define-derived-mode caledonia-event-form-mode text-mode "Caledonia-Event"
  "Major mode for editing calendar event fields.
\\<caledonia-event-form-mode-map>
\\[caledonia-event-form-submit] to submit, \\[caledonia-event-form-cancel] to cancel.
TAB to next field (opens org-read-date on date fields), S-TAB to previous field.")

(defun caledonia-event-form--insert-field (name &optional value)
  "Insert a form field with NAME as read-only label and VALUE as editable.
If NAME is \"Description\", the field supports multiple lines."
  (let ((start (point)))
    (insert (propertize (format "%s: " name)
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky '(read-only face)
                        'face 'bold
                        'field-name name))
    (if (string= name "Description")
        (insert (or value "") "\n")
      (insert (or value ""))
      (insert (propertize "\n" 'read-only t
                          'front-sticky nil
                          'rear-nonsticky '(read-only))))))

(defun caledonia-event-form--insert-help ()
  "Insert the help text at the bottom of the form."
  (insert (propertize "\n" 'read-only t
                      'rear-nonsticky '(read-only)))
  (let ((help (concat
               (propertize "TAB" 'face 'bold) " next field  "
               (propertize "S-TAB" 'face 'bold) " prev field  "
               (propertize "C-c C-c" 'face 'bold) " submit  "
               (propertize "C-c C-k" 'face 'bold) " cancel")))
    (insert (propertize help 'read-only t))))

(defun caledonia-event-form--get-field (name)
  "Get the value of field NAME from the form buffer.
For the Description field, captures multiple lines up to the help text."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^%s: " (regexp-quote name)) nil t)
      (let* ((value-start (point))
             (value-end (if (string= name "Description")
                            ;; Capture everything until the help separator
                            (or (and (re-search-forward "^\n" nil t)
                                     (match-beginning 0))
                                (point-max))
                          (line-end-position)))
             (val (string-trim (buffer-substring-no-properties value-start value-end))))
        (unless (string-empty-p val) val)))))

(defun caledonia-event-form--current-field ()
  "Return the field name on the current line, or nil.
For multi-line fields like Description, walks backwards to find the label."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (if (string-match "^\\([^:]+\\): " line)
        (match-string 1 line)
      ;; On a continuation line — walk backwards to find the field label
      (save-excursion
        (while (and (not (bobp))
                    (let ((l (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))))
                      (not (string-match "^\\([^:]+\\): " l))))
          (forward-line -1))
        (let ((l (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([^:]+\\): " l)
            (match-string 1 l)))))))

(defun caledonia-event-form--goto-field-value ()
  "Move point to the value portion of the current field line."
  (beginning-of-line)
  (when (re-search-forward "^[^:]+: " (line-end-position) t)
    (point)))

(defun caledonia-event-form--on-label-line-p ()
  "Return non-nil if the current line has a field label (Name: ...)."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (string-match-p "^[^:]+: " line)))

(defun caledonia-event-form-next-field ()
  "Move to the next field.  On date fields, open org-read-date."
  (interactive)
  (let ((start-field (caledonia-event-form--current-field)))
    (forward-line 1)
    ;; Skip continuation lines of the current field and non-field lines
    (while (and (not (eobp))
                (let ((f (caledonia-event-form--current-field)))
                  (or (not f)
                      (and (equal f start-field)
                           (not (caledonia-event-form--on-label-line-p))))))
      (forward-line 1))
    (when (caledonia-event-form--on-label-line-p)
      (caledonia-event-form--goto-field-value)
      (let ((field (caledonia-event-form--current-field)))
        (cond
         ((member field caledonia-event-form--date-fields)
          (caledonia-event-form-pick-date))
         ((member field caledonia-event-form--completing-fields)
          (caledonia-event-form-pick-completing)))))))

(defun caledonia-event-form-prev-field ()
  "Move to the previous field.  On date fields, open org-read-date."
  (interactive)
  (let ((start-field (caledonia-event-form--current-field)))
    ;; Move up past current field's label line
    (forward-line -1)
    (while (and (not (bobp))
                (let ((f (caledonia-event-form--current-field)))
                  (or (not f)
                      (and (equal f start-field)
                           (not (caledonia-event-form--on-label-line-p))))))
      (forward-line -1))
    ;; If we landed on the same field's label, go up one more field
    (when (and (equal (caledonia-event-form--current-field) start-field)
               (caledonia-event-form--on-label-line-p)
               (not (bobp)))
      (forward-line -1)
      (while (and (not (bobp))
                  (not (caledonia-event-form--on-label-line-p)))
        (forward-line -1)))
    (when (caledonia-event-form--on-label-line-p)
      (caledonia-event-form--goto-field-value)
      (let ((field (caledonia-event-form--current-field)))
        (cond
         ((member field caledonia-event-form--date-fields)
          (caledonia-event-form-pick-date))
         ((member field caledonia-event-form--completing-fields)
          (caledonia-event-form-pick-completing)))))))

(defun caledonia-event-form-pick-completing ()
  "Use completing-read for the field at point (e.g. timezone)."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line (buffer-substring-no-properties line-start line-end)))
    (when (string-match "^\\([^:]+\\): \\(.*\\)$" line)
      (let* ((field (match-string 1 line))
             (current (string-trim (match-string 2 line)))
             (candidates (pcase field
                           ("Timezone" (caledonia--timezone-list))
                           ("End Timezone" (caledonia--timezone-list))
                           (_ nil)))
             (new-val (completing-read (format "%s: " field)
                                       candidates nil nil
                                       (unless (string-empty-p current) current)
                                       'caledonia-timezone-history)))
        (let ((inhibit-read-only t))
          (delete-region line-start (min (1+ line-end) (point-max)))
          (caledonia-event-form--insert-field field new-val)
          (forward-line -1)
          (caledonia-event-form--goto-field-value))))))

(defun caledonia-event-form-pick-date ()
  "Use org-read-date to pick a date for the field at point."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line (buffer-substring-no-properties line-start line-end)))
    (when (string-match "^\\([^:]+\\): \\(.*\\)$" line)
      (let* ((field (match-string 1 line))
             (current (string-trim (match-string 2 line)))
             (default (unless (string-empty-p current) current))
             (new-val (org-read-date nil nil nil (format "%s: " field) nil default t)))
        (let ((inhibit-read-only t))
          (delete-region line-start (min (1+ line-end) (point-max)))
          (caledonia-event-form--insert-field field new-val)
          (forward-line -1)
          (caledonia-event-form--goto-field-value))))))

(defun caledonia-event-form--parse-datetime (str)
  "Parse STR as \"YYYY-MM-DD HH:MM\" or \"YYYY-MM-DD\".
Returns (date . time) where time may be nil."
  (when str
    (let ((parts (split-string str " ")))
      (cons (car parts)
            (when (and (cdr parts) (string-match-p "^[0-9][0-9]:[0-9][0-9]" (cadr parts)))
              (cadr parts))))))

(defun caledonia-event-form-submit ()
  "Submit the event form."
  (interactive)
  (let* ((type caledonia-event-form--type)
         (calendar (caledonia-event-form--get-field "Calendar"))
         (summary (caledonia-event-form--get-field "Summary"))
         (start-str (caledonia-event-form--get-field "Start"))
         (end-str (caledonia-event-form--get-field "End"))
         (timezone (caledonia-event-form--get-field "Timezone"))
         (end-timezone (caledonia-event-form--get-field "End Timezone"))
         (recurrence (caledonia-event-form--get-field "Recurrence"))
         (alarms-str (caledonia-event-form--get-field "Alarms"))
         (alarms (when alarms-str
                   (mapcar #'string-trim
                           (split-string alarms-str "," t "[ \t]+"))))
         (location (caledonia-event-form--get-field "Location"))
         (description (caledonia-event-form--get-field "Description"))
         (start (caledonia-event-form--parse-datetime start-str))
         (start-date (when start (car start)))
         (start-time (when start (cdr start)))
         (end (caledonia-event-form--parse-datetime end-str))
         (end-date (when end (car end)))
         (end-time (when end (cdr end)))
         (return-buf caledonia-event-form--return-buffer))
    ;; Submit to server — let server validate, report errors via user-error
    (condition-case err
        (progn
          (pcase type
            ('create
             (let* ((fields `(("calendar" . ,calendar)
                              ("summary" . ,summary)
                              ("start_date" . ,start-date)
                              ("start_time" . ,start-time)
                              ("timezone" . ,timezone)
                              ("end_timezone" . ,end-timezone)
                              ("end_date" . ,end-date)
                              ("end_time" . ,end-time)
                              ("recurrence" . ,recurrence)
                              ("alarms" . ,alarms)
                              ("location" . ,location)
                              ("description" . ,description)))
                    (request-str (format "(CreateEvent (%s))"
                                         (caledonia--build-sexp-fields fields))))
               (caledonia--send-request request-str)
               (message "Event created: %s" summary)))
            ('edit
             (let* ((id caledonia-event-form--id)
                    (occurrence-start caledonia-event-form--occurrence-start)
                    (fields `(("id" . ,id)
                              ("summary" . ,summary)
                              ("start_date" . ,start-date)
                              ("start_time" . ,start-time)
                              ("end_date" . ,end-date)
                              ("end_time" . ,end-time)
                              ("timezone" . ,timezone)
                              ("end_timezone" . ,end-timezone)
                              ("recurrence" . ,recurrence)
                              ("alarms" . ,alarms)
                              ("location" . ,location)
                              ("description" . ,description)
                              ("occurrence_start" . ,occurrence-start)))
                    (request-str (format "(EditEvent (%s))"
                                         (caledonia--build-sexp-fields fields))))
               (caledonia--send-request request-str)
               (message "Event updated: %s" (or summary "(no summary)")))))
          ;; Only close form and refresh on success
          (quit-window t)
          (when (and return-buf (buffer-live-p return-buf))
            (switch-to-buffer return-buf)
            (when (eq major-mode 'caledonia-agenda-mode)
              (caledonia-refresh))))
      (error (user-error "%s" (error-message-string err))))))

(defun caledonia-event-form-cancel ()
  "Cancel the event form."
  (interactive)
  (let ((return-buf caledonia-event-form--return-buffer))
    (quit-window t)
    (when (and return-buf (buffer-live-p return-buf))
      (switch-to-buffer return-buf))
    (message "Cancelled.")))

(defun caledonia-add-event ()
  "Add a new event using a form buffer.
Fill in the fields, then press C-c C-c to create or C-c C-k to cancel.
Use C-c C-d on a date field to pick with org-read-date."
  (interactive)
  (let* ((calendars (caledonia--get-available-calendars))
         (calendar (if (= (length calendars) 1)
                       (car calendars)
                     (completing-read "Calendar: " calendars nil t)))
         (return-buf (current-buffer))
         (buf (get-buffer-create caledonia-event-form-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (caledonia-event-form-mode)
      (setq-local caledonia-event-form--type 'create)
      (setq-local caledonia-event-form--return-buffer return-buf)
      (let ((inhibit-read-only t))
        (caledonia-event-form--insert-field "Calendar" calendar)
        (caledonia-event-form--insert-field "Summary")
        (caledonia-event-form--insert-field "Start")
        (caledonia-event-form--insert-field "End")
        (caledonia-event-form--insert-field "Timezone")
        (caledonia-event-form--insert-field "End Timezone")
        (caledonia-event-form--insert-field "Recurrence")
        (caledonia-event-form--insert-field "Alarms")
        (caledonia-event-form--insert-field "Location")
        (caledonia-event-form--insert-field "Description")
        (caledonia-event-form--insert-help))
      ;; Position cursor on Summary field value
      (goto-char (point-min))
      (re-search-forward "^Summary: " nil t))
    (switch-to-buffer-other-window buf)))

(defun caledonia-edit-event ()
  "Edit the event at point using a form buffer.
Fill in the fields, then press C-c C-c to save or C-c C-k to cancel.
Use C-c C-d on a date field to pick with org-read-date.
If the event is recurring, prompt whether to edit this occurrence or all."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (unless event
      (user-error "No event at point"))
    (let* ((id (caledonia--get-key 'id event))
           (summary (or (caledonia--get-key 'summary event) ""))
           (location (or (caledonia--get-key 'location event) ""))
           (description (or (caledonia--get-key 'description event) ""))
           (calendar (or (caledonia--get-key 'calendar event) ""))
           (start (caledonia--get-key 'start event))
           (end (caledonia--get-key 'end event))
           (start-tz (or (caledonia--get-key 'start_tz event) ""))
           (end-tz (or (caledonia--get-key 'end_tz event) ""))
           (alarms (or (caledonia--get-key 'alarms event) ""))
           (recurring (caledonia--get-key 'recurring event))
           (start-utc (caledonia--get-key 'start_utc event))
           (is-date (caledonia--get-key 'is_date event))
           (occurrence-start
            (when recurring
              (let ((scope (completing-read
                            (format "Edit '%s': " summary)
                            '("This event" "All events in series")
                            nil t nil nil "This event")))
                (when (string= scope "This event") start-utc))))
           (start-str (when start
                        (if is-date
                            (caledonia--format-timestamp start "%Y-%m-%d")
                          (caledonia--format-timestamp start "%Y-%m-%d %H:%M"))))
           (end-str (when end
                      (if is-date
                          (caledonia--format-timestamp end "%Y-%m-%d")
                        (caledonia--format-timestamp end "%Y-%m-%d %H:%M"))))
           (return-buf (current-buffer))
           (buf (get-buffer-create caledonia-event-form-buffer)))
      (with-current-buffer buf
        (erase-buffer)
        (caledonia-event-form-mode)
        (setq-local caledonia-event-form--type 'edit)
        (setq-local caledonia-event-form--id id)
        (setq-local caledonia-event-form--occurrence-start occurrence-start)
        (setq-local caledonia-event-form--return-buffer return-buf)
        (let ((inhibit-read-only t))
          (caledonia-event-form--insert-field "Calendar" calendar)
          (caledonia-event-form--insert-field "Summary" summary)
          (caledonia-event-form--insert-field "Start" start-str)
          (caledonia-event-form--insert-field "End" end-str)
          (caledonia-event-form--insert-field "Timezone" start-tz)
          (caledonia-event-form--insert-field "End Timezone" end-tz)
          (unless occurrence-start
            (caledonia-event-form--insert-field "Recurrence"))
          (caledonia-event-form--insert-field "Alarms" alarms)
          (caledonia-event-form--insert-field "Location" location)
          (caledonia-event-form--insert-field "Description" description)
          (caledonia-event-form--insert-help))
        ;; Position cursor on Summary field value
        (goto-char (point-min))
        (re-search-forward "^Summary: " nil t))
      (switch-to-buffer-other-window buf))))

(defun caledonia-delete-event ()
  "Delete the event at point.
If the event is recurring, prompt whether to delete this occurrence or all."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (unless event
      (user-error "No event at point"))
    (let* ((id (caledonia--get-key 'id event))
           (summary (or (caledonia--get-key 'summary event) "(no summary)"))
           (recurring (caledonia--get-key 'recurring event))
           (start-utc (caledonia--get-key 'start_utc event))
           (scope (if recurring
                      (completing-read
                       (format "Delete '%s': " summary)
                       '("This event" "All events in series")
                       nil t nil nil "This event")
                    "all")))
      (when (y-or-n-p (format "Delete %s? "
                               (if (string= scope "All events in series")
                                   (format "all events in series '%s'" summary)
                                 (format "event '%s'" summary))))
        (let ((request-str
               (if (and recurring (string= scope "This event") start-utc)
                   (format "(DeleteEvent ((id %S)(occurrence_start %S)))" id start-utc)
                 (format "(DeleteEvent ((id %S)))" id))))
          (caledonia--send-request request-str)
          (message "Event deleted: %s" summary)
          (when (eq major-mode 'caledonia-agenda-mode)
            (caledonia-refresh)))))))

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
      (pop-to-buffer-same-window buffer))))

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
      (pop-to-buffer-same-window buffer))))

(provide 'caledonia)
;;; caledonia.el ends here
