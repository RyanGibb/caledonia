;;; caledonia.el --- Emacs integration for Caledonia -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ryan Gibb

;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Version: 0.4.0
;; Keywords: calendar
;; Package-Requires: ((emacs "24.4"))
;; URL: https://ryan.freumh.org/caledonia.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an Emacs interface to the Caledonia calendar CLI.
;; It communicates with Caledonia using S-expressions for data exchange.

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

(defcustom caledonia-calendar-column-width 0
  "Column width for the Calendar entry."
  :type 'natnum)

(defcustom caledonia-start-column-width 0
  "Column width for the Start entry."
  :type 'natnum)

(defcustom caledonia-end-column-width 0
  "Column width for the End entry."
  :type 'natnum)

(defcustom caledonia-list-from-date "today"
  "Default start date for calendar list view."
  :type 'string
  :group 'caledonia)

(defcustom caledonia-list-to-date "+3m"
  "Default end date for calendar list view (3 months from today)."
  :type 'string
  :group 'caledonia)

(defcustom caledonia-search-from-date nil
  "Default start date for calendar search; nil means no start date limit."
  :type 'string
  :group 'caledonia)

(defcustom caledonia-search-to-date "+75y"
  "Default end date for calendar search (75 years from today)."
  :type 'string
  :group 'caledonia)

;; Define histories for input fields

(defvar caledonia-from-history nil "History for from date inputs.")
(defvar caledonia-to-history nil "History for to date inputs.")
(defvar caledonia-timezone-history nil "History for timezone inputs.")
(defvar caledonia-calendars-history nil "History for calendar inputs.")
(defvar caledonia-text-history nil "History for search text inputs.")
(defvar caledonia-search-fields-history nil "History for search fields inputs.")
(defvar caledonia-id-history nil "History for event ID inputs.")
(defvar caledonia-limit-history nil "History for limit inputs.")
(defvar caledonia-search-prompt-history nil "History for search prompt inputs.")

;; Internal variables

(defvar caledonia--events-buffer "*Caledonia Events*"
  "Buffer name for displaying Caledonia events.")
(defvar caledonia--details-buffer "*Caledonia Event Details*"
  "Buffer name for displaying Caledonia event details.")
(defvar caledonia--server-process nil
  "The persistent Caledonia server process.")
(defvar caledonia--server-buffer-name "*caledonia-server-io*"
  "Buffer for server process I/O.")
(defvar caledonia--response-line nil
  "Last response line received.")
(defvar caledonia--response-flag nil
  "Non-nil means a responce has been recieved.")
(defvar-local caledonia--current-query nil
  "The current query parameters being displayed in this buffer.")

;; API functions

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
  "Send REQUEST-STR and get responce back."
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

;; UI functions

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

(defun caledonia--tabulated-list-entries (events)
  "Convert EVENTS for a format suitable for showing via a tabulated-list-mode'."
  (let ((max-calendar-width 0)
        (max-start-width 0)
        (max-end-width 0)
        (tabulated-list-entries nil))
    ;; first pass: calculate maximum widths
    (dolist (event events)
      (let* ((calendar (caledonia--get-key 'calendar event))
             (start (caledonia--get-key 'start event))
             (end (caledonia--get-key 'end event))
             (start-tz (cadr (assoc 'start_tz event)))
             (cal-str (if (not calendar) "unkown" calendar))
             (start-str (caledonia--format-timestamp start))
             (start-str (if start-tz
                            (format "%s (%s)" start-str start-tz)
                          start-str))
             (end-str (when end
                        (caledonia--format-timestamp (format "%s" end)))))
        (setq max-calendar-width (max max-calendar-width (length cal-str)))
        (setq max-start-width (max max-start-width (+ (length start-str) 2)))
        (setq max-end-width (max max-end-width (length end-str)))))
    (setq caledonia-calendar-column-width  (max max-calendar-width (length "Calendar")))
    (setq caledonia-start-column-width (max max-start-width (length "Start")))
    (setq caledonia-end-column-width  (max max-end-width (length "End")))
    ;; second pass: prepare tabulated-list entries with properties
    (setq tabulated-list-entries
          (mapcar (lambda (event)
                    (let* (
                           (id (caledonia--get-key 'id event))
                           (summary (caledonia--get-key 'summary event))
                           (start (caledonia--get-key 'start event))
                           (end (caledonia--get-key 'end event))
                           (location (caledonia--get-key 'location event))
                           (calendar (caledonia--get-key 'calendar event))
                           (start-tz (caledonia--get-key 'start_tz event))
                           (start-str (caledonia--format-timestamp start))
                           (start-str (if start-tz
                                          (format "%s (%s)" start-str start-tz)
                                        start-str))
                           (end-str (if end (caledonia--format-timestamp (format "%s" end)) ""))
                           (start-str (if end (format "%s -" start-str) start-str))
                           (location-str (if location (concat " @ " location) ""))
                           (cal-prop (propertize calendar 'face 'caledonia-calendar-name-face))
                           (start-prop (propertize start-str 'face 'caledonia-date-face))
                           (end-prop (propertize end-str 'face 'caledonia-date-face))
                           (summary-prop (propertize (concat summary location-str)
                                                     'face 'caledonia-summary-face))
                           ;; Store the full event data as a text property for retrieval
                           (entry-id (propertize (format "%s" id) 'event-data event)))
                      (list entry-id (vector cal-prop start-prop end-prop summary-prop))))
                  events))
    tabulated-list-entries))

(defun caledonia--sort-calendar (A B)
  "Sort function for calendar column between A and B."
  (let ((a (aref (cadr A) 0))
        (b (aref (cadr B) 0)))
    (string< a b)))

(defun caledonia--sort-start (A B)
  "Sort function for date/time column between A and B."
  (let ((a (aref (cadr A) 1))
        (b (aref (cadr B) 1)))
    (time-less-p (date-to-time a) (date-to-time b))))

(defun caledonia--sort-end (A B)
  "Sort function for date/time column between A and B."
  (let ((a (aref (cadr A) 2))
        (b (aref (cadr B) 2)))
    (time-less-p (date-to-time a) (date-to-time b))))

(defun caledonia--make-query (&optional query)
  "Make a query with the QUERY S-expression.
If QUERY is nil, use the current query stored in `caledonia--current-query`."
  (interactive)
  (let* ((query-to-use (or query caledonia--current-query '()))  ;; Use current query if available
         ;; Ensure to date is set if not present in query
         (query-to-use (if (assq 'to query-to-use)
                           query-to-use
                         (cons `(to ,caledonia-list-to-date) query-to-use)))
         (request-str (format "(Query %s)" (prin1-to-string query-to-use)))
         (payload (caledonia--send-request request-str))
         (events (caledonia--get-events payload))
         (entries (caledonia--tabulated-list-entries events)))
    ;; Save this query for future refreshes if explicitly provided
    (when query
      (setq-local caledonia--current-query query-to-use))
    (setq tabulated-list-entries entries))
  (setq tabulated-list-format
        `[("Calendar" ,caledonia-calendar-column-width caledonia--sort-calendar)
          ("Start" ,caledonia-start-column-width caledonia--sort-start)
          ("End" ,caledonia-end-column-width caledonia--sort-end)
          ("Summary" 0 t)])
  (setq tabulated-list-sort-key (cons "Start" nil))
  (tabulated-list-init-header)
  (tabulated-list-print t))

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

;; Query parameter modification functions

(defun caledonia-query-date-range ()
  "Set the date range for the current calendar view."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((dates (caledonia--read-date-range))
           (from (car dates))
           (to (cdr dates))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the new date range
      (setq new-query (assq-delete-all 'from new-query))
      (setq new-query (assq-delete-all 'to new-query))
      (when (and from (not (string-empty-p from)))
        (push `(from ,from) new-query))
      (when (and to (not (string-empty-p to)))
        (push `(to ,to) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

(defun caledonia-query-calendars ()
  "Set the calendars to filter by for the current calendar view.
Fetches available calendars from server to allow selection from a list."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((available-calendars
            (caledonia--send-request "ListCalendars"))
           (calendars-list
            (if (and (listp available-calendars)
                     (eq (car available-calendars) 'Calendars))
                (cadr available-calendars)
              (progn
                (message "Failed to get calendar list from server")
                nil)))
           ;; Use completing-read-multiple to select from available calendars
           (selected-calendars
            (completing-read-multiple
             "Select calendars (comma-separated, empty for all): "
             ;; Use empty list if no calendars found
             (or calendars-list '())
             nil nil
             (let ((current-calendars (cdr (assq 'calendars caledonia--current-query))))
               (when current-calendars
                 (mapconcat #'identity current-calendars ",")))
             'caledonia-calendars-history))
           (calendars (mapcar #'string-trim selected-calendars))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the new calendars
      (setq new-query (assq-delete-all 'calendars new-query))
      (when (and calendars (not (null calendars)))
        (push `(calendars ,calendars) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

(defun caledonia-query-text ()
  "Set the search text for the current calendar view."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((text (read-string "Search text (leave empty for no text search): "
                              nil 'caledonia-text-history))
           (search-in-str (when (and text (not (string-empty-p text)))
                            (read-string "Search in (summary,description,location - leave empty for all): "
                                         nil 'caledonia-search-fields-history)))
           (search-in (when (and search-in-str (not (string-empty-p search-in-str)))
                        (mapcar (lambda (field)
                                  (intern (string-trim field)))
                                (split-string search-in-str "," t))))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the new text search parameters
      (setq new-query (assq-delete-all 'text new-query))
      (setq new-query (assq-delete-all 'search_in new-query))
      (when (and text (not (string-empty-p text)))
        (push `(text ,text) new-query))
      (when search-in
        (push `(search_in ,search-in) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

(defun caledonia-query-id ()
  "Set the event ID to filter by for the current calendar view."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((id (read-string "Event ID (leave empty for all events): "
                            nil 'caledonia-id-history))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the new ID
      (setq new-query (assq-delete-all 'id new-query))
      (when (and id (not (string-empty-p id)))
        (push `(id ,id) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

(defun caledonia-query-recurring ()
  "Set whether to filter by recurring events for the current calendar view."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((recurring (completing-read "Recurring events (yes/no/all, leave empty for all): "
                                       '("" "yes" "no") nil nil nil))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the recurring filter
      (setq new-query (assq-delete-all 'recurring new-query))
      (when (not (string-empty-p recurring))
        (push `(recurring ,(if (string= recurring "yes") t nil)) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

(defun caledonia-query-limit ()
  "Set the maximum number of events to show in the current calendar view."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((limit-str (read-string "Maximum events to show (leave empty for no limit): "
                                   nil 'caledonia-limit-history))
           (limit (when (and limit-str (not (string-empty-p limit-str)))
                    (string-to-number limit-str)))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the new limit
      (setq new-query (assq-delete-all 'limit new-query))
      (when limit
        (push `(limit ,limit) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

(defun caledonia-query-timezone ()
  "Set the timezone for the current calendar view."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((timezone-str (read-string "Timezone (e.g. Europe/London, leave empty for default): "
                                      nil 'caledonia-timezone-history))
           (timezone (when (not (string-empty-p timezone-str)) timezone-str))
           (current-query caledonia--current-query)
           (new-query (copy-tree current-query)))
      ;; Update the query with the new timezone
      (setq new-query (assq-delete-all 'timezone new-query))
      (when timezone
        (push `(timezone ,timezone) new-query))
      ;; Execute the updated query
      (caledonia--make-query new-query))))

;; Buffer functions

(defun caledonia-show-event ()
  "Show details for the event at point in a separate buffer."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((id (tabulated-list-get-id))
           (event (when id (get-text-property 0 'event-data id))))
      (if event
          (caledonia--display-event-details event)
        (message "No event at point")))))

(defun caledonia-open-event-file ()
  "Open the file associated with the event at point.
If the file contains the event ID, the cursor will be positioned at that
location."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((id (tabulated-list-get-id))
           (event (when id (get-text-property 0 'event-data id)))
           (file (when event (caledonia--get-key 'file event)))
           (event-id (when event (caledonia--get-key 'id event))))
      (cond
       ((not event)
        (message "No event at point"))
       ((not file)
        (message "No file associated with this event"))
       ((not (file-exists-p file))
        (message "File does not exist: %s" file))
       (t
        (find-file file)
        (caledonia--find-and-highlight-event-in-file file event-id))))))

(defun caledonia-refresh ()
  "Refresh calendar data from disk and update the current view.
This is useful when calendar files have been modified outside Emacs."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    ;; Send a refresh command to clear the server's cache
    (caledonia--send-request "Refresh")
    ;; Re-apply the current query to update the view
    (when (string= (buffer-name) caledonia--events-buffer)
      ;; Just use caledonia--make-query without args to use the stored query
      (caledonia--make-query))))

;; Entry functions

(defun caledonia-query ()
  "Query events with interactive prompts for all filter parameters.
Opens a series of prompts to build a complete query and then displays the
results. After the initial query is displayed, you can further refine the
results using the caledonia-query-* family of functions."
  (interactive)
  (let* (
         (dates (caledonia--read-date-range))
         (from (car dates))
         (to (cdr dates))
         (timezone-str (read-string "Timezone (e.g. Europe/London, leave empty for default): "
                                    nil 'caledonia-timezone-history))
         (timezone (when (not (string-empty-p timezone-str)) timezone-str))
         (available-calendars
          (caledonia--send-request "ListCalendars"))
         (calendars-list
          (if (and (listp available-calendars)
                   (eq (car available-calendars) 'Calendars))
              (cadr available-calendars)
            (progn
              (message "Failed to get calendar list from server")
              nil)))
         (selected-calendars
          (completing-read-multiple
           "Select calendars (comma-separated, empty for all): "
           (or calendars-list '()) nil nil nil 'caledonia-calendars-history))
         (calendars (mapcar #'string-trim selected-calendars))
         (text (read-string "Search text (leave empty for no text search): "
                            nil 'caledonia-text-history))
         (search-in-str (when (and text (not (string-empty-p text)))
                          (read-string "Search in (summary,description,location - leave empty for all): "
                                       nil 'caledonia-search-fields-history)))
         (search-in (when (and search-in-str (not (string-empty-p search-in-str)))
                      (mapcar (lambda (field)
                                (intern (string-trim field)))
                              (split-string search-in-str "," t))))
         (id (read-string "Event ID (leave empty for all events): "
                          nil 'caledonia-id-history))
         (recurring (completing-read "Recurring events (yes/no/all, leave empty for all): "
                                     '("" "yes" "no") nil nil nil))
         (limit-str (read-string "Maximum events to show (leave empty for no limit): "
                                 nil 'caledonia-limit-history))
         (limit (when (and limit-str (not (string-empty-p limit-str)))
                  (string-to-number limit-str)))
         (query nil))
    ;; Build query based on parameters
    (when (and from (not (string-empty-p from)))
      (push `(from ,from) query))
    (when (and to (not (string-empty-p to)))
      (push `(to ,to) query))
    (when timezone
      (push `(timezone ,timezone) query))
    (when calendars
      (push `(calendars ,calendars) query))
    (when (and text (not (string-empty-p text)))
      (push `(text ,text) query))
    (when search-in
      (push `(search_in ,search-in) query))
    (when (and id (not (string-empty-p id)))
      (push `(id ,id) query))
    (when (not (string-empty-p recurring))
      (push `(recurring ,(if (string= recurring "yes") t nil)) query))
    (when limit
      (push `(limit ,limit) query))
    ;; Create buffer and execute query
    (let ((buffer (get-buffer-create caledonia--events-buffer)))
      (with-current-buffer buffer
        ;; Clear the buffer and reset it
        (let ((inhibit-read-only t))
          (erase-buffer))
        ;; Activate our mode and make the query
        (caledonia-mode)
        (caledonia--make-query query)
        (switch-to-buffer buffer)))))

(defun caledonia-list (&optional from-date to-date)
  "List calendar in a new buffer within the default date range.
FROM-DATE and TO-DATE override the default date range if provided. TO-DATE is
required and will use a default if not specified. With prefix arg, prompts for
the date range with an interactive calendar."
  (interactive
   (when current-prefix-arg
     (let* ((dates (caledonia--read-date-range)))
       (list (car dates) (cdr dates)))))
  (let ((buffer (get-buffer-create caledonia--events-buffer))
        (from (or from-date caledonia-list-from-date))
        ;; Ensure to date is always provided
        (to (or (and to-date (not (string-empty-p to-date)) to-date)
                caledonia-list-to-date)))
    (with-current-buffer buffer
      ;; Clear the buffer and reset it
      (let ((inhibit-read-only t))
        (erase-buffer))
      ;; Build the query
      (let* ((query `((to ,to))))
        ;; Add from date only if specified
        (when (and from (not (string-empty-p from)))
          (setq query (append query `((from ,from)))))
        ;; Activate our mode and make the query
        (caledonia-mode)
        (caledonia--make-query query)
        (switch-to-buffer buffer)))))

(defun caledonia-search (&optional expr from-date to-date)
  "Search for query EXPR with optional FROM-DATE and TO-DATE.
This is an interactive function which asks user for EXPR if not passed as an
argument. With prefix arg, also prompts for date range with an interactive
calendar. Use this to find events matching specific text across all calendars.
TO-DATE is required; a default will be used if not provided."
  (interactive
   (let* ((search-text (read-string "Search for: " nil 'caledonia-search-prompt-history))
          (dates (when current-prefix-arg (caledonia--read-date-range))))
     (list search-text
           (when current-prefix-arg (car dates))
           (when current-prefix-arg (cdr dates)))))
  (let ((buffer (get-buffer-create caledonia--events-buffer))
        (from (or from-date caledonia-search-from-date))
        (to (or to-date caledonia-search-to-date)))
    (with-current-buffer buffer
      ;; Clear the buffer and reset it
      (let ((inhibit-read-only t))
        (erase-buffer))
      ;; Build the query
      (let* ((query `((text ,expr)(to ,to))))
        ;; Add from date only if specified
        (when (and from (not (string-empty-p from)))
          (setq query (append query `((from ,from)))))
        ;; Activate our mode and make the query
        (caledonia-mode)
        (caledonia--make-query query)
        (switch-to-buffer buffer)))))

;; Event CRUD operations

(defun caledonia--get-available-calendars ()
  "Get list of available calendar names from server."
  (let ((response (caledonia--send-request "ListCalendars")))
    (if (and (listp response) (eq (car response) 'Calendars))
        (cadr response)
      nil)))

(defun caledonia--read-date (prompt &optional default)
  "Read a date with PROMPT using org-read-date, with optional DEFAULT."
  (let ((date (org-read-date nil nil nil prompt
                             (when default
                               (date-to-time default))
                             nil t)))
    (when (and date (not (string-empty-p date)))
      date)))

(defun caledonia--read-time (prompt &optional default)
  "Read a time with PROMPT, with optional DEFAULT value."
  (let ((time (read-string (if default
                               (format "%s [%s]: " prompt default)
                             (format "%s: " prompt))
                           nil nil default)))
    (when (and time (not (string-empty-p time)))
      time)))

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

(defun caledonia-add-event ()
  "Add a new event via the server."
  (interactive)
  (let* ((calendars (caledonia--get-available-calendars))
         (calendar (completing-read "Calendar: " calendars nil t))
         (summary (read-string "Summary: "))
         (start-date (caledonia--read-date "Start date"))
         (start-time (caledonia--read-time "Start time (HH:MM)"))
         (end-date (caledonia--read-date "End date"))
         (end-time (caledonia--read-time "End time (HH:MM)"))
         (location (read-string "Location (empty for none): "))
         (description (read-string "Description (empty for none): "))
         (fields `(("calendar" . ,calendar)
                   ("summary" . ,summary)
                   ("start_date" . ,start-date)
                   ("start_time" . ,(when start-time start-time))
                   ("end_date" . ,(when (and end-date (not (string-empty-p end-date))) end-date))
                   ("end_time" . ,(when (and end-time (not (string-empty-p end-time))) end-time))
                   ("location" . ,(when (not (string-empty-p location)) location))
                   ("description" . ,(when (not (string-empty-p description)) description))))
         (request-str (format "(CreateEvent (%s))" (caledonia--build-sexp-fields fields))))
    (caledonia--send-request request-str)
    (message "Event created: %s" summary)
    (when (eq major-mode 'caledonia-mode)
      (caledonia-refresh))))

(defun caledonia-edit-event ()
  "Edit the event at point via the server."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((id-str (tabulated-list-get-id))
           (event (when id-str (get-text-property 0 'event-data id-str))))
      (unless event
        (user-error "No event at point"))
      (let* ((id (caledonia--get-key 'id event))
             (cur-summary (or (caledonia--get-key 'summary event) ""))
             (cur-location (or (caledonia--get-key 'location event) ""))
             (cur-description (or (caledonia--get-key 'description event) ""))
             (summary (read-string (format "Summary [%s]: " cur-summary) nil nil cur-summary))
             (location (read-string (format "Location [%s]: " cur-location) nil nil cur-location))
             (description (read-string (format "Description [%s]: " cur-description) nil nil cur-description))
             (fields `(("id" . ,id)
                       ("summary" . ,(unless (string= summary cur-summary) summary))
                       ("location" . ,(unless (string= location cur-location) location))
                       ("description" . ,(unless (string= description cur-description) description))))
             (request-str (format "(EditEvent (%s))" (caledonia--build-sexp-fields fields))))
        (caledonia--send-request request-str)
        (message "Event updated: %s" summary)
        (caledonia-refresh)))))

(defun caledonia-delete-event ()
  "Delete the event at point via the server."
  (interactive)
  (when (eq major-mode 'caledonia-mode)
    (let* ((id-str (tabulated-list-get-id))
           (event (when id-str (get-text-property 0 'event-data id-str))))
      (unless event
        (user-error "No event at point"))
      (let* ((id (caledonia--get-key 'id event))
             (summary (or (caledonia--get-key 'summary event) "(no summary)")))
        (when (y-or-n-p (format "Delete event '%s'? " summary))
          (caledonia--send-request (format "(DeleteEvent %S)" id))
          (message "Event deleted: %s" summary)
          (caledonia-refresh))))))

;; Modes
;;;###autoload

;; Create a filter prefix map for query refinement
(defvar caledonia-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'caledonia-query-date-range)
    (define-key map (kbd "c") 'caledonia-query-calendars)
    (define-key map (kbd "t") 'caledonia-query-text)
    (define-key map (kbd "i") 'caledonia-query-id)
    (define-key map (kbd "r") 'caledonia-query-recurring)
    (define-key map (kbd "l") 'caledonia-query-limit)
    (define-key map (kbd "z") 'caledonia-query-timezone)
    map)
  "Keymap for filter commands in Caledonia mode.")

(defvar caledonia-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'caledonia-show-event)
    (define-key map (kbd "M-RET") 'caledonia-open-event-file)
    (define-key map (kbd "l") 'caledonia-list)
    (define-key map (kbd "s") 'caledonia-search)
    (define-key map (kbd "r") 'caledonia-refresh)
    (define-key map (kbd "a") 'caledonia-add-event)
    (define-key map (kbd "e") 'caledonia-edit-event)
    (define-key map (kbd "d") 'caledonia-delete-event)
    (define-key map (kbd "A") 'caledonia-agenda)
    (define-key map (kbd "q") 'quit-window)
    ;; Individual filter command bindings
    (define-key map (kbd "C-c d") 'caledonia-query-date-range)
    (define-key map (kbd "C-c c") 'caledonia-query-calendars)
    (define-key map (kbd "C-c t") 'caledonia-query-text)
    (define-key map (kbd "C-c i") 'caledonia-query-id)
    (define-key map (kbd "C-c r") 'caledonia-query-recurring)
    (define-key map (kbd "C-c l") 'caledonia-query-limit)
    (define-key map (kbd "C-c z") 'caledonia-query-timezone)
    ;; Use f prefix for filter commands
    (define-key map (kbd "C-c f") caledonia-filter-map)
    map)
  "Keymap for Caledonia mode.")

(define-derived-mode caledonia-mode tabulated-list-mode "Caledonia"
  "Major mode for displaying calendar entries in a tabular view.")

;; Agenda view

(defvar caledonia--agenda-buffer "*Caledonia Agenda*"
  "Buffer name for the agenda view.")

(defface caledonia-agenda-date-face
  '((t :inherit org-agenda-date :weight bold))
  "Face used for date headers in the agenda view."
  :group 'caledonia)

(defface caledonia-agenda-time-face
  '((t :inherit font-lock-string-face))
  "Face used for times in the agenda view."
  :group 'caledonia)

(defvar caledonia-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'caledonia-agenda-show-event)
    (define-key map (kbd "M-RET") 'caledonia-agenda-open-event-file)
    (define-key map (kbd "l") 'caledonia-list)
    (define-key map (kbd "r") 'caledonia-agenda-refresh)
    (define-key map (kbd "a") 'caledonia-add-event)
    (define-key map (kbd "d") 'caledonia-agenda-delete-event)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for Caledonia agenda mode.")

(define-derived-mode caledonia-agenda-mode special-mode "Caledonia-Agenda"
  "Major mode for displaying calendar events in an agenda view.")

(defvar-local caledonia-agenda--query nil
  "Current query used by this agenda buffer.")

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

(defun caledonia--render-agenda (events)
  "Render EVENTS in agenda format, grouped by date."
  (let ((day-groups (make-hash-table :test 'equal))
        (day-order nil))
    ;; Group events by date
    (dolist (event events)
      (let* ((start (caledonia--get-key 'start event))
             (parsed (caledonia--parse-iso-date start))
             (date-key (list (nth 0 parsed) (nth 1 parsed) (nth 2 parsed))))
        (unless (gethash date-key day-groups)
          (push date-key day-order))
        (puthash date-key
                 (append (gethash date-key day-groups) (list event))
                 day-groups)))
    ;; Sort day-order chronologically
    (setq day-order (sort day-order
                          (lambda (a b)
                            (or (< (nth 0 a) (nth 0 b))
                                (and (= (nth 0 a) (nth 0 b))
                                     (or (< (nth 1 a) (nth 1 b))
                                         (and (= (nth 1 a) (nth 1 b))
                                              (< (nth 2 a) (nth 2 b)))))))))
    ;; Render each day
    (dolist (date-key day-order)
      (let ((year (nth 0 date-key))
            (month (nth 1 date-key))
            (day (nth 2 date-key))
            (day-events (gethash date-key day-groups)))
        ;; Insert date header
        (insert (propertize (caledonia--format-day-header year month day)
                            'face 'caledonia-agenda-date-face)
                "\n")
        ;; Insert events for this day
        (dolist (event day-events)
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
            (insert (propertize line 'event-data event))))))))

(defun caledonia-agenda-show-event ()
  "Show details for the event on the current line."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (if event
        (caledonia--display-event-details event)
      (message "No event on this line"))))

(defun caledonia-agenda-open-event-file ()
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

(defun caledonia-agenda-delete-event ()
  "Delete the event on the current line."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (if event
        (let ((id (caledonia--get-key 'id event))
              (summary (or (caledonia--get-key 'summary event) "(no summary)")))
          (when (y-or-n-p (format "Delete event '%s'? " summary))
            (caledonia--send-request (format "(DeleteEvent %S)" id))
            (message "Event deleted: %s" summary)
            (caledonia-agenda-refresh)))
      (message "No event on this line"))))

(defun caledonia-agenda-refresh ()
  "Refresh the agenda view."
  (interactive)
  (when (eq major-mode 'caledonia-agenda-mode)
    (caledonia--send-request "Refresh")
    (let* ((query caledonia-agenda--query)
           (request-str (format "(Query %s)" (prin1-to-string query)))
           (payload (caledonia--send-request request-str))
           (events (caledonia--get-events payload)))
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (caledonia--render-agenda events)
        (goto-char (min pos (point-max)))))))

(defun caledonia-agenda (&optional from-date to-date)
  "Show an org-agenda style view of calendar events.
FROM-DATE and TO-DATE override defaults. With prefix arg, prompts for dates."
  (interactive
   (when current-prefix-arg
     (let ((dates (caledonia--read-date-range)))
       (list (car dates) (cdr dates)))))
  (let* ((from (or from-date caledonia-list-from-date))
         (to (or (and to-date (not (string-empty-p to-date)) to-date)
                 caledonia-list-to-date))
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
          (caledonia--render-agenda events))
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

(provide 'caledonia)
;;; caledonia.el ends here
