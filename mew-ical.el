;;; mew-ical.el --- iCalendar support for Mew

;; Copyright (C) 2007-2008 Akira TAGOH

;; Author: Akira TAGOH  <akira@tagoh.org>
;; Keywords: mew icalendar

;; This file can be distributed under the same terms of Mew.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;; Code:

(require 'browse-url)

(defvar mew-ical-prog-icalendar 'mew-ical-mime-text/calendar)
(defvar mew-ical-version "0.3")
(defvar mew-ical-hash (make-hash-table :weakness 'key))

(defgroup mew-ical nil
  "iCalendar handler for Mew customize group."
  :prefix "mew-ical-"
  :group 'mew)

(defcustom mew-ical-server-uri nil
  "iCal server URI that support iTIP."
  :type 'string
  :group 'mew-ical)
(defcustom mew-ical-home-uri nil
  "iCal server's home URI"
  :type 'string
  :group 'mew-ical)
(defcustom mew-ical-user nil
  "A user name that your iCal server may requires"
  :type 'string
  :group 'mew-ical)
(defcustom mew-ical-pass nil
  "A password that your iCal server may requires. using this variable isn't recommended."
  :type 'string
  :group 'mew-ical)
(defcustom mew-ical-browser #'browse-url
  "Default browser supposed to be used to look at `mew-ical-home-uri'."
  :type '(choice
	  (function-item :tag "Follow to browse-url's default" :value browse-url)
	  (function-item :tag "Firefox" :value browse-url-firefox))
  :group 'mew-ical)

(add-hook 'mew-init-hook
	  '(lambda ()
	     (add-to-list 'mew-mime-content-type
			  '("text/calendar" "\\.ics$" nil mew-ical-prog-icalendar mew-icon-text))))

(define-button-type 'mew-ical-master
  'action #'mew-ical-button-handler)
(define-button-type 'mew-ical-accept
  :supertype 'mew-ical-master
  'status "ACCEPTED")
(define-button-type 'mew-ical-decline
  :supertype 'mew-ical-master
  'status "DECLINED")
(define-button-type 'mew-ical-tentative
  :supertype 'mew-ical-master
  'status "TENTATIVE")
(define-button-type 'mew-ical-cancel
  :supertype 'mew-ical-master
  'status "CANCEL")
(define-button-type 'mew-ical-open-calendar
  'action #'mew-ical-button-handler-open-calendar)

(defun mew-ical-send-ical (organizer summary status file actionfile need-reply extraopts)
  (let* ((opts (append extraopts `("-k"
				   "--data-binary"
				   ,(concat "@" file)
				   ,mew-ical-server-uri)))
	 (process (apply 'start-process
			 "mew-ical"
			 nil
			 "curl"
			 (append opts extraopts))))
    (puthash process
	     (copy-tree (list `(request . ,file)
			      `(reply . ,actionfile)
			      `(organizer . ,organizer)
			      `(summary . ,summary)
			      `(result . t)
			      `(status . ,status)
			      `(need-reply . ,need-reply)
			      `(opts . ,extraopts)))
	     mew-ical-hash)
    (set-process-filter process 'mew-ical-uploading-filter)
    (set-process-sentinel process 'mew-ical-uploading-sentinel)
    (message "Uploading ics file...")))

(defun mew-ical-uploading-filter (process event)
  (let* ((val (gethash process mew-ical-hash))
	 (alist (assoc 'result val)))
    (if (and (eq (cdr alist) t)
	     (string-match ".*permission denied.*" event))
	(setcdr alist 'permission)
      (setcdr alist 'unknown))))

(defun mew-ical-uploading-sentinel (process event)
  (let ((msg "Uploading ics file..."))
    (set-process-sentinel process nil)
    (set-process-filter process nil)
    (if (string= event "finished\n")
	(let* ((alist (gethash process mew-ical-hash))
	       (result (cdr (assoc 'result alist)))
	       (organizer (cdr (assoc 'organizer alist)))
	       (summary (cdr (assoc 'summary alist)))
	       (request (cdr (assoc 'request alist)))
	       (reply (cdr (assoc 'reply alist)))
	       (status (cdr (assoc 'status alist)))
	       (need-reply (cdr (assoc 'need-reply alist)))
	       (opts (cdr (assoc 'opts alist))))
	  (case result
	    ('permission (message (format "%spermission denied." msg))
			 (if (and (not opts)
				  mew-ical-user)
			     (let* ((prompt (format "iCal server password (%s): "
						    mew-ical-user))
				    (passwd (mew-input-passwd prompt nil)))
			       (remhash process mew-ical-hash)
			       (mew-ical-send-ical organizer
						   summary
						   status
						   request
						   reply
						   need-reply
						   (append opts `("--anyauth"
								  "--user"
								  ,(concat mew-ical-user ":" passwd))))
			       ))
			 )
	    ('unknown (message (format "%sfailed due to the unknown result." msg)))
	    (t (message (format "%sdone." msg))
	       (if reply
		   (mew-ical-send-ical organizer summary status reply nil need-reply opts)
		 ;; all the required operation has been done.
		 (if need-reply
		     (when (y-or-n-p "Would you like to send an email to the organizer with your reply? ")
		       (mew-user-agent-compose (mew-ical-print-cal-address organizer)
					       (concat "Reply for " summary)
					       `(("body" . ,(concat "I have " status " your appointment.")))
					       nil
					       nil
					       nil
					       nil)
		       (mew-draft-prepare-attachments)
		       (mew-attach-copy request "calendar.ics"))
		   )))
	    ))
      (message (format "%s%s" msg event)))
    ))

(defun mew-ical-button-handler (button)
  (save-excursion
    (let ((cache (button-get button 'buffer)))
      (set-buffer cache)
      (let* ((substr (buffer-substring (point-min) (point-max)))
	     (recurrence (if (string-match "\\(RECURRENCE-ID.*\\)" substr)
			     (concat (match-string 1 substr) "\n")
			   ""))
	     (uid (if (string-match "\\(UID:.*\\)" substr)
		      (concat (match-string 1 substr) "\n")
		    ""))
	     (rfile (mew-make-temp-name))
	     (afile (if (not (string= (button-get button 'status) "CANCEL"))
			(mew-make-temp-name)))
	     (buf (generate-new-buffer " *Mew iCal Response*"))
	     (need-reply (not afile))
	     (aalist (button-get button 'attendance))
	     (oalist (button-get button 'organizer))
	     (summary (button-get button 'summary))
	     (status (button-get button 'status))
	     (begin (button-get button 'begin))
	     (end (button-get button 'end)))
	(set-buffer buf)
	(if afile
	    (progn
	      (insert "BEGIN:VCALENDAR\n"
		      (format "PRODID:-//Mew//Mew iCal %s//EN\n" mew-ical-version)
		      "VERSION:2.0\n"
		      "METHOD:REPLY\n"
		      "BEGIN:VEVENT\n"
		      (format "ATTENDEE;PARTSTAT=%s:MAILTO:%s\n"
			      status (cadr (assoc 'email aalist)))
		      (format "DTSTAMP:%s\n"
			      (format-time-string "%Y%m%dT%H%M%SZ" (current-time)))
		      (format "ORGANIZER:%s\n"
			      (cadr (assoc 'email oalist)))
		      recurrence
		      uid
		      "SEQUENCE:0\n"
		      "END:VEVENT\n"
		      "END:VCALENDAR\n")
	      (mew-flet
	       (write-region (point-min) (point-max) afile nil 'no-msg))
	      ))
	(kill-buffer buf)
	(save-excursion
	  (set-buffer cache)
	  (mew-flet
	   (write-region begin end rfile nil 'no-msg)))
	(mew-ical-send-ical oalist
			    summary
			    status
			    rfile
			    afile
			    need-reply
			    (if (and mew-ical-user mew-ical-pass)
				`("--anyauth"
				  "--user"
				  ,(concat mew-ical-user ":" mew-ical-pass))
			      nil))
	))))

(defun mew-ical-button-handler-open-calendar (button)
  (if (functionp mew-ical-browser)
      (apply mew-ical-browser '(mew-ical-home-uri))))

(defun mew-ical-get-line ()
  (unless (eobp)
    (let ((line (buffer-substring (line-beginning-position)
				  (line-end-position))))
      (forward-line)
      line)))

(defun mew-ical-normalize ()
  (reverse
   (let (lines line)
     (save-excursion
       (beginning-of-buffer)
       (while (setq line (mew-ical-get-line))
	 (setq lines (if (string-match "^[ \t]+\\(.*\\)" line)
			 (cons (concat (car lines) (match-string 1 line)) (cdr lines))
		       (cons line lines)))
	 ))
     lines)))

(defun mew-ical-parse-date-time (line)
  (cond
   ((string-match "^DTSTART;\\(.*\\):\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(Z?\\)" line)
    `((dtstart . ,`((year . ,(string-to-number (match-string 2 line)))
		    (month . ,(string-to-number (match-string 3 line)))
		    (day . ,(string-to-number (match-string 4 line)))
		    (hour . ,(string-to-number (match-string 5 line)))
		    (minute . ,(string-to-number (match-string 6 line)))
		    (second . ,(string-to-number (match-string 7 line)))
		    (tzstring . ,(match-string 0 line))
		    (utc . ,(if (match-string 8 line) t nil))))))
   ((string-match "^DTSTART:\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(Z?\\)" line)
    `((dtstart . ,`((year . ,(string-to-number (match-string 1 line)))
		    (month . ,(string-to-number (match-string 2 line)))
		    (day . ,(string-to-number (match-string 3 line)))
		    (hour . ,(string-to-number (match-string 4 line)))
		    (minute . ,(string-to-number (match-string 5 line)))
		    (second . ,(string-to-number (match-string 6 line)))
		    (utc . ,(if (match-string 7 line) t nil))))))
   ((string-match "^DTEND;\\(.*\\):\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(Z?\\)" line)
    `((dtend . ,`((year . ,(string-to-number (match-string 2 line)))
		  (month . ,(string-to-number (match-string 3 line)))
		  (day . ,(string-to-number (match-string 4 line)))
		  (hour . ,(string-to-number (match-string 5 line)))
		  (minute . ,(string-to-number (match-string 6 line)))
		  (second . ,(string-to-number (match-string 7 line)))
		  (tzstring . ,(match-string 0 line))
		  (utc . ,(if (match-string 8 line) t nil))))))
   ((string-match "^DTEND:\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\(Z?\\)" line)
    `((dtend . ,`((year . ,(string-to-number (match-string 1 line)))
		  (month . ,(string-to-number (match-string 2 line)))
		  (day . ,(string-to-number (match-string 3 line)))
		  (hour . ,(string-to-number (match-string 4 line)))
		  (minute . ,(string-to-number (match-string 5 line)))
		  (second . ,(string-to-number (match-string 6 line)))
		  (utc . ,(if (match-string 7 line) t nil))))))
   (t
    nil)))

(defun mew-ical-check-address (addr)
  (catch 'check-address
    (dolist (address mew-from-list)
      (let ((email (if (string-match ".*<\\(.*\\)>" address)
		       (match-string 1 address)
		     address)))
	(if (string= email addr)
	    (throw 'check-address t))))
    nil))

(defun mew-ical-parse-cal-address (key ownsym awaysym line)
  (cond
   ((string-match (format "^%s\\(;\\(.*\\)\\)?:\\(MAILTO:\\(.*@.*\\)\\)" key)
		  line)
    (let* ((calparam (match-string 2 line))
	   (caladdr (match-string 4 line))
	   (sym (if (mew-ical-check-address caladdr)
		    ownsym
		  awaysym))
	   (cn (if (and calparam
			(string-match "CN=\\([^;]+\\)\\(;.*\\|$\\)" calparam))
		   (match-string 1 calparam)
		 nil))
	   (status (if (and calparam
			    (string-match "PARTSTAT=\\([^:;]+\\)[:;]?" calparam))
		       (match-string 1 calparam)
		     nil)))
      `((,sym . ,`((email ,caladdr)
		   (param ((cn ,cn)
			   (status ,status)
			   (all ,calparam))))))
      ))
   ((string-match (format "^%s:\\(.*@.*\\)" key) line)
    (let* ((caladdr (match-string 1 line))
	   (sym (if (mew-ical-check-address caladdr)
		    ownsym
		  awaysym)))
      `((,sym . ,`((email ,caladdr)
		   (param nil))))))
   (t
    nil)))

(defun mew-ical-parse-event (line)
  (cond
   ((string-match "^SUMMARY:\\(.*\\)" line)
    `((summary . ,(match-string 1 line))))
   ((string-match "^LOCATION:\\(.*\\)" line)
    `((location . ,(match-string 1 line))))
   ((string-match "^DESCRIPTION:\\(.*\\)" line)
    `((description . ,(match-string 1 line))))
   ((string-match "^RRULE:\\(.*\\)" line)
    `((rrule . ,(match-string 1 line))))
   ((string-match "^DURATION" line)
    nil)
   (t
    (or (mew-ical-parse-cal-address "ORGANIZER"
				    'ownrequest
				    'organizer
				    line)
	(mew-ical-parse-cal-address "ATTENDEE"
				    'myattendance
				    'attendee
				    line)
	(mew-ical-parse-date-time line)))
   ))

(defun mew-ical-parse-timezone (line)
  (cond
   ((string-match "^TZID:\\(.*\\)" line)
    `((tzid . ,(match-string 1 line))))
   ((string-match "^TZOFFSETTO:\\(.*\\)" line)
    `((tzoffsetto . ,(match-string 1 line))))
   ((string-match "^TZOFFSETFROM:\\(.*\\)" line)
    `((tzoffsetfrom . ,(match-string 1 line))))
   (t
    (mew-ical-parse-date-time line))
   ))

(defun mew-ical-append (one two)
  (let* ((key (caar two))
	 (val (cdar two))
	 (map (assoc key one)))
    (if two
	(case key
	  ('attendee (let* ((amap (assoc 'attendees one))
			    (aval (cdr amap)))
		       (if aval
			   (progn
			     (setcdr amap (append aval `(,val)))
			     one)
			 (append one `((attendees . (,val)))))))
	  (t (if map
		 (progn
		   (setcdr map two)
		   one)
	       (append one two))))
      one)))

(defun mew-ical-date-format (alist sym)
  (let ((val (cdr (assoc sym alist))))
    (if val
	(format (case sym
		  ('year "%04d")
		  (t "%02d"))
		val)
      (case sym
	('year "----")
	(t "--")))))

(defun mew-ical-print-cal-address (alist)
  (let* ((email (cadr (assoc 'email alist)))
	 (param (cadr (assoc 'param alist)))
	 (cn (cadr (assoc 'cn param))))
    (if cn
	(format "%s <%s>" cn email)
      email)))

(defun mew-ical-mime-text/calendar (cache begin end &optional params fname ct cte)
  (save-excursion
    (insert-buffer-substring cache begin end)
    (let ((lines (mew-ical-normalize))
	  (mode nil)
	  (event nil)
	  tmp map line method timezone)
      (delete-region (point-min) (point-max))
      (insert "    #     #####     #    #\n"
	      "         #     #   # #   #\n"
	      "    #    #        #   #  #\n"
	      "    #    #       #     # #\n"
	      "    #    #       ####### #\n"
	      "    #    #     # #     # #\n"
	      "    #     #####  #     # #######\n"
	      "\n\n")
      (mapc
       (lambda (line)
	 (case mode
	   ('vtimezone (if (string-match "^END:VTIMEZONE" line)
			   (setq mode nil)
			 (setq timezone (mew-ical-append timezone (mew-ical-parse-timezone line)))))
	   ('vevent (if (string-match "^END:VEVENT" line)
			(setq mode nil)
		      (setq event (mew-ical-append event (mew-ical-parse-event line)))))
	   (t (if (string-match "^BEGIN:VTIMEZONE" line)
		  (setq mode 'vtimezone)
		(if (string-match "^BEGIN:VEVENT" line)
		    (setq mode 'vevent)
		  (if (string-match "^METHOD:\\(.*\\)" line)
		      (setq method (match-string 1 line)))
		  )))))
       lines)
      (if (string= method "REPLY")
	  (progn
	    ;; assume that it has only one attendee field.
	    ;; FIXME: should we check that?
	    (let* ((alist (car (cdr (assoc 'attendees event))))
		   (name (mew-ical-print-cal-address alist))
		   (param (cadr (assoc 'param alist)))
		   (status (cadr (assoc 'status param))))
	      (if status
		  (insert (format "%s has %s your following request:\n\n"
				  name status))
		(insert "[BUG] failed to parse ical.\n")
		(insert "You have gotten an reply as the following:\n\n"))))
	(if (assoc 'ownrequest event)
	    (insert "You are requesting the following appointment.\n\n")
	  (if (string= method "REQUEST")
	      (progn
		(insert "The following appointment are requested.\n"
			"Please take an action:\n\n  ")
		(insert-button "ACCEPT"
			       :type 'mew-ical-accept
			       'buffer cache
			       'begin begin
			       'end end
			       'attendance (cdr (assoc 'myattendance event))
			       'organizer (cdr (assoc 'organizer event))
			       'summary (cdr (assoc 'summary event)))
		(insert "  ")
		(insert-button "DECLINE"
			       :type 'mew-ical-decline
			       'buffer cache
			       'begin begin
			       'end end
			       'attendance (cdr (assoc 'myattendance event))
			       'organizer (cdr (assoc 'organizer event))
			       'summary (cdr (assoc 'summary event)))
		(insert "  ")
		(insert-button "TENTATIVE"
			       :type 'mew-ical-tentative
			       'buffer cache
			       'begin begin
			       'end end
			       'attendance (cdr (assoc 'myattendance event))
			       'organizer (cdr (assoc 'organizer event))
			       'summary (cdr (assoc 'summary event)))
		(insert "  ")
		(insert-button "Open a calendar"
			       :type 'mew-ical-open-calendar)
		(insert "\n\n"))
	    (if (string= method "CANCEL")
		(progn
		  (insert "The following apointment has been canceled.\n\n")
		  (insert-button "SEND"
				 :type 'mew-ical-cancel
				 'buffer cache
				 'begin begin
				 'end end
				 'attendance (cdr (assoc 'myattendance event))
				 'organizer (cdr (assoc 'organizer event))
				 'summary (cdr (assoc 'summary event)))
		  (insert "\n\n"))
	      ))))
      (mew-insert "Subject:\t%s\n" (cdr (assoc 'summary event)))
      (mew-insert "Location:\t%s\n" (cdr (assoc 'location event)))
      (mew-insert "Organizer:\t%s\n"
		  (mew-ical-print-cal-address (or (cdr (assoc 'organizer event))
						  (cdr (assoc 'ownrequest event)))))
      (insert "Attendee(s):\n")
      (let* ((attendees (cdr (assoc 'attendees event))))
	(dolist (alist attendees)
	  (let ((val (cadr (assoc 'email alist))))
	    (if val
		(mew-insert "\t%s\n"
			    (mew-ical-print-cal-address alist))))))
      (insert (format "Starting Date:\t%s/%s/%s %s:%s:%s\n"
		      (mew-ical-date-format (cdr (assoc 'dtstart event)) 'year)
		      (mew-ical-date-format (cdr (assoc 'dtstart event)) 'month)
		      (mew-ical-date-format (cdr (assoc 'dtstart event)) 'day)
		      (mew-ical-date-format (cdr (assoc 'dtstart event)) 'hour)
		      (mew-ical-date-format (cdr (assoc 'dtstart event)) 'minute)
		      (mew-ical-date-format (cdr (assoc 'dtstart event)) 'second)))
      (insert (format "Ending Date:\t%s/%s/%s %s:%s:%s\n"
		      (mew-ical-date-format (cdr (assoc 'dtend event)) 'year)
		      (mew-ical-date-format (cdr (assoc 'dtend event)) 'month)
		      (mew-ical-date-format (cdr (assoc 'dtend event)) 'day)
		      (mew-ical-date-format (cdr (assoc 'dtend event)) 'hour)
		      (mew-ical-date-format (cdr (assoc 'dtend event)) 'minute)
		      (mew-ical-date-format (cdr (assoc 'dtend event)) 'second)))
      (mew-insert "Repeat:\t\t%s\n" (cdr (assoc 'rrule event)))
      (mew-insert "Description:\n%s\n"
		  (replace-regexp-in-string "\\\\n" "\n"
					    (let ((desc (cdr (assoc 'description event))))
					      (if desc
						  desc
						""))))
      )))

(provide 'mew-ical)

;;; mew-ical.el ends here
