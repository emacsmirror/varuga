;;; varuga.el --- Send ical calendar invites by email -*- lexical-binding: t -*-

;; Send ical calendar invites by email
;; Copyright © 2024 Arun Isaac
;; Copyright © 2025 Jake Coble <j@kecoble.com>
;;
;; Author: Arun Isaac <arunisaac@systemreboot.net>
;; Version: 0.1.0
;; Homepage: https://git.systemreboot.net/varuga
;; Package-Requires: ((emacs "26.1"))

;; This file is part of varuga.

;; varuga is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; varuga is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with varuga.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Send ical calendar invites using your Emacs mail client.  These
;; invites are similar to those produced by Google Calendar, Outlook
;; Calendar, etc. and are compatible with them.
;;
;; varuga populates a message mode buffer with an ical MIME part
;; (using MML, the MIME Meta Language).  It also adds a plain text
;; part listing the time of the event in various configured timezones.
;;
;; All dates and times you enter into varuga are in your local
;; timezone. varuga automatically converts these into a set of
;; configured timezones (specified in `varuga-clock-list`) for your
;; correspondents' benefit.
;;
;; 🙏 வருக வருக! (varuga varuga!) is a Tamil expression that is used
;; to warmly welcome people to an event.

;;; Code:

(require 'cl-lib)
(require 'message)
(require 'org)
(require 'org-duration)
(require 'org-id)
(require 'time)

(cl-defstruct (varuga-calendar (:constructor varuga-calendar)
                               (:copier nil))
  components)

(cl-defstruct (varuga-calendar-event (:constructor varuga-calendar-event)
                                     (:copier nil))
  organizer
  organizer-email-address
  time-start
  time-end
  summary
  location)

(defvar varuga-product-identifier
  "-//systemreboot//varuga//NONSGML v1.0//EN")

(defvar varuga-clock-list
  (if (listp world-clock-list)
      world-clock-list
    zoneinfo-style-world-list))

(defvar varuga-time-format
  world-clock-time-format)

(defun varuga-insert-calendar-line (key value)
  "Insert ical calendar line.
KEY is the name of the ical property and VALUE is its value."
  ;; Limit content line length to 75 octets as required by RFC 5545.
  (let ((maximum-octets-per-line 75)
        (octets-so-far 0))
    (seq-do (lambda (c)
              (let* (;; Escape the linefeed character.
                     (str (if (eql c ?\n)
                              "\\N"
                            (string c)))
                     (next-octets (+ octets-so-far
                                     (string-bytes str))))
                (if (< next-octets maximum-octets-per-line)
                    (setq octets-so-far next-octets)
                  (insert "\n ")
                  ;; Add an extra 1 to octets so far to account for
                  ;; the folding space.
                  (setq octets-so-far (1+ (string-bytes str))))
                (insert str)))
            (format "%s:%s"
                    (upcase (symbol-name key))
                    value)))
  (insert "\n"))

(defun varuga-format-time-string (time)
  "Format TIME to ical specification."
  (format-time-string "%Y%m%dT%H%M%SZ" time t))

(defun varuga-insert-calendar-event (event)
  "Insert ical calendar EVENT at point."
  (varuga-insert-calendar-line 'begin "VEVENT")
  (varuga-insert-calendar-line 'uid (org-id-uuid))
  (varuga-insert-calendar-line 'dtstamp
                               (varuga-format-time-string (current-time)))
  (varuga-insert-calendar-line 'organizer
                               (format "CN=%s:MAILTO:%s"
                                       (varuga-calendar-event-organizer event)
                                       (varuga-calendar-event-organizer-email-address event)))
  (varuga-insert-calendar-line 'dtstart
                               (varuga-format-time-string
                                (varuga-calendar-event-time-start event)))
  (varuga-insert-calendar-line 'dtend
                               (varuga-format-time-string
                                (varuga-calendar-event-time-end event)))
  (varuga-insert-calendar-line 'summary
                               (varuga-calendar-event-summary event))
  (varuga-insert-calendar-line 'location
                               (varuga-calendar-event-location event))
  (varuga-insert-calendar-line 'end "VEVENT"))

(defun varuga-insert-calendar (calendar)
  "Insert ical CALENDAR at point."
  (varuga-insert-calendar-line 'begin "VCALENDAR")
  (varuga-insert-calendar-line 'version "2.0")
  (varuga-insert-calendar-line 'prodid varuga-product-identifier)
  (seq-do #'varuga-insert-calendar-event
          (varuga-calendar-components calendar))
  (varuga-insert-calendar-line 'end "VCALENDAR"))

(defun varuga-message-subject ()
  "Return subject of message."
  (or (message-fetch-field "Subject")
      ""))

(defun varuga-invite (summary location when duration)
  "Insert calendar invitation into current email message buffer.
SUMMARY is a short description of the event.  LOCATION is the
location of the event (typically a URI for online meetings).
WHEN is the encoded time when the event is scheduled.  DURATION
is the length of the event in minutes."
  (interactive (list (read-string "Event Summary: "
                                  (string-trim
                                   (string-remove-prefix
                                    "Re:" (varuga-message-subject))))
                     (read-string "Location: ")
                     (org-read-date t t nil "When?")
                     (org-duration-to-minutes
                      (read-string "Duration: " "1h"))))
  (save-excursion
    ;; Fill Subject header if it is blank.
    (when (string-blank-p (varuga-message-subject))
      (save-restriction
        (message-narrow-to-headers)
        (re-search-forward "^Subject:")
        (message-narrow-to-field)
        (end-of-line)
        ;; TODO: Allow customization of the Subject format.
        (insert (format "Invitation: %s" summary))))
    ;; Fill email body.
    (goto-char (point-max))
    (insert "<#multipart type=mixed>\n")
    ;; Add human-readable time in configured timezones. TODO: Allow
    ;; customization of this format.
    (insert "<#part type=text/plain>\n\n")
    (seq-do (pcase-lambda (`(,zone ,place))
              (insert place)
              (insert " — ")
              (insert (let ((system-time-locale "C"))
                        (format-time-string varuga-time-format when zone)))
              (insert "\n"))
            varuga-clock-list)
    ;; Insert ical part.
    (insert "<#part type=text/calendar>\n")
    (varuga-insert-calendar
     (varuga-calendar
      :components (list (pcase (mail-extract-address-components
                                (message-fetch-field "From"))
                          (`(,organizer ,organizer-email-address)
                           (varuga-calendar-event
                            :organizer organizer
                            :organizer-email-address organizer-email-address
                            :time-start when
                            :time-end (time-add when
                                                (* 60 (org-duration-to-minutes duration)))
                            :summary summary
                            :location location))))))
    (insert "<#/multipart>\n")))

(provide 'varuga)

;;; varuga.el ends here
