;;; varuga.el --- Send ical calendar invites by email
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of varuga.el.
;;;
;;; varuga.el is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; varuga.el is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with varuga.el.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (emacs-varuga-package)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public emacs-varuga
  (package
    (name "emacs-varuga")
    (version "0.1.0")
    (source (local-file ".."
                        "emacs-varuga-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (build-system emacs-build-system)
    (arguments
     (list #:tests? #t
           #:test-command #~(list "make" "check")))
    (home-page "https://git.systemreboot.net/varuga")
    (synopsis "Send ical calendar invites by email")
    (description "@code{emacs-varuga} lets you send ical calendar invites using your
Emacs mail client.  These invites are similar to those produced by
Google Calendar, Outlook Calendar, etc. and are compatible with them.")
    (license license:gpl3+)))

emacs-varuga
