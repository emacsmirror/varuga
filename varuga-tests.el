;;; varuga.el --- Send ical calendar invites by email -*- lexical-binding: t -*-

;; Send ical calendar invites by email
;; Copyright © 2024 Arun Isaac
;; Copyright © 2025 Jake Coble <j@kecoble.com>
;;
;; Author: Arun Isaac <arunisaac@systemreboot.net>
;; Homepage: https://git.systemreboot.net/varuga

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
;; Unit testing for varuga.el

;;; Code:

(require 'varuga)

(ert-deftest fold-long-line ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo (make-string 100 ?a))
            (count-lines (point-min) (point-max)))
          2)))

(ert-deftest actual-line-feed-in-data ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo "foo\nbar")
            (buffer-string))
          "FOO:foo\\Nbar\n")))

(ert-deftest line-limit-includes-properties ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo (make-string 72 ?a))
            (count-lines (point-min) (point-max)))
          2)))

(ert-deftest line-limit-includes-folding-space ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo (make-string (+ 72 75) ?a))
            (count-lines (point-min) (point-max)))
          3)))

(ert-deftest line-limit-is-octets-not-characters-escaped-line-feed ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo (make-string 36 ?\n))
            (count-lines (point-min) (point-max)))
          2)))

(ert-deftest line-limit-is-octets-not-characters-multibyte-characters ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo (make-string 24 ?அ))
            (count-lines (point-min) (point-max)))
          2)))

(ert-deftest line-limit-does-not-eat-characters ()
  (should
   (equal (with-temp-buffer
            (varuga-insert-calendar-line 'foo (make-string 100 ?a))
            (count-matches "a" (point-min) (point-max)))
          100)))
