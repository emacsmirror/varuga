## varuga.el --- Send ical calendar invites by email

## Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>

## This file is part of varuga.

## varuga is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## varuga is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with varuga.  If not, see <http://www.gnu.org/licenses/>.

EMACS = emacs

.PHONY: build check clean
build:
	$(EMACS) -Q --batch -L . --eval="(progn (setq byte-compile-debug t) (byte-recompile-directory \".\" 0))"

check:
	$(EMACS) -Q --batch -L . -l varuga-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -vf *.elc
