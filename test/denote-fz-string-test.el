;;; denote-fz-string-test.el --- Tests for string commands.  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mirko Hernandez

;; Author: Mirko Hernandez <mirkoh@fastmail.com>
;; Maintainer: Mirko Hernandez <mirkoh@fastmail.com>>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://github.com/MirkoHernandez/denote-fz

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(ert-deftest denote-fz-test-folgezettel< ()
  (should (denote-fz-folgezettel< "1" "11"))
  (should (denote-fz-folgezettel< "1" "2"))
  (should (denote-fz-folgezettel< "1a" "11"))
  (should (denote-fz-folgezettel< "1" "1a"))
  (should (denote-fz-folgezettel< "1a" "1b"))
  (should (denote-fz-folgezettel< "5a" "5b"))
  (should (denote-fz-folgezettel< "5ab" "5ac"))
  (should (denote-fz-folgezettel< "5a2" "5b"))
  (should (denote-fz-folgezettel< "3a" "3a1"))
  
  (should-not (denote-fz-folgezettel< "1" "1"))
  (should-not (denote-fz-folgezettel< "11" "11"))
  (should-not (denote-fz-folgezettel< "11a" "11"))
  (should-not (denote-fz-folgezettel< "3a1" "3a"))
  (should-not (denote-fz-folgezettel< "2za" "2a2"))
  (should-not (denote-fz-folgezettel< "9" "8"))
  (should-not (denote-fz-folgezettel< "2aa2" "2a2"))
  (should-not (denote-fz-folgezettel< "2a22d" "2a22c")))


(ert-deftest denote-fz-test-string-increment ()
  (should (equal (denote-fz-string-increment "9") "10"))
  (should (equal (denote-fz-string-increment "5a") "5b"))
  (should (equal (denote-fz-string-increment "1a") "1b"))
  (should (equal (denote-fz-string-increment "5z") "5za"))
  (should (equal (denote-fz-string-increment "5zz") "5zza")))

(ert-deftest denote-fz-test-string-decrement ()
  (should (equal (denote-fz-string-decrement "9") "8"))
  (should (equal (denote-fz-string-decrement "10") "9"))
  (should (equal (denote-fz-string-decrement "11") "10"))
  (should (equal (denote-fz-string-decrement "9b") "9a"))
  (should (equal (denote-fz-string-decrement "2aa") "2a"))
  (should (equal (denote-fz-string-decrement "1a1") "1a0"))
  ;; can't be decremented 
  (should (equal (denote-fz-string-decrement "1a0") "1a0"))
  (should (equal (denote-fz-string-decrement "1a") "1a")))

(ert-deftest denote-fz-test-string-variation ()
  (should (equal (denote-fz-string-variation "1a" 'child) "1a1"))
  (should (equal (denote-fz-string-variation "1a1" 'child) "1a1a"))
  (should (equal (denote-fz-string-variation "1a1" 'parent) "1a"))
  (should (equal (denote-fz-string-variation "1a" 'sibling) "1b"))
  (should (equal (denote-fz-string-variation "1" 'sibling) "2"))
  (should (equal (denote-fz-string-variation "10z" 'flat) "10a"))
  (should (equal (denote-fz-string-variation "10a20" 'flat) "10a1")))

(provide 'denote-fz-string-test)

