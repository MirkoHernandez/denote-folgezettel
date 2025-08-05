;;; denote-fz.el --- Provides folgezettel commands to denote  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mirko Hernandez

;; Author: Mirko Hernandez <mirkoh@fastmail.com>
;; Maintainer: Mirko Hernandez <mirkoh@fastmail.com>>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.5.0
;; Keywords: denote notes zettelkasten folgezettel
;; URL: https://github.com/MirkoHernandez/denote-fz
;; Package-Requires: ((emacs "27.1") (denote "2.0.0"))

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
;; This  package  provides  functions  for  creating  and  navigating
;; folgezettel notes.

;;; Code:
(require 'denote)

;; NOTE:denote-rename-buffer is included in denote 4.0.0
(require 'denote-rename-buffer nil :noerror)

(define-obsolete-function-alias
  'denote-fz-first-note
  'denote-fz-new
  "0.2.0")

(defvar denote-fz-create-function 'denote-fz-create
  "Function used to create notes with a a folgezettel signature.
The function  must create a  denote note and  it should accept  a single
SIGNATURE parameter." )

(defvar denote-fz-dired-function 'denote-fz-dired-sorted
  "Function used to create a Dired buffer with notes sorted by folgezettel." )

(defvar denote-fz-commands
  '(denote-fz-create)
  "List of commands that denote-fz can use to create notes.")

(defvar denote-fz-dired-dirs-table (make-hash-table :test 'equal)
  "Table of directories in which a denote-fz Dired command has been used.
It can be used to describe active zettelkasten directories.")

(defvar denote-fz-nested-notes nil 
  "Non-nil means that the navigation and searching commands will start looking from `denote-directory'.")

;;;; Constants
;; TODO: make obsolete
(defconst denote-fz-sort-command
  " | sed  's/--/=@/' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "
  "String used in find when calling `shell-command-to-string' command.
This enables the correct sorting of the Luhmann id according to the zettelkasten convention.")

;; TODO: make obsolete
(defconst denote-fz-sort-ls-option
  " | sed  's/--/=@/3' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "
  "String for setting `ls-option' for `find-dired' command.")


(defconst denote-fz-top-level-regex
  "[0-9]+--.*"
  "Regexp used to describe top level notes.")

;;;; Helpers - Sort
;; NOTE:  This method  of  sorting  avoids using  regexp  so that  the
;; algorithm  can be  ported to  other languages,  possibly without  a
;; regexp library. And also to try out emacs lisp primitives.
(defun denote-fz-folgezettel<  (a b)
  "Return non-nil if string A has a lower folgezettel value than string B."
  (let* ((len-a (length a))
	 (len-b (length b))
	 (i 0)
	 (shortest (min  len-b len-a))
	 (both-numbers t)
	 (result))
    (while (< i  shortest)
      (let ((char-a (aref a i))
	    (char-b (aref b i)))
	;; both numbers
	(cond ((and (< char-a 58)
		    (< char-b 58))
	       ;; check if previous values were letters.
	       (cond ((and (not both-numbers )
			   (not (equal result "=")))
		      (setq i shortest)
		      result)
		     ;; equal
		     (t (if (= char-a char-b)
			    (unless (equal result "=")
			      (when (= i 0)
				(setq result "=")))
			  (if (equal result "=")
			      (setq result (< char-a char-b))
			    (when (or (= i 0) (not both-numbers))
			      (if (or (equal result "=") (not (= i 0)))
				  result
				(setq result (< char-a char-b))))))))
	       (setq both-numbers t))
	      ;; only a is a number
	      ((< char-a 58)
	       (when both-numbers
		 (if (= i 0)
		     (setq result t)
		   (setq result nil))
		 (setq i shortest))
	       (setq both-numbers nil))
	      ;; only b is a number
	      ((< char-b 58)
	       (when both-numbers
		 (if (= i 0)
		     (setq result nil)
		   (setq result t))
		 (setq i shortest))
	       (setq both-numbers nil))
	      ;; both letters
	      (t
	       (if (or (equal result "=" ))
		   (unless (= char-a char-b)
		     (setq result (< char-a char-b)))
		 (setq i shortest))
	       (setq both-numbers nil))))
      ;; determine which longer string has more digits
      (when (= i (1- shortest))
	(if both-numbers
	    (cond ((< len-a len-b)
		   (when (or (< (aref b (1+ i )) 58)
			     (equal result "="))
		     (setq result t)))
		  ((and (< len-b len-a)
			(< (aref a (1+ i )) 58))
		   (setq result nil)))
	  (if (equal result "=")
	      (setq result (< len-a len-b)))))
      (setq i (1+ i)))
    (if (equal  result "=")
	nil
      result)))

(defun denote-fz-note< (a b)
  "Return non-nil if note A has a lower folgezettel value in the signature than note B."
  (if (file-directory-p a)
      t
    (denote-fz-folgezettel< (denote-retrieve-filename-signature a)
			    (denote-retrieve-filename-signature b))))

;;;; Helpers - Strings
(defun denote-fz-trim-chars (str)
  "Trim letters from STR, from the right side."
  (string-trim str nil "[a-z]+"))

(defun denote-fz-trim-numbers (str)
  "Trim numbers from STR, from the right side."
  (string-trim str nil "[0-9]+"))

(defun denote-fz-base (str)
  "Return the base number of folgezettel STR."
  (string-match "[0-9]+" str)
  (when (not (string-empty-p str ))
    (match-string 0 str)))

(defun denote-fz-get-last-char (str)
 "Return the last char of STR."
  (substring str  -1 nil))


;; NOTE: this is just for reducing  the repetition of very similar let
;; expressions.
(defun denote-fz-split-last (str)
  "Split STR and return 3 values: string-without-last-char, last-char, is-last-char-number."
  (cl-values (substring str 0 -1 )
	     (denote-fz-get-last-char str)
	     (string-match "[0-9]" (denote-fz-get-last-char str))))

;; NOTE: full-section is the list of notes including the selected note
;; and all its descendants, section includes only the immediate descendants.
(defun denote-fz-create-regex-string (id variation)
  "Create  a  regex   based  on  ID.
VARIATION can be base (initial number), parent, children, section
or full-section. Used by `denote-fz-execute-find-command' to find
related notes."
  (cl-multiple-value-bind (string-without-last-char last-char last-char-is-num)
      (denote-fz-split-last id)
    (cl-case variation
      (base  (concat (denote-fz-base id)
		     "[^0-9-]+.*"))
      (parent  (if last-char-is-num
		   (concat (denote-fz-trim-numbers id) "[^a-z-]-")
		 (concat id "[^0-9-]-")))
      (children  (if last-char-is-num
		     (concat id "[^0-9-]+--")
		   (concat id "[^a-z-]+--")))
      (full-section  (if last-char-is-num
			 (concat id "[^0-9-]*[^a-z]*.*")
		       (concat id "[^a-z-]*[^0-9]*.*")))
      (section  (if last-char-is-num
		    (concat id "[^0-9-]*--.*")
		  (concat id "[^a-z-]*--.*"))))))

;;;; Luhmann Id manipulation
(defun denote-fz-string-increment (str)
  "Increment the ascii value of the last character of STR.
Used for creating a Luhmann id."
  (cl-multiple-value-bind (string-without-last-char last-char)
      (denote-fz-split-last str)
    ;; Increment string that ends in 9.
    (if (equal last-char "z")
	(concat str "a")
      (if  (equal last-char "9")
	  (cond ((equal string-without-last-char "")
		 "10")
		((string-match "[0-9]" (denote-fz-get-last-char string-without-last-char))
		 (concat (denote-fz-string-increment string-without-last-char) "0" ))
		;; antecedent char is a letter, incrementing 9 to 10.
		(t (concat  string-without-last-char  "10")))
	(concat string-without-last-char (char-to-string (1+ (string-to-char last-char))))))))

(defun denote-fz-string-decrement (str)
  "Decrement the ascii value of the last character of STR.
Used for creating a Luhmann id."
  (cl-multiple-value-bind (string-without-last-char last-char)
      (denote-fz-split-last str)
    ;; Decrement string that ends in a.
    (if (equal str "")
	""
      (if (equal last-char "a")
	  (if (string-match "[0-9]" (denote-fz-get-last-char string-without-last-char))
	      str
	    string-without-last-char)
	;; Decrement string that ends in 0.
	(if  (equal last-char "0")
	    (cond ((equal string-without-last-char "") ;; 0 can't be decremented, just return 0.
		   "0")
		  ((equal string-without-last-char "1")  ;; decrement 10 to 9, not 09.
		   "9")
		  ((string-match "[0-9]" (denote-fz-get-last-char string-without-last-char))
		   (concat (denote-fz-string-decrement string-without-last-char) "9" ))
		  (t (concat  string-without-last-char  "0")))
	  (concat string-without-last-char (char-to-string (1- (string-to-char last-char)))))))))

;; NOTE: This function manages almost all id manipulations.
(defun denote-fz-string-variation (str variation)
  "Return a variation of the Luhmann id specified in STR.
VARIATION indicates how to modify the id."
  (when (and str (not (string-empty-p str)))
    (cl-multiple-value-bind (string-without-last-char last-char last-char-is-num)
	(denote-fz-split-last str)
      (let ((result (cl-case variation
		      (parent (if last-char-is-num
				  (denote-fz-trim-numbers str)
				string-without-last-char))
		      (decrement (denote-fz-string-decrement str))
		      (sibling (denote-fz-string-increment str))
		      (increment (denote-fz-string-increment str))
		      (child (if last-char-is-num
				 (concat str "a")
			       (concat str "1")))
		      (zero (if last-char-is-num
				""
			       (concat str "0")))
		      (flat (if last-char-is-num
				(concat (denote-fz-trim-numbers str) "1")
			      (concat (denote-fz-trim-chars str) "a"))))))
	(if (and result (not (string-empty-p result)))
	    result
	  (if (string-empty-p result)
	      ;; empty result, return nil (no variation found).
	      nil
	    ;; no result, variation parameter is nil, return the same  str.
	    (when (stringp str)
	      str)))))))

;;; Helpers - Find Files
;; Functions that find the corresponding  denote files by using the signature
;; or a regex as input.
;; TODO: make obsolete
(defun denote-fz-execute-find-command (regex)
  "Execute the find command, REGEX is concatenated with == and -
(the enclosing characters of the signature).
Return string."
  (shell-command-to-string
   (concat "find * -regex " "'\..*==" regex ".*'" denote-fz-sort-command)))

(defun denote-fz-find-sorted-files (regex &optional no-sort)
  "Find a list  of notes matching REGEX.
The list  is sorted by folgezettel unless NO-SORT is non-nil."
  (require 'find-lisp)
  (let* ((dir (if denote-fz-nested-notes denote-directory default-directory))
	 (files (if denote-fz-nested-notes
		    (denote-directory-files (concat    ".*==" (or regex "")  ".*") )
		  (directory-files dir nil (concat    ".*==" (or regex "")  ".*")))))
    ;; (mapcar 'expand-file-name (directory-files-recursively  dir (concat     ".*==" (or regex "")  ".*")))
    (if no-sort
	files
      (sort files 'denote-fz-note<))))

(defun denote-fz-find-sorted-files-dired (regex &optional no-sort)
  (require 'find-lisp)
  (let* ((dir default-directory)
	 (files (directory-files dir nil)))
    (if no-sort
	files
      (sort files 'denote-fz-note<))))

(defun denote-fz-search-files (id &optional variation)
  "Execute the find command to find files matching the Luhmann ID.
VARIATION is a variation of ID.
Return string."
  (cl-case variation
    ;; prefix variation returns all the files that have id as prefix.
    (base (denote-fz-find-sorted-files  (denote-fz-create-regex-string id 'base)))
    (prefix (denote-fz-find-sorted-files (concat id "--")))
    (children (denote-fz-find-sorted-files
	       (denote-fz-create-regex-string id 'children)))
    ;; like children but it includes the parent note.
    (section (denote-fz-find-sorted-files
	      (denote-fz-create-regex-string id 'section)))
    (siblings
     (if (denote-fz-string-variation id 'parent)
	 (denote-fz-search-files (denote-fz-string-variation id 'parent) 'children))
     (denote-fz-find-sorted-files
      denote-fz-top-level-regex))
    (t
     (denote-fz-find-sorted-files id))))

(defun denote-fz-search-note (id &optional variation)
  "Search for note that matches ID and its VARIATION."
  (when id
    (let* ((notes  (denote-fz-search-files id variation))
	   (note  (and notes (car notes))))
      (if (string-empty-p note)
	  nil
	note))))

(defun denote-fz-find-valid-signature (signature)
  "Find if SIGNATURE is valid signature for note creation.
Keep incrementing the signature until a valid one is found."
  (if (not (denote-fz-search-note signature))
	signature
      (denote-fz-find-valid-signature (denote-fz-string-increment  signature))))

(defun denote-fz-find-last-signature-at-level (file-or-signature)
  "Find the last signature at the level of FILE-OR-SIGNATURE.
Signature 20a1 might find 20a14 as the last signature"
  (let* ((signature (if (denote-file-is-note-p file-or-signature )
			(denote-retrieve-filename-signature file-or-signature)
		      file-or-signature))
	 (note (denote-fz-search-note signature)))
    (if (not note)
	nil
      (let* ((next-note-signature (denote-fz-string-increment signature))
	     (next-note
	      (denote-fz-search-note
	       next-note-signature)))
	(if (not next-note)
	    signature
	  (denote-fz-find-last-signature-at-level (denote-fz-string-increment signature)))))))

(defun denote-fz-find-last-signature-nested (file-or-signature)
  "Find the last signature at the level of FILE-OR-SIGNATURE.
Signature 20a1 might find 20a1y as the last nested signature."
  (when file-or-signature
    (let* ((signature (if (denote-file-is-note-p file-or-signature )
			  (denote-retrieve-filename-signature file-or-signature)
			file-or-signature))
	   (note (denote-fz-search-note signature))
	   (child-signature (denote-fz-string-variation signature 'child))
	   (child (denote-fz-search-note child-signature )))
      (if (not note)
	  nil
	(if (not child)
	    signature
	  (let* ((last-child-signature (denote-fz-find-last-signature-at-level child))
		 (last-child
		  (denote-fz-search-note
		   last-child-signature )))
	    (if (not last-child)
		child-signature
	      (denote-fz-find-last-signature-nested last-child-signature))))))))

;;;; Denote defuns
(defun denote-fz-find-note ()
  "Visit a note using from a pretty printed list of candidates."
  (interactive)
  (let* ((vertico-sort-function 'identity));; Prevents sorting by history
    (call-interactively 'denote-fz-find-file)))

(defun denote-fz-find-note-in-full-section ()
 "Find a note in the full section of the current file's signature."
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (result (denote-fz-find-file signature 'base)))
	 (when result
	   (find-file result))))

(defun denote-fz-visit-by-signature (signature)
  "Visit note that has SIGNATURE."
  (let ((note  (denote-fz-search-note
		signature) ))
    (when note
      (find-file note))))

;;;; Zettel creation
(defun denote-fz-retrieve-ids (files)
  "Return a list of signatures corresponding to the list FILES."
  (mapcar #'denote-retrieve-filename-signature files))

(cl-defun denote-fz-custom (&key title keywords file subdirectory date template signature)
  "Helper function to facilitate the creation of notes with signatures."
  (interactive)
  (funcall-interactively 'denote
			 (or title (denote-title-prompt))
			 (or keywords
			     (and (member 'keywords denote-prompts)
				  (denote-keywords-prompt)))
			 ;; this is the filetype value
			 (and file (denote-filetype-heuristics file))
			 (and subdirectory (denote-subdirectory-prompt))
			 (and date (denote-date-prompt))
			 (and template (denote-template-prompt))
			 (or signature nil)))

(defun denote-fz-create (signature)
  "This is the default `denote-fz-create-function'.
It creates a note using  SIGNATURE, prompts are used depending on
the value of `denote-prompts'"
  (let ((denote-user-enforced-denote-directory default-directory)
	(denote-directory default-directory))
    (denote (and (member 'title denote-prompts)
		 (denote-title-prompt nil (format "[%s] New file Title" signature)))
	    (and (member 'keywords denote-prompts)
		 (denote-keywords-prompt))
	    ;; this is the filetype value
	    (and (member 'file-type denote-prompts)
		 (denote-file-type-prompt))
	    (and (member 'subdirectory denote-prompts)
		 (denote-subdirectory-prompt))
	    (and (member 'date denote-prompts)
		 (denote-date-prompt))
	    (and (member 'template denote-prompts)
		 (denote-template-prompt))
	    signature)))

(defun denote-fz-select-command ()
  "Select a `denote-fz-create-function' from the list `denote-fz-commands'."
  (interactive)
  (setq denote-fz-create-function
	(intern
	 (completing-read
	  "Command: " denote-fz-commands ))))

(defun denote-fz-create-note (file-or-signature &optional no-auto-increment)
  "Create a  note using SIGNATURE.
If the note already exists  keep incrementing the signature until
it finds a  valid one for note creation.  If NO-AUTO-INCREMENT is
non-nil the signature will not be incremented."
  (if (not (denote-fz-search-note file-or-signature 'prefix))
      (funcall denote-fz-create-function file-or-signature)
    (if (not no-auto-increment)
	(if (equal file-or-signature "unnumbered")
	    (funcall denote-fz-create-function "unnumbered")
	  (denote-fz-create-note (denote-fz-string-increment file-or-signature)))
      (if (equal file-or-signature "unnumbered")
	  (funcall denote-fz-create-function file-or-signature)
	(message "Signature %s already exists" (propertize file-or-signature 'face  'font-lock-warning-face))))))

(defun denote-fz-derived-signature (&optional variation file-or-signature)
  "Retrieve the current buffer's signature and create a variation.
VARIATION    specifies    how    to   modify    the    signature,
FILE-OR-SIGNATURE  a file  or  signature to  use  instead of  the
current buffer. Return string."
  (when-let* ((file-or-signature (or file-or-signature (buffer-file-name)))
	      (signature (or (denote-retrieve-filename-signature file-or-signature)
			     file-or-signature)))
    (if (equal signature "unnumbered")
	"unnumbered"
      (if signature
	  (denote-fz-string-variation signature variation)
	(message  "No signature found for %s" file-or-signature)
	nil))))

(defun denote-fz-new()
  "Create a new top level note (the  folgezettel will be a number).
If there are no top level notes, it creates the first note, using \"1\"
as the signature."
  (interactive)
  (denote-fz-create-note "1"))

(defun denote-fz-unnumbered ()
 "Create an unnumbered note."
  (interactive)
  (denote-fz-create-note "unnumbered" t))

(defun denote-fz-unnumbered-cycle ()
  "Cycle between unnumbered notes."
  (interactive)
  (let* ((unnumbered-notes (denote-fz-search-files "unnumbered"))
	 (unnumbered-array  (cl-map 'vector 'identity  unnumbered-notes))
	 (array-length  (length unnumbered-array))
	 (current-position (cl-position  (file-name-nondirectory (buffer-file-name)) unnumbered-array :test 'equal))
	 (new-position (if (= (1+ current-position) array-length)
			   0
			 (1+ current-position))))
    (find-file
     (aref unnumbered-array new-position))))

(defun denote-fz-insert-dwim()
"Insert a nested note using the current buffer's signature as the target."
  (interactive)
  (denote-fz-create-note (denote-fz-derived-signature 'child)))

(defun denote-fz-insert-zero()
  "Insert a nested note with signature '0' using the current-buffer's signature as the target."
  (interactive)
  (when-let ((signature (denote-fz-derived-signature 'zero)))
    (denote-fz-create-note)))

(defun denote-fz-insert-at-level-dwim ()
  "Use the current buffer's signature as the target.
Insert a note of the target's signature id incremented by one."
  (interactive)
  (denote-fz-create-note (denote-fz-derived-signature 'sibling)))

(defun denote-fz-insert ()
  "Insert a nested note using the target's signature id."
  (interactive)
  (let ((file  (denote-fz-find-file)))
    (denote-fz-create-note (denote-fz-derived-signature 'child file))))

(defun denote-fz-insert-at-level ()
 "Insert a note using the target's signature id incremented by one unit."
  (interactive)
  (let ((file  (denote-fz-find-file)))
    (denote-fz-create-note (denote-fz-derived-signature 'sibling file))))


;;;; Links
(defun denote-fz-link ()
  (interactive)
  (let* ((file  (denote-fz-find-file))
	 (file-type (denote-filetype-heuristics file))
	 (description (when (file-exists-p file)
			(denote--link-get-description file))))
    (denote-link file file-type description current-prefix-arg )))

;;;; Zettel Editing
(defun denote-fz-add-signature (&optional file variation)
  "Add a signature to  FILE or the current's buffer unnumbered note.
A prompt asks for a target note and VARIATION describes which new
signature is created from the target note."
  (interactive)
  (let* ((file  (or file (dired-get-filename nil t) (buffer-file-name)))
	 (file-type (denote-filetype-heuristics file))
	 (title (denote-retrieve-title-value file file-type))
	 (keywords (denote-retrieve-keywords-value file file-type))
	 (current-signature  (denote-retrieve-filename-signature file))
	 (date (denote-valid-date-p (denote-retrieve-filename-identifier file)))
	 (target (denote-fz-find-file nil nil "Select a note to derive a new signature:"))
	 (signature  (if variation
			 (denote-fz-find-valid-signature (denote-fz-derived-signature variation target))
		       (completing-read "New Signature:"
					(list (denote-fz-find-valid-signature (denote-fz-derived-signature 'child target))
					      (denote-fz-find-valid-signature (denote-fz-derived-signature 'sibling target))
					      )
					nil nil nil t))))
    (if (or (equal "unnumbered" current-signature) (not current-signature))
	(progn
	  (denote-rename-file file title keywords signature date)
	  (when (not (eq major-mode 'dired-mode ))
	    (save-buffer)))
      (message "Not an unnumbered note."))))

(defun denote-fz-add-signature-nested (&optional file)
  "Add a nested signature to FILE or the current buffer's unnumbered note.
A prompt asks for the target note on which to base the signature."
  (interactive)
  (let ((file (or file (dired-get-filename nil t))))
    (denote-fz-add-signature file 'child)))

(defun denote-fz-add-signature-at-level (&optional file)
  "Add a  signature at  level to  FILE or the current  buffer's unnumbered  note.
A prompt asks for the target note on which to base the signature."
  (interactive)
  (let ((file (or file (dired-get-filename nil t))))
    (denote-fz-add-signature file 'sibling)))

(defun denote-fz-add-signature-to-link-nested (&optional file)
  (interactive)
  (if-let ((file (concat
		  (denote-directory)
		  (denote-select-linked-file-prompt
		   (denote-link-return-links)))))
      (denote-fz-add-signature file 'child)
         (user-error "No links found")))

(defun denote-fz-add-signature-to-link-at-level (&optional file)
  (interactive)
  (if-let ((file (concat
		  (denote-directory)
		  (denote-select-linked-file-prompt
		   (denote-link-return-links)))))
      (denote-fz-add-signature file 'sibling)
    (user-error "No links found")))

;;;; Zettel navigation
(defun denote-fz-goto-upper-level ()
  "Visit the upper level note of the current buffer's signature id."
  (interactive)
  (let* ((signature (denote-fz-derived-signature))
	 (parent-signature (denote-fz-derived-signature 'parent))
	 (parent (denote-fz-search-note parent-signature)))
    (if (and parent (not (equal signature "unnumbered")))
	(find-file parent)
      (if-let ((zero (denote-fz-search-note "0")))
	  (find-file zero)
	(message "Note in upper level does not exists.")))))

(defun denote-fz-goto-nested ()
  "Visit a note corresponding with  the current buffer's signature id first nested note."
  (interactive)
  (let* ((child-signature (denote-fz-derived-signature 'child))
	 (child (denote-fz-search-note child-signature)))
    (if child
	(find-file child)
      (message "Nested note does not exists."))))

(defun denote-fz-goto-next ()
  "Visit a note with the current buffer's signature id incremented by one unit."
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'sibling))
	 (sibling (denote-fz-search-note sibling-signature)))
    (if sibling
	(find-file sibling)
      (message "%s" (propertize "Last Note of the sequence." 'face  'font-lock-warning-face)))))

(defun denote-fz-goto-previous ()
 "Visit a note with the current buffer's signature id decremented by one unit."
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'decrement))
	 (first-sibling-signature (denote-fz-derived-signature 'flat))
	 (sibling (denote-fz-search-note sibling-signature)))
    (if sibling
	(progn
	  (find-file sibling)
	  (when (equal sibling-signature first-sibling-signature)
	    (message "%s" (propertize "First Note of the sequence." 'face  'font-lock-warning-face)))))))

(defun denote-fz-cycle ()
  "Visit a note  with the current buffer's  signature id incremented by one unit.
If the end  of the sequence is reached start  from the first note
of the current level."
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'sibling))
	 (sibling (denote-fz-search-note sibling-signature))
	 (first-sibling-signature (denote-fz-derived-signature 'flat)))
    (if (not (string-empty-p sibling))
	(find-file sibling)
      (find-file (denote-fz-search-note first-sibling-signature))
      (message "%s" (propertize "Last Note of the sequence." 'face  'font-lock-warning-face)))))

(defun denote-fz-follow-through (&optional file)
  "Find  the next  contiguous  note. FILE
Prioritize nested notes,  then notes at the same  level, then the
next note in the upper level."
  (interactive)
  (let* ((current-signature (denote-retrieve-filename-signature (or file (buffer-file-name))))
	 (child-signature (denote-fz-derived-signature 'child current-signature))
	 (child (denote-fz-search-note child-signature))
	 (sibling-signature (denote-fz-derived-signature 'sibling current-signature))
	 (sibling (denote-fz-search-note sibling-signature))
	 (parent-signature (denote-fz-derived-signature 'parent current-signature))
	 (parent (denote-fz-search-note  (denote-fz-derived-signature 'parent current-signature)))
	 (parent-last-child (denote-fz-find-last-signature-nested parent-signature))
	 (next-to-parent (denote-fz-search-note
			  (denote-fz-derived-signature 'sibling
						       parent-signature))))
    (cond (child
	   (find-file child))
	  (sibling
	   (find-file sibling))
	  (next-to-parent
	   (find-file next-to-parent))
	  ((equal parent-last-child current-signature)
	   (find-file
	    (denote-fz-search-note
	     (denote-fz-derived-signature 'sibling
					  (denote-fz-derived-signature 'parent parent-signature)))))
	  (parent
	   parent)
	  (t
	   (message "%s" (propertize "Last Note." 'face  'font-lock-warning-face))))))

(defun denote-fz-backward-follow-through (&optional file)
  "Find  the previous  contiguous  note of FILE or the current buffer's signature.
Prioritize nested notes  of the previous note, then  notes at the
same level, then the previous note in the upper level."
  (interactive)
  (let* ((current-signature (denote-retrieve-filename-signature (or file (buffer-file-name))))
	 (previous-signature (denote-fz-derived-signature 'decrement current-signature))
	 (previous (unless (equal current-signature
				  previous-signature)
		     (denote-fz-search-note previous-signature)))
	 (previous-last-child-signature (and previous
					     (denote-fz-find-last-signature-nested previous-signature)))
	 (previous-last-child (denote-fz-search-note previous-last-child-signature))
	 (parent (denote-fz-search-note (denote-fz-derived-signature 'parent current-signature))))
    (cond (previous-last-child
	   (find-file previous-last-child))
	  (previous
	   (find-file previous))
	  (parent
	   (find-file parent))
	  (t
	   (message "%s" (propertize "First Note." 'face  'font-lock-warning-face))))))

;;; Helpers - Dired
;; TODO: make obsolete.
(defun denote-fz-set-find-ls-option (&optional regex)
  "Create  find-ls-option (used  with  find-dired)  using REGEX.
If regex is null  use a regexp that searches for  all the notes that
have a signature."
  (defvar-local find-ls-option nil)
  (let ((find-argument  (concat (if regex
				    (concat "-regex '.*==" regex "'")
				  (concat "-regex '.*==.*" "'"))
				" -exec ls -ld {} \\+| awk /==/" denote-fz-sort-ls-option)))
    `(,find-argument
      .
      "-ld")))

;; TODO: make obsolete.
(defun denote-fz-find-dired (&optional regex)
  (let ((find-ls-option (denote-fz-set-find-ls-option regex))
	(current-find-dired-option find-dired-refine-function))

    ;; NOTE: this needs to be set globally, find-dired works asynchronously.
    (setq find-dired-refine-function nil)

    (find-dired default-directory "")
    (denote-fz-dired-mode)
    (setq find-dired-refine-function current-find-dired-option)))

(defun denote-fz-dired-sorted (&optional regex)
 "Creates  a  sorted Dired  buffer  with  notes corresponding  with REGEX."
 (let* ((denote-fz-nested-notes nil)
	(files (denote-fz-find-sorted-files-dired  regex)))
    (dired (cons default-directory files))
    (denote-fz-dired-mode t)))

(defun denote-fz-set-section (signature)
  (let* ((file (or (dired-get-filename nil t) (buffer-file-name)))
	 (dir (file-name-parent-directory file)))
  (puthash
   dir
   signature
   denote-fz-dired-dirs-table)))

(defun denote-fz-get-section ()
  (let* ((file (or (dired-get-filename nil t) (buffer-file-name)))
	 (dir (file-name-parent-directory file)))
    (gethash
     dir
     denote-fz-dired-dirs-table)))

;;;; Hierarchy Commands
(defun denote-fz-build-hierarchy (&optional file no-parentfn)
  (let* ((parentfn #'denote-fz-hierarchy-parent)
	 (childrenfn #'denote-fz-hierarchy-children)
	 (hierarchy (hierarchy-new)))
    (when (denote-retrieve-filename-signature file)
      (hierarchy-add-tree hierarchy (or file (buffer-file-name)) (unless no-parentfn parentfn) childrenfn)
      (hierarchy-sort hierarchy 'denote-fz-note<)
      hierarchy)))

(defun denote-fz-hierarchy-label ()
  (hierarchy-labelfn-indent
   (hierarchy-labelfn-button
    (lambda (item _)
      (let* ((signature (denote-retrieve-filename-signature item))
	     (file-type (denote-filetype-heuristics item))
	     (title (denote-retrieve-front-matter-title-value item file-type))
	     (keywords (denote-extract-keywords-from-path item))
	     (keywords-as-string (mapconcat 'identity keywords ", ")))
	(insert (format "%-6s %s %s"
			(if signature (propertize signature  'face 'font-lock-warning-face) "")
			(propertize (or title "") 'face 'font-lock-doc-face)
			(propertize keywords-as-string 'face 'font-lock-note-face)))))
    (lambda (item _)
      (find-file item)))
   "    "))

(defun denote-fz-hierarchy-parent (item)
  (let* ((parent (denote-fz-derived-signature 'parent item))
	 (note (denote-fz-search-note parent  'prefix)))
    note))

(defun denote-fz-hierarchy-children (item)
  (let* ((signature (denote-retrieve-filename-signature item))
	 (children
	  (denote-fz-find-sorted-files
	   (denote-fz-create-regex-string signature 'children))))
    children))

(defun denote-fz-hierarchy (&optional file no-parentfn)
  "Display  a hierarchy  buffer of  notes related  hierarchically to
FILE, if FILE is nil use the current buffer as the target note."
  (interactive)
  (require 'hierarchy)
  (let* ((file (or file (buffer-file-name)))
	 (hierarchy (denote-fz-build-hierarchy file no-parentfn))
	 (signature (denote-retrieve-filename-signature file)))
    (if hierarchy
	(progn
	  (switch-to-buffer
	   (hierarchy-tabulated-display
	    hierarchy
	    (denote-fz-hierarchy-label)
	    (get-buffer-create "*Denote Folgezettel Hierarchy*")))
	  (denote-fz-hierarchy-mode)
	  (re-search-forward (concat "[[:space:]]*" signature) nil t  )
	  (recenter-top-bottom))
      (message "Buffer has no valid hierarchy"))))

(defun denote-fz-hierarchy-next-section ()
  "Move to the next section of the current active hierarchy."
  (interactive)
  (let* ((entry (caar tabulated-list-entries))
	 (next-signature (denote-fz-derived-signature 'increment
						      entry))
	 (note (denote-fz-search-note next-signature)))
    (if note
	(denote-fz-hierarchy note)
      (message "Last Section"))))

(defun denote-fz-hierarchy-previous-section ()
  "Move to the previous section of the current active hierarchy."
  (interactive)
  (let* ((entry (caar tabulated-list-entries))
	 (previous-signature (denote-fz-derived-signature 'decrement
							  entry))
	 (note (denote-fz-search-note previous-signature)))
    (if note
	(denote-fz-hierarchy note)
      (message "First Section"))))

(defun denote-fz-hierarchy-top ()
 "Display a hierarchy buffer of the top level notes." 
  (interactive)
  (let* ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy
			 (denote-fz-find-sorted-files
			  denote-fz-top-level-regex)
			 nil)
    (if hierarchy
	(progn
	  (switch-to-buffer
	   (hierarchy-tabulated-display
	    hierarchy
	    (denote-fz-hierarchy-label)
	    (get-buffer-create "*Denote Folgezettel Hierarchy*")))
	  (denote-fz-hierarchy-mode))
      (message "Buffer has no valid hierarchy"))))

(defun denote-fz-hierarchy-expand ()
  "Display a hierarchy buffer of the current hierarchy item at point." 
  (interactive)
  (let* ((button (button-at (point)))
	 ;; NOTE: get note from the button's action.
	 (file (aref (aref 
		      (button-get button 'action) 2) 1))) 
    (denote-fz-hierarchy file :no-parentfn)))

(defun denote-fz-hierarchy-upper-level ()
  "Display a hierarchy buffer of the current hierarchy's root." 
  (interactive)
  (if-let* ((button (button-at (point)))
	    ;; NOTE: get note from the root entry.
	    (entry (caar tabulated-list-entries))
	    (signature (denote-fz-derived-signature 'parent entry))
	    (parent (denote-fz-search-note signature)))
      (denote-fz-hierarchy parent :no-parentfn)
    (denote-fz-hierarchy-top)))

(defvar denote-fz-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "K") 'denote-fz-hierarchy-previous-section)
    (define-key map (kbd "J") 'denote-fz-hierarchy-next-section)
    (define-key map (kbd "j") 'forward-button)
    (define-key map (kbd "k") 'backward-button)
    (define-key map (kbd "o") 'denote-fz-hierarchy-top)
    (define-key map (kbd "e") 'denote-fz-hierarchy-expand)
    (define-key map (kbd "u") 'denote-fz-hierarchy-upper-level)
    map)
  "Keymap used for `denote-fz-hierarchy-mode'.")

(define-derived-mode denote-fz-hierarchy-mode hierarchy-tabulated-mode "dfz-h"
  "Minor mode for providing keybindings to a hierarchy.el buffer."
  :init-value nil
  :group 'denote-fz-hierarchy
  :keymap denote-fz-hierarchy-mode-map
  :lighter "dfz-h"
  )

;;;; Dired Commands
(defun denote-fz-dired-signature-buffer ()
  "Create  a Dired  buffer displaying sorted notes.
With prefix argument, call `dired-jump' instead."
  (interactive)
  (if (or current-prefix-arg (not denote-fz-mode))
      (funcall (advice--cd*r (symbol-function #'dired-jump)))
    (funcall denote-fz-dired-function)))

(defun denote-fz-dired-top-level-notes ()
  "Create a Dired buffer displaying the top level notes."
  (interactive)
  (funcall denote-fz-dired-function denote-fz-top-level-regex))

(defun denote-fz-dired-section (&optional file)
  "Create a  Dired buffer displaying immediate descendent notes.
The Dired file at point or the current buffer is used as starting signature."
  (interactive)
  (let* ((file (or file (dired-get-filename nil t) (buffer-file-name (current-buffer))))
	 (signature (denote-retrieve-filename-signature file))
	 (regex (denote-fz-create-regex-string signature 'section)))
   (denote-fz-set-section signature)
  (funcall denote-fz-dired-function regex)))

(defun denote-fz-dired-full-section (&optional file)
  "Create a  Dired buffer displaying the descendent notes.
The Dired file at point or the current buffer is used as starting signature."
  (interactive)
  (let* ((file (or file (dired-get-filename nil t) (buffer-file-name (current-buffer))))
	 (signature (denote-retrieve-filename-signature file))
	 (regex (denote-fz-create-regex-string signature 'full-section)))
   (denote-fz-set-section signature)
    (funcall denote-fz-dired-function regex)))

(defun denote-fz-dired-section-up (&optional file)
  "Create a  Dired buffer.
It displays the immediate descendant  notes of the upper level of
the Dired file at point or the current buffer."
  (interactive)
  (let* ((file (or (dired-get-filename nil t) (buffer-file-name (current-buffer))))
	 (signature (denote-retrieve-filename-signature file))
	 (parent (denote-fz-string-variation signature 'parent))
	 (regex (if (not parent)
		    "[0-9]+--.*"
		  (denote-fz-create-regex-string parent 'section))))
    (denote-fz-set-section parent)
    (funcall denote-fz-dired-function regex)))

(defun denote-fz-dired-full-section-up (&optional file)
  "Create a  Dired buffer.
It displays the immediate descendant  notes of the upper level of
the Dired file at point or the current buffer."
  (interactive)
  (let* ((file (or (dired-get-filename nil t) (buffer-file-name (current-buffer))))
	 (signature (denote-retrieve-filename-signature file))
	 (parent (denote-fz-string-variation signature 'parent))
	 (regex (if (not parent)
		    (denote-fz-dired-signature-buffer)
		  (denote-fz-create-regex-string parent 'full-section))))
    (denote-fz-set-section parent)
    (funcall denote-fz-dired-function regex)))

(defun denote-fz-dired-next-section ()
  "Create a Dired Buffer.
It displays  section that follows `denote-fz-dired-current-section'."
  (interactive)
  (let* ((next-signature (denote-fz-derived-signature 'increment
						     (denote-fz-get-section)))
	 (note (denote-fz-search-note next-signature)))
    (if note
	(denote-fz-dired-section note)
      (message "Last Section"))))

(defun denote-fz-dired-next-full-section ()
  "Create a Dired Buffer.
It displays the full section that follows `denote-fz-dired-current-section'."
  (interactive)
  (let* ((signature (denote-fz-derived-signature 'increment
						 (denote-fz-get-section)))
	 (note (denote-fz-search-note signature)))
    (if note
	(denote-fz-dired-full-section note)
      (message "Last Section"))))

(defun denote-fz-dired-previous-section ()
  "Create a Dired Buffer.
It displays the section before `denote-fz-dired-current-section'."
  (interactive)
  (let* ((signature (denote-fz-derived-signature 'decrement
						 (or (denote-fz-derived-signature 'parent
										  (denote-fz-get-section))
						     (denote-fz-get-section))))
	 (note (denote-fz-search-note signature)))
    (if note
	(denote-fz-dired-section note)
      (message "First Section"))))

(defun denote-fz-dired-previous-full-section ()
  "Create a Dired Buffer.
It displays the full section before `denote-fz-dired-current-section'."
  (interactive)
  (let* ((signature (denote-fz-derived-signature 'decrement
						 (or (denote-fz-derived-signature 'parent
										  (denote-fz-get-section)
										  )
						     (denote-fz-get-section))))
	 (note (denote-fz-search-note signature)))
    (if note
	(denote-fz-dired-full-section note)
      (message "Last Section"))))

(defun denote-fz-dired-last-section ()
 "Create a Dired Buffer using the section of `denote-fz-dired-current-section'."
  (interactive)
  (denote-fz-dired-section (denote-fz-search-note (denote-fz-get-section))))

(defun denote-fz-dired-last-full-section ()
  "Create a Dired Buffer using the full section of `denote-fz-dired-current-section'."
  (interactive)
  (denote-fz-dired-full-section (denote-fz-search-note (denote-fz-get-section))))

;;;; Renaming commands
(defun denote-fz-rename-unnumbered (&optional file)
  "Rename Dired marked files' signature to unnumbered."
  (interactive)
  (let ((denote--used-ids)
	(denote-prompts nil)
	(denote-rename-confirmations nil))
    (if-let ((marks (dired-get-marked-files)))
	(progn
	  (unless (seq-every-p #'denote-file-has-identifier-p marks)
	    (setq denote--used-ids (denote--get-all-used-ids)))
	  (dolist (file marks)
	    (pcase-let ((`(,title ,keywords ,signature ,date)
			 (denote--rename-get-file-info-from-prompts-or-existing file)))
	      (denote--rename-file file title keywords "unnumbered" date)))
	  (denote-fz-dired-signature-buffer))
      (user-error "No marked files; aborting"))))

(defun denote-fz-rename-assign-top-level ()
  (interactive)
  "Rename marked dired files and set them as children of a target note."
  (let (
	(target  (denote-fz-find-file))
	(denote--used-ids)
	(denote-prompts nil)
	(denote-rename-confirmations nil))
    (if-let ((marks (dired-get-marked-files)))
	(progn
	  (unless (seq-every-p #'denote-file-has-identifier-p marks)
	    (setq denote--used-ids (denote--get-all-used-ids)))
	  (dolist (file marks)
	    (pcase-let ((`(,title ,keywords ,signature ,date)
			 (denote--rename-get-file-info-from-prompts-or-existing file)))
	      (denote--rename-file file title keywords (denote-fz-find-valid-signature (denote-fz-derived-signature 'child  target))   date))
	    )
	  (denote-fz-dired-signature-buffer))
      (user-error "No marked files; aborting"))))

;;; Find File
(define-inline denote-fz-pretty-format-filename (&optional file)
  "Return  a  pretty  formatted string of a note.
If  denote id  is omitted  it includes  only signature  title and
keywords. FILE is a denote path or string."
  (inline-quote
   (cons
    (let* ((file (or ,file (buffer-file-name)))
	   (signature (denote-retrieve-filename-signature file))
	   (file-type (denote-filetype-heuristics file))
	   (title (denote-retrieve-front-matter-title-value file file-type))
	   (keywords (denote-extract-keywords-from-path file))
	   (keywords-as-string (mapconcat 'identity keywords ", ")))
      (format "%-6s %s %s"
	      (propertize (or signature "") 'face 'font-lock-warning-face)
	      (propertize (or title "") 'face 'font-lock-doc-face)
	      (propertize keywords-as-string 'face 'font-lock-note-face)))
    ,file)))

(defun denote-fz-find-file (&optional regex variation prompt)
  "Find a denote note using  `completing-read'.
The  list of  candidates is  pretty printed.  REGEX is  a regular
expression to filter the search.  VARIATION is what kind of notes
to look  for. PROMPT  is prompt  string. Called  interactively it
uses `find-file' otherwise return the filename."
  (interactive)
  (let* ((vertico-sort-function 'identity);; Prevents sorting by history
	 (paths (mapcar #'denote-fz-pretty-format-filename
			(denote-fz-search-files (or regex ".*") variation)))
	 (filename (cdr (assoc (completing-read (or prompt "Note: ") paths  nil t) paths))))
    (if (called-interactively-p 'interactive)
	(find-file filename)
      ;; For programmatic use, just return the filename.
      filename)))

;;;; Dynamic Blocks
(defun denote-fz-parent-directory (file)
  "Return parent directory of FILE."
  (file-name-nondirectory
	 (directory-file-name
	  (file-name-directory
	   file))))

(defun denote-fz-insert-dblock (regexp)
  "Create a db block using REGEXP.
Sorted by signature"
  (org-create-dblock (list :name "denote-links"
			   :regexp regexp
			   :sort-by-component 'denote-fz-note<
			   :reverse-sort nil
			   :id-only nil))
  (org-update-dblock))

(defun denote-fz-insert-section-dblock ()
  "Insert   dblock   of the current buffer's section."
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (section (if (and signature (not (string-empty-p signature)))
		      (denote-fz-create-regex-string signature 'section)
		    "[0-9]+--"))
	 (regexp (concat parent-dir "/.*==" section)))
    (denote-fz-insert-dblock regexp)))

(defun denote-fz-insert-full-section-dblock ()
  "Insert dblock of the current buffer's full section."
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (full-section (if (not (equal signature ""))
			   (denote-fz-create-regex-string signature 'full-section)
			 "[0-9]+--"))
	 (regexp (concat parent-dir ".*==" full-section)))
    (denote-fz-insert-dblock regexp)))

(defun denote-fz-insert-current-section-dblock ()
  "Insert dblock  with the current buffer's notes  at  the same level."
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (full-section (if (not (equal signature ""))
			   (denote-fz-create-regex-string (denote-fz-string-variation signature 'parent)
							  'section)
			 "[0-9]+--"))
	 (regexp (concat parent-dir ".*==" full-section)))
    (denote-fz-insert-dblock regexp)))

;;;; denote-fz-rename-buffer
(defun denote-fz-rename-buffer (&optional buffer)
  "Rename the buffer using  `denote-rename-buffer' adding the number of nested notes."
  (let* ((file (buffer-file-name buffer))
	 (signature (and file (denote-retrieve-filename-signature file)))
	 (regex (and signature (denote-fz-create-regex-string signature 'children)))
	 (notes (and regex (denote-fz-find-sorted-files regex)))
	 (denote-rename-buffer-format
	  (if (> (length notes) 0)
	      (format "[%s] %s" (length notes)  denote-rename-buffer-format)
	    denote-rename-buffer-format)))
    (denote-rename-buffer buffer)))

;;;; denote-fz-mode
(defvar denote-fz-replace-dired-mode t)

(defvar denote-fz-mode-string
" d-fz")

(defvar denote-fz-dired-mode-string
" d-fzd")

(defcustom denote-fz-mode-hook nil
  "Run at the very end of `denote-fz-mode'."
  :group 'denote-fz
  :type 'hook)

(defcustom denote-fz-dired-mode-hook nil
  "Run at the very end of `denote-fz-dired-mode'."
  :group 'denote-fz-dired
  :type 'hook)

(defcustom denote-fz-keymap-prefix nil
  "denote-fz keymap prefix."
  :group 'denote-fz
  :type 'string)

(defvar denote-fz-command-map
  (let ((map (make-sparse-keymap)))
    ;; Note creation
    (define-key map (kbd "I") #'denote-fz-insert)
    (define-key map (kbd "i") #'denote-fz-insert-dwim)
    (define-key map (kbd "L") #'denote-fz-insert-at-level)
    (define-key map (kbd "l") #'denote-fz-insert-at-level-dwim)
    (define-key map (kbd "z") #'denote-fz-insert-zero)
    (define-key map (kbd "o") #'denote-fz-new)
    (define-key map (kbd "U") #'denote-fz-unnumbered)
    (define-key map (kbd "S") #'denote-fz-select-command)
    ;; Navigation
    (define-key map (kbd "u") #'denote-fz-unnumbered-cycle)
    (define-key map (kbd "f") #'denote-fz-find-note)
    (define-key map (kbd "F") #'denote-fz-find-note-in-full-section)
    (define-key map (kbd "k") #'denote-fz-goto-previous)
    (define-key map (kbd "j") #'denote-fz-goto-next)
    (define-key map (kbd "n") #'denote-fz-goto-nested)
    (define-key map (kbd "p") #'denote-fz-goto-upper-level)
    (define-key map (kbd "c") #'denote-fz-cycle)
    (define-key map (kbd ".") #'denote-fz-follow-through)
    (define-key map (kbd ",") #'denote-fz-backward-follow-through)
    (define-key map (kbd "g") #'denote-fz-hierarchy)
    ;; Dired
    (define-key map (kbd "m") #'denote-fz-dired-top-level-notes)
    (define-key map (kbd "a") #'denote-fz-dired-signature-buffer)
    (define-key map (kbd "s") #'denote-fz-dired-section)
    ;; DBlocks
    (define-key map (kbd "q") #'denote-fz-insert-section-dblock)
    (define-key map (kbd "w") #'denote-fz-insert-full-section-dblock)
    map)
  "Keymap for denote-fz commands after `denote-fz-keymap-prefix'.")

(defvar denote-fz-mode-map
  (let ((map (make-sparse-keymap)))
    (when denote-fz-keymap-prefix
      (define-key map denote-fz-keymap-prefix 'denote-fz-command-map))
    map)
  "Keymap used for `denote-fz-mode'.")

;;;###autoload
(define-minor-mode denote-fz-mode
  "Provide functions for creating and navigating folgezettel notes."
  :init-value nil
  :keymap denote-fz-mode-map
  :lighter denote-fz-mode-string
  (if denote-fz-mode
      (progn
	(setq-local denote-rename-buffer-format  "%s %t")
	(setq-local denote-rename-buffer-function  'denote-fz-rename-buffer)
	(when denote-fz-replace-dired-mode
          (advice-add 'dired-jump :override #'denote-fz-dired-signature-buffer))
	(denote-rename-buffer-mode t)
	(denote-rename-buffer-rename-function-or-fallback)
	(run-hooks 'denote-fz-mode-hook))
    (progn
      (advice-remove 'dired-jump #'denote-fz-dired-signature-buffer)
      (setq-local denote-rename-buffer-format  "%t"))
    (denote-rename-buffer-rename-function-or-fallback)))

(defvar denote-fz-dired-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used for `denote-fz-dired-mode'.")

;;;###autoload
(define-minor-mode denote-fz-dired-mode
  "Minor mode for creating Dired buffers containing sorted notes."
  :init-value nil
  :group 'denote-fz-dired
  :keymap denote-fz-dired-mode-map
  :lighter denote-fz-dired-mode-string
  (when denote-fz-dired-mode
      (run-mode-hooks 'denote-fz-dired-mode-hook)))

(provide 'denote-fz)
;;; denote-fz.el ends here


