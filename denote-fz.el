;;; denote-fz.el --- Provides folgezettel commands to denote  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mirko Hernandez

;; Author: Mirko Hernandez <mirkoh@fastmail.com>
;; Maintainer: Mirko Hernandez <mirkoh@fastmail.com>>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
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
;; This  packages  provides  functions  for  creating  and  navigating
;; folgezettel notes.

(require 'denote)
(require 'denote-rename-buffer)

;;;; Constants
(defconst denote-fz-sort-command
  " | sed  's/--/=@/' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "
  "String used in find when calling `shell-command-to-string' command.
This enables the correct sorting of the Luhmann id according to the zettelkasten convention.")

(defconst denote-fz-sort-ls-option
  " | sed  's/--/=@/3' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "
  "String for setting `ls-option' for `find-dired' command")

;;;; Helpers - Strings
(defun denote-fz-trim-chars (str)
  "Trim letters from STR, from the right side."
  (string-trim str nil "[a-z]+"))

(defun denote-fz-trim-numbers (str)
  "Trim numbers from STR, from the right side."
  (string-trim str nil "[0-9]+"))

(defun denote-fz-get-last-char (str)
 "Return the last char of STR." 
  (substring str  -1 nil))

;; NOTE: this is just for reducing  the repetition of very similar let
;; expressions.
(defun denote-fz-split-last (str)
  "Return 3 values: string-without-last-char, last-char, is-last-char-number."
  (cl-values (substring str 0 -1 )
	     (denote-fz-get-last-char str)
	     (string-match "[0-9]" (denote-fz-get-last-char str))))

;; NOTE: full-section is the list of notes including the selected note
;; and all its descendants.
(defun denote-fz-create-regex-string (id variation)
  "Create a regex for searching children,parent, section, full-section.
Used by `denote-fz-execute-find-command' to find related notes."
  (cl-multiple-value-bind (string-without-last-char last-char last-char-is-num)
      (denote-fz-split-last id)
    (cl-case variation
      (parent  (if last-char-is-num
		   (concat (denote-fz-trim-numbers id) "[^a-z]-")
		 (concat id "[^0-9-]-")))
      (children  (if last-char-is-num
		     (concat id "[^0-9-]+")
		   (concat id "[^a-z-]+")))
      (full-section  (if last-char-is-num
			 (concat id "[^0-9]+[^a-z]*.*")
		       (concat id "[^a-z]+[^0-9]*.*")))
      (section  (if last-char-is-num
		    (concat id "[^0-9]+--.*")
		  (concat id "[^a-z]+--.*"))))))

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
      (cl-case variation
	(parent (if last-char-is-num
		    (denote-fz-trim-numbers str)
		  string-without-last-char))
	(decrement (denote-fz-string-decrement str))
	(sibling (denote-fz-string-increment str))
	(child (if last-char-is-num
		   (concat str "a")
		 (concat str "1")))
	(flat (if last-char-is-num
		  (concat (denote-fz-trim-numbers str) "1")
		(concat (denote-fz-trim-chars str) "a")))))))

;;; Find File String
;; Functions that find the corresponding  denote files by using the signature
;; or a regex as input.
(defun denote-fz-execute-find-command (regex)
  "Execute the find command, REGEX is concatenated with == and - (the enclosing characters of the signature).
Return string."
  (shell-command-to-string
   (concat "find * -regex " "'\..*==" regex ".*'" denote-fz-sort-command)))

(defun denote-fz-search-files (id &optional variation)
  "Execute the find command to find files matching the Luhmann ID modified by VARIATION.
Return string."
  (cl-case variation
    ;; prefix variation returns all the files that have id as prefix.
    (prefix (denote-fz-execute-find-command (concat id "--")))
    (children (denote-fz-execute-find-command
	       (denote-fz-create-regex-string id 'children)))
    ;; like children but it includes the parent note.
    (section (denote-fz-execute-find-command
	      (denote-fz-create-regex-string id 'section)))
    (siblings
     (denote-fz-search-files (denote-fz-string-variation id 'parent) 'children))
    (t
     (denote-fz-execute-find-command (concat id "--")))))

(defun denote-fz-search-note (id &optional variation)
  (let ((note  (string-trim (denote-fz-search-files id variation) nil "\n") ))
    (if (string-empty-p note)
	nil
      note)))

(defun denote-fz-find-valid-signature (signature)
  "Find if SIGNATURE is valid signature for note creation. Keep incrementing
the signature until a valid one is found." 
  (if (not (denote-fz-search-note signature))
	signature
      (denote-fz-find-valid-signature (denote-fz-string-increment  signature))))

(defun denote-fz-find-last-signature-at-level (file-or-signature)
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
	    (denote-fz-find-last-signature-nested last-child-signature)))))))

;;;; Denote defuns
(defun denote-fz-find-note ()
  "Visit a note using `completing-read.' and a pretty printed list."
  (interactive)
  (call-interactively 'denote-fz-find-file))

(defun denote-fz-visit-by-signature (signature)
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
			 (or keywords (denote-keywords-prompt))
			 ;; this is the filetype value
			 (and file (denote-filetype-heuristics file))
			 (and subdirectory (denote-subdirectory-prompt))
			 (and date (denote-date-prompt))
			 (and template (denote-template-prompt))
			 (or signature nil)))

(defun denote-fz-create-note (signature &optional no-auto-increment)
  "Creates a  note using SIGNATURE.If  the note already  exists keep
incrementing the signature until it finds a valid one for note creation.
If NO-AUTO-INCREMENT is non-nil the signature will not be incremented."
  (let ((denote-user-enforced-denote-directory default-directory)) 
    (if (not (denote-fz-search-note signature))
	(denote-fz-custom :signature signature :keywords nil)
      (if (not no-auto-increment)
	  (denote-fz-create-note (denote-fz-string-increment signature))
	(if (equal signature "un")
	    (denote-fz-custom :signature signature)
	  (message "Signature %s already exists" (propertize signature 'face  'font-lock-warning-face)))))))

(defun denote-fz-derived-signature (&optional variation file-or-signature)
  "Retrieves the current buffer's signature and creates a variation of that signature.
VARIATION    specifies    how    to   modify    the    signature,
FILE-OR_SIGNATURE  a file  or  signature to  use  instead of  the
current buffer. Return string."
  (let* ((file-or-signature (or file-or-signature (buffer-file-name)))
	 (signature (if (denote-file-is-note-p file-or-signature)
			(denote-retrieve-filename-signature file-or-signature)
		      file-or-signature)))
    (denote-fz-string-variation signature variation)))

(defun denote-fz-first-note()
  "Create the first folgezettel note, with 1 as the signature."
  (interactive)
  (denote-fz-create-note "1"))

(defun denote-fz-unnumbered ()
 "Create an unnumbered note." 
  (interactive)
  (denote-fz-create-note "un" t))

(defun denote-fz-unnumbered-cycle ()
 "Cycle between unnumbered notes." 
  (interactive)
  (let* ((unnumbered-notes (split-string (denote-fz-search-files "un")))
	 (unnumbered-array  (cl-map 'vector 'identity  unnumbered-notes))
	 (array-length  (length unnumbered-array))
	 (current-position (cl-position  (file-name-nondirectory (buffer-file-name)) unnumbered-array :test 'equal))
	 (new-position (if (= (1+ current-position) array-length)
			   0
			 (1+ current-position))))
    (find-file
     (aref unnumbered-array new-position))))

(defun denote-fz-insert-dwim()
  "Uses  the current  buffer's  signature as  the  target. Insert  a
nested note using the target's signature id."
  (interactive)
  (denote-fz-create-note (denote-fz-derived-signature 'child)))

(defun denote-fz-insert-at-level-dwim ()
  "Uses the current buffer's signature as the target. Insert a
note of the target's signature id incremented by one."
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

;;;; Zettel Editing 
(defun denote-fz-add-signature (&optional file variation)
  (interactive)
  (let* ((file  (buffer-file-name))
	 (file-type (denote-filetype-heuristics file))
	 (title (denote-retrieve-title-value file file-type))
	 (keywords (denote-retrieve-keywords-value file file-type))
	 (current-signature  (denote-retrieve-filename-signature file))
	 (keywords (denote-retrieve-keywords-value file file-type))
	 (target (denote-fz-find-file))
	 (signature  (denote-fz-find-valid-signature (denote-fz-derived-signature (or variation 'child) target))))
    (if (equal "un" current-signature)
	(denote-rename-file file title keywords signature)
      (message "Not an unnumbered note."))))

(defun denote-fz-add-signature-at-level (&optional file)
  (interactive)
  (denote-fz-add-signature file 'sibling)) 

;;;; Zettel navigation
(defun denote-fz-goto-upper-level ()
  "Visit the upper level note of the current buffer's signature id."  
  (interactive)
  (let* ((parent-signature (denote-fz-derived-signature 'parent))
	 (parent (denote-fz-search-note parent-signature)))
    (if parent
	(find-file parent)
      (message "Note in upper level does not exists."))))

(defun denote-fz-goto-nested ()
  "Visit a note corresponding with  the current buffer's signature id
first nested note." 
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
 "Visit a note  with the current buffer's  signature id incremented
by one unit. If the end of the sequence is reached start from the
first note of the current level." 
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'sibling))
	 (sibling (denote-fz-search-note sibling-signature))
	 (first-sibling-signature (denote-fz-derived-signature 'flat))
	 (first-sibling (denote-fz-search-note first-sibling-signature)))
    (if (not (string-empty-p sibling))
	(find-file sibling)
      (find-file (denote-fz-search-note first-sibling-signature))
      (message "%s" (propertize "Last Note of the sequence." 'face  'font-lock-warning-face)))))

(defun denote-fz-follow-through ()
  "Find  the next  contiguous  note. Prioritize  nested notes,  then
notes at the same level, then the next note in the upper level." 
  (interactive)
  (let* ((child-signature (denote-fz-derived-signature 'child))
	 (child (denote-fz-search-note child-signature) ))
    (if child
	(find-file child)
      (let* ((sibling-signature (denote-fz-derived-signature 'sibling))
	     (sibling (denote-fz-search-note sibling-signature)))
	(if sibling
	    (find-file sibling)
	  (let* ((parent-signature (denote-fz-derived-signature 'parent))
		 (parent-incremented (denote-fz-search-note (denote-fz-string-increment parent-signature))))
	    (if parent-incremented
		(find-file parent-incremented)
	      (message "%s" (propertize "Last Note." 'face  'font-lock-warning-face)))))))))

(defun denote-fz-backward-follow-through (&optional file)
  "Find  the previous  contiguous  note. Prioritize  nested notes of the previous note,  then
notes at the same level, then the previous note in the upper level." 
  (interactive)
  (let* ((current-signature (denote-retrieve-filename-signature (or file (buffer-file-name))))
	 (previous-signature (denote-fz-derived-signature 'decrement current-signature))
	 (previous-note (denote-fz-search-note previous-signature) )
	 (parent (denote-fz-search-note
		  (denote-fz-derived-signature 'decrement
					       (denote-fz-derived-signature 'parent))) )
	 (last-child-signature (denote-fz-find-last-signature-nested previous-signature))
	 (last-child (denote-fz-search-note last-child-signature)))
    (if (equal current-signature previous-signature)
	(if parent
	    (denote-fz-visit-by-signature  (denote-fz-find-last-signature-nested parent))
	  (message "%s" (propertize "First Note." 'face  'font-lock-warning-face)))
      (if (and last-child (not (string-empty-p last-child)))
	  (find-file last-child)
	(if (and previous-note (not (string-empty-p previous-note)))
	    (find-file previous-note)
	  (let* ((parent-signature (denote-fz-derived-signature 'parent))
		 (previous-parent (denote-fz-search-note (denote-fz-string-decrement parent-signature)) ))
	    (if (and previous-parent (not (string-empty-p previous-parent)))
		(find-file previous-parent)
	      (message "%s" (propertize "First Note." 'face  'font-lock-warning-face)))))))))

;;; Dired defuns
(defun denote-fz-set-find-ls-option (&optional regex)
  "Create  find-ls-option (used  with  find-dired)  using REGEX,  if
regex is null  use a regexp that searches for  all the notes that
have a signature." 
  (defvar-local find-ls-option nil) 
  (let ((find-argument  (concat (if regex
				    (concat "-regex '.*==" regex "'")
				  (concat "-regex '.*==.*" "'"))
				" -exec ls -ld {} \\+| awk /==/" denote-fz-sort-ls-option)))
    `(,find-argument
      .
      "-ld")))

;; NOTE: this needs to be set globally, find-dired works asynchronously.
(setq find-dired-refine-function nil)

(defun denote-fz-dired-signature-buffer ()
 "Create  a dired  buffer displaying  all the  notes sorted  by the
signature id." 
  (interactive)
  (let ((find-ls-option (denote-fz-set-find-ls-option)))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

(defun denote-fz-dired-top-level-notes ()
  "Create a dired buffer displaying the top level notes."  
  (interactive)
  (let ((find-ls-option (denote-fz-set-find-ls-option "[0-9]+--.*")))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

(defun denote-fz-dired-up ()
  "Create a  dired buffer displaying the  immediate descendent notes
of the dired file at point or the current buffer." 
  (interactive)
  (let ((find-ls-option (denote-fz-set-find-ls-option "[0-9]+--.*")))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

(defun denote-fz-dired-section ()
  "Create a  dired buffer displaying the  immediate descendent notes
of the dired file at point or the current buffer." 
  (interactive)
  (let* ((file (or (dired-get-filename nil t) (buffer-file-name (current-buffer))))
	 (signature (denote-retrieve-filename-signature file))
	 (regex (denote-fz-create-regex-string signature 'full-section))
	 (find-ls-option (denote-fz-set-find-ls-option
			  regex)))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

(defun denote-fz-dired-section-up ()
 "Create a  dired buffer displaying the  immediate descendant notes
of  the  upper   level  of  the  dired  file  at   point  or  the
current-buffer." 
  (interactive)
  (let* ((file (or (dired-get-filename nil t) (buffer-file-name (current-buffer))))
	 (signature (denote-retrieve-filename-signature file))
	 (parent (denote-fz-string-variation signature 'parent))
	 (find-ls-option (denote-fz-set-find-ls-option
			  (concat (if (equal parent "")
				      "[0-9]+"
				    parent)
				  "--.*"))))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

;;; Other
(define-inline denote-fz-pretty-format-filename (&optional file)
  "Return  a  pretty  formatted string of the note,  denote id  is  ommited  it
includes only signature title and keywords. FILE is a denote path or string."
  (inline-quote
   (cons
    (let* ((file (or ,file (buffer-file-name)))
	   (signature (denote-retrieve-filename-signature file))
	   (title (denote-retrieve-filename-title file))
	   (keywords (denote-extract-keywords-from-path file))
	   (keywords-as-string (mapconcat 'identity keywords ", ")))
      (format (concat "%s %s " (if keywords "-" "") "%s")
	      (propertize (or signature "") 'face 'font-lock-warning-face)
	      (propertize title 'face 'font-lock-doc-face)
	      (propertize keywords-as-string 'face 'font-lock-note-face)))
    ,file)))

(defun denote-fz-find-file (&optional regex)
  "Find a denote note using  `completing-read'. The list of candidates
is pretty  printed. REGEX is  a regular expression to  filter the
search. Called interactively uses find-file otherwise returns the
filename." 
  (interactive)
  (let* ((vertico-sort-function 'identity);; Prevents sorting by history
	 (vertico-buffer-mode t)
	 (paths (mapcar #'denote-fz-pretty-format-filename
			(split-string (denote-fz-search-files (or regex ".*")))))
	 (filename (cdr (assoc (completing-read "Note: " paths  nil t) paths))))
    (if (called-interactively-p 'interactive)
	(find-file filename)
      ;; For programatic use, just return the filename. 
      filename)))

;;;; org dynamic blocks
(defun denote-fz-parent-directory (file)
  "Return parent directory of FILE."
  (file-name-nondirectory
	 (directory-file-name 
	  (file-name-directory
	   file))))

(defun denote-fz-insert-dblock (regexp)
 "Create a db block using REGEXP. Sorted by signature" 
    (org-create-dblock (list :name "denote-links"
                             :regexp regexp
                             :sort-by-component 'signature
                             :reverse-sort nil
                             :id-only nil))
    (org-update-dblock))


(defun denote-fz-insert-section-dblock ()
 "Insert   dblock   with   a    regexp   corresponding   with   the
section (immediate descendents) of the current buffer id." 
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (section (if (not (equal signature ""))
			   (denote-fz-create-regex-string signature 'section)
			 "[0-9]+--"))
	 (regexp (concat parent-dir ".*==" section)))
    (denote-fz-insert-dblock regexp)))

(defun denote-fz-insert-full-section-dblock ()
 "Insert dblock with a regexp  corresponding with the full section of the
current buffer id. " 
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (full-section (if (not (equal signature ""))
			   (denote-fz-create-regex-string signature 'full-section)
			 "[0-9]+--"))
	 (regexp (concat parent-dir ".*==" full-section)))
    (denote-fz-insert-dblock regexp)))

;;;; denote-fz-mode 
(defvar denote-fz-mode-string
" denote-fz")

(defvar denote-fz-dired-mode-string
" denote-fz-dired")


(defcustom denote-fz-mode-hook nil
  "Run at the very end of `denote-fz-mode'."
  :group 'denote-fz
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
    (define-key map (kbd "u") #'denote-fz-unnumbered)
   ;; Navigation
    (define-key map (kbd "f") #'denote-fz-find-note)
    (define-key map (kbd "t") #'denote-fz-unnumbered-cycle)
    (define-key map (kbd "k") #'denote-fz-goto-previous)
    (define-key map (kbd "j") #'denote-fz-goto-next)
    (define-key map (kbd "n") #'denote-fz-goto-nested)
    (define-key map (kbd "p") #'denote-fz-goto-upper-level)
    (define-key map (kbd "c") #'denote-fz-cycle)
    (define-key map (kbd ".") #'denote-fz-follow-through)
    (define-key map (kbd ",") #'denote-fz-backward-follow-through)
    ;; Dired
    (define-key map (kbd "d") #'denote-fz-dired-mode)
    (define-key map (kbd "m") #'denote-fz-dired-top-level-notes)
    (define-key map (kbd "a") #'denote-fz-dired-signature-buffer)
    (define-key map (kbd "s") #'denote-fz-dired-section)
    ;; DBlocks
    (define-key map (kbd "q") #'denote-fz-insert-section-dblock)
    (define-key map (kbd "w") #'denote-fz-insert-full-section-dblock)
    map)
  "Keymap for denote-fz commands after `denote-fz-keymap-prefix'.")

(dolist (cmd '(denote-fz-goto-nested
	       denote-fz-goto-upper-level
	       denote-fz-goto-previous
	       denote-fz-goto-next
	       denote-fz-unnumbered-cycle
	       denote-fz-follow-through
	       denote-fz-backward-follow-through))
  (put cmd 'repeat-map 'denote-fz-command-map))

(defvar denote-fz-mode-map
  (let ((map (make-sparse-keymap)))
    (when denote-fz-keymap-prefix
      (define-key map denote-fz-keymap-prefix 'denote-fz-command-map))
    map)
  "Keymap used for `denote-fz-mode'.")


;;;###autoload
(define-minor-mode denote-fz-mode
  "Provides functions for creating and navigating folgezettel notes." 
  :init-value nil
  :keymap denote-fz-mode-map
  :lighter denote-fz-mode-string
  (if denote-fz-mode
      (progn
	(setq denote-rename-buffer-format "%s %t")
	(denote-rename-buffer-mode t)
	(denote-rename-buffer-rename-function-or-fallback)
	(run-mode-hooks 'denote-fz-mode-hook))
    (progn 
      (setq denote-rename-buffer-format "%t")
      (denote-rename-buffer-rename-function-or-fallback))))

(defvar denote-fz-dired-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used for `denote-fz-dired-mode'.")

;;;###autoload
(define-minor-mode denote-fz-dired-mode
  "Minor mode for creating dired buffers of folgezettel notes,
it  provides the  correct ordering  of  files in  dired and  some
narrowing functions." 
  :init-value nil
  :group 'denote-fz-dired
  :keymap denote-fz-dired-mode-map
  :lighter denote-fz-dired-mode-string 
  (if denote-fz-mode
      (progn
	(unless (eq major-mode 'dired-mode)
	  (when (denote-retrieve-filename-signature
		 (buffer-file-name))
	    (denote-fz-dired-signature-buffer))))))

(provide 'denote-fz)
;;; denote-fz.el ends here
