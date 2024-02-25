;;; denote-fz.el --- Provides folgezettel commands to denote  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mirko Hernandez

;; Author: Mirko Hernandez <mirkoh@fastmail.com>
;; Maintainer: Mirko Hernandez <mirkoh@fastmail.com>>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Keywords: notes zettelkasten folgezettel
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
    (if  (equal last-char "9")
	(cond ((equal string-without-last-char "")
	       "10")
	      ((string-match "[0-9]" (denote-fz-get-last-char string-without-last-char))
	       (concat (denote-fz-string-increment string-without-last-char) "0" ))
	      ;; antecedent char is a letter, incrementing 9 to a 10.
	      (t (concat  string-without-last-char  "10")))
      (concat string-without-last-char (char-to-string (1+ (string-to-char last-char)))))))

(defun denote-fz-string-decrement (str)
  "Decrement the ascii value of the last character of STR.
Used for creating a Luhmann id."
  (cl-multiple-value-bind (string-without-last-char last-char)
      (denote-fz-split-last str)
    ;; Decrement string that ends in 0.
    (if  (equal last-char "0")
	(cond ((equal string-without-last-char "") ;; 0 can't be decremented, just return 0.
	       "0")
	      ((string-match "[0-9]" (denote-fz-get-last-char string-without-last-char))
	       (concat (denote-fz-string-decrement string-without-last-char) "9" ))
	      (t (concat  string-without-last-char  "0")))
      (concat string-without-last-char (char-to-string (1- (string-to-char last-char)))))))

;; NOTE: This function manages almost all id manipulations.
(defun denote-fz-string-variation (str variation)
  "Return a variation of the Luhmann id specified in STR.
VARIATION indicates how to modify the id."
  (when str
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

;;;; Denote defuns
(defun denote-fz-retrieve-ids (files)
  (mapcar #'denote-retrieve-filename-signature files))

;;;; Zettel creation
(defun denote-fz-find-note (id)
  (let ((vertico-sort-function 'identity) ;; Prevents sorting by history
	) 
    (completing-read "note:" (split-string
			      (denote-fz-search-files id))
		     nil
		     nil
		     "==")))

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
    (if (string-empty-p (denote-fz-search-files signature))
	(denote-fz-custom :signature signature :keywords nil)
      (if (not no-auto-increment)
	  (denote-fz-create-note (denote-fz-string-increment signature))
	(message "Signature %s already exists" (propertize signature 'face  'font-lock-warning-face))))))

(defun denote-fz-derived-signature (&optional variation file)
  "Retrieves the current buffer's signature and creates a variation of that signature.
VARIATION specifies how to modify the signature, FILE a file to use instead of the current buffer.
Return string."
  (let* ((file (or file (buffer-file-name)))
	 (signature (denote-retrieve-filename-signature file)))
    (denote-fz-string-variation signature variation)))

(defun denote-fz-first-note()
  "Create the first folgezettel note, with 1 as the signature."
  (interactive)
  (denote-fz-create-note "1"))

(defun denote-fz-insert-child-here()
  "Uses the current buffer's signature as starting point"
  (interactive)
  (denote-fz-create-note (denote-fz-derived-signature 'child)))

(defun denote-fz-insert-sibling-here ()
  "Uses the current buffer's signature as starting point"
  (interactive)
  (denote-fz-create-note (denote-fz-derived-signature 'sibling)))

(defun denote-fz-insert-child ()
  (interactive)
  (let ((file  (denote-fz-find-file)))
    (denote-fz-create-note (denote-fz-derived-signature 'child file))))

(defun denote-fz-insert-sibling ()
  (interactive)
  (let ((file  (denote-fz-find-file)))
    (denote-fz-create-note (denote-fz-derived-signature 'sibling file))))

;;; Zettel navigation
(defun denote-fz-goto-parent ()
  (interactive)
  (let* ((parent-signature (denote-fz-derived-signature 'parent))
	 (parent (string-trim (denote-fz-search-files parent-signature) nil "\n")))
    (if (not  (string-empty-p parent))
	(find-file parent)
      (message "Parent note does not exists."))))

(defun denote-fz-goto-child ()
  (interactive)
  (let* ((child-signature (denote-fz-derived-signature 'child))
	 (child (string-trim (denote-fz-search-files child-signature) nil "\n")))
    (if (not  (string-empty-p child))
	(find-file child)
      (message "Child note does not exists."))))

(defun denote-fz-goto-next-sibling ()
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'sibling))
	 (sibling (string-trim (denote-fz-search-files sibling-signature) nil "\n")))
    (if (not (string-empty-p sibling))
	(find-file sibling)
      (message "%s" (propertize "Last Note of the sequence." 'face  'font-lock-warning-face)))))

(defun denote-fz-goto-previous-sibling ()
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'decrement))
	 (first-sibling-signature (denote-fz-derived-signature 'flat))
	 (sibling (string-trim (denote-fz-search-files sibling-signature) nil "\n")))
    (if (not  (string-empty-p sibling))
	(progn
	  (find-file sibling)
	  (when (equal sibling-signature first-sibling-signature)
	    (message "%s" (propertize "First Note of the sequence." 'face  'font-lock-warning-face)))))))

(defun denote-fz-cycle-siblings () 
  (interactive)
  (let* ((sibling-signature (denote-fz-derived-signature 'sibling))
	 (sibling (string-trim (denote-fz-search-files sibling-signature) nil "\n"))
	 (first-sibling-signature (denote-fz-derived-signature 'flat))
	 (first-sibling (string-trim (denote-fz-search-files first-sibling-signature) nil "\n")))
    (if (not (string-empty-p sibling))
	(find-file sibling)
      (find-file (string-trim (denote-fz-search-files first-sibling-signature) nil "\n"))
      (message "%s" (propertize "Last Note of the sequence." 'face  'font-lock-warning-face)))))

;;; Dired defuns
(defun denote-fz-set-find-ls-option (&optional regex)
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
  (interactive)
  (let ((find-ls-option (denote-fz-set-find-ls-option)))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

(defun denote-fz-dired-main-notes ()
  (interactive)
  (let ((find-ls-option (denote-fz-set-find-ls-option "[0-9]+--.*")))
    (find-dired default-directory "")
      (denote-fz-dired-mode)))

(defun denote-fz-dired-parent ()
  (interactive)
  (let ((find-ls-option (denote-fz-set-find-ls-option "[0-9]+--.*")))
    (find-dired default-directory "")
      (denote-fz-dired-mode)))

(defun denote-fz-dired-section ()
  (interactive)
  (let* ((file (dired-get-filename nil nil) )
	 (signature (denote-retrieve-filename-signature file))
	 (regex (denote-fz-create-regex-string signature 'full-section))
	 (find-ls-option (denote-fz-set-find-ls-option
			  regex)))
    (find-dired default-directory "")
    (denote-fz-dired-mode)))

(defun denote-fz-dired-section-up ()
  (interactive)
  (let* ((file (or (dired-get-filename nil nil) (buffer-file-name)) )
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
 "Insert dblock with a regexp  corresponding with the section of the
current buffer id. " 
  (interactive)
  (let* ((file (buffer-file-name))
	 (parent-dir (denote-fz-parent-directory file))
	 (signature (denote-retrieve-filename-signature file))
	 (section (if signature
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
	 (full-section (if signature
			   (denote-fz-create-regex-string signature 'full-section)
			 "[0-9]+--"))
	 (regexp (concat parent-dir ".*==" full-section)))
    (denote-fz-insert-dblock regexp)))

;;;; denote-fz-mode 
(defvar denote-fz-mode-string
" denote-fz")

(defvar denote-fz-dired-mode-string
" denote-fz-dired")

(defvar denote-fz-mode-map
  (let ((map (make-sparse-keymap)))
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
	(denote-rename-buffer-rename-function-or-fallback))
    (setq denote-rename-buffer-format "%t")
    (denote-rename-buffer-rename-function-or-fallback)))

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
