# Denote Folgezettel

Creating denote notes with signatures that correspond with a
folgezettel id can be cumbersome and error prone; denote folgezettel
(abbreviated denote-fz) provides commands for automatic signature
creation and for convenient navigation through a zettelkasten.

# Installation

## Manual

Download `denote-fz.el`, then use M-x `package-install-file`. M-x
`package-initialize` may be required to recognize the package after
installation.

Denote 2.0.0 or above is required. 
For now *sed* and *awk* are also required for sorting dired buffers.

# Usage

denote-fz is primarily a set of commands, each can be used on its own.

All the commands use the current directory (`default-directory`) they
are supposed to be used from a folgezettel note or a directory
containing one.

To enable the mode you need `.dir-locals.el` in a zettelkasten
directory.

``` emacs-lisp
((nil . ((denote-fz-mode . t))))
```
## Note creation

denote-fz provides the following functions for automatically creating
notes with a folgezettel signature:

`denote-fz-first-note`

Creates a note with signature "1".

`denote-fz-insert-child`

Prompts for a target note and creates a child note based in the target
note's signature. A target note with a signature 2a4 would create the
note 2a4a, if that note is already created it keeps looking for a
valid signature for note creation.

`denote-fz-insert-sibling`

Prompts for a target note and creates a sibling note based in the
target note's signature. A target note with a signature 2a4 would
create a note 2a5, if that note is already created it keeps looking
for a valid signature for note creation.

`denote-fz-insert-child-here`

Same as denote-fz-insert-child but uses the current buffer's signature
as the target.

`denote-fz-insert-sibling-here` 

Same as denote-fz-insert-sibling but uses the current buffer's signature
as the target.

## Navigation

`denote-fz-find-file`

Find a note using the minibuffer, the notes are sorted and pretty
printed.

`denote-fz-goto-parent`

Find the parent note corresponding with the current buffer's
signature. Example: Using it in a 2a4 signature would find the 2a
signature.

`denote-fz-goto-child`

Find the first child note corresponding with the current buffer's
signature. Example: Using it in a 2a4 signature would find the 2a4a
signature.

`denote-fz-goto-next-sibling`

Find the next sibling note corresponding with the current buffer's
signature. Example: Using it in a 2a4 signature would find the 2a5
signature.

`denote-fz-goto-previous-sibling`

Find the previous sibling note corresponding with the current buffer's
signature. Example: Using it in a 2a4c signature would find the 2a4b
signature.

`denote-fz-cycle-siblings`

Like `denote-fz-goto-next-sibling` but it moves to the first note of
the sequence if it can't find the next sibling.

## Dired integration

`denote-fz-dired-mode`

Opens a dired buffer with the notes sorted by the folgezettel
id.

`denote-fz-dired-section`

Opens a dired buffer with the notes corresponding with all the
descendants of the dired file at point.

`denote-fz-dired-section-up`

Opens a dired buffer with the notes corresponding with all the
descendants of the parent of the dired file at point.

`denote-fz-dired-main-notes`

Opens a dired buffer with all the top level notes.
