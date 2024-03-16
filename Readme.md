# Denote Folgezettel

Creating denote notes with signatures that correspond with a
folgezettel id can be cumbersome and error prone; denote folgezettel
(abbreviated denote-fz) provides commands for automatic signature
creation and for convenient navigation through a zettelkasten.

# Installation

## Manual

Download `denote-fz.el`, then use M-x `package-install-file`. 

M-x `package-initialize` may be required to immediately recognize the package after installation.

Denote 2.0.0 or above is required.

For now *sed* and *awk* are also required for sorting dired buffers.

# Usage

denote-fz is primarily a set of commands, each can be used on its own.

All the commands use the current directory (`default-directory`) they
are supposed to be used from a folgezettel note or a directory
containing one.

Enabling the mode in a zettelkasten silo. Just add the following
variable in the `.dir-locals.el` file.

``` emacs-lisp
((nil . ((denote-fz-mode . t))))
```

## Functions

> [!NOTE] 
> Nested notes in a zettelkasten are not necessarily organized
> in a hierarchy. The commands are named in a neutral way to avoid
> denoting a specific relationship between the notes.

### Note creation

* [denote-fz-first-note](#denote-fz-first-note)
* [denote-fz-unnumbered](#denote-fz-unnumbered)
* [denote-fz-insert](#denote-fz-insert)
* [denote-fz-insert-at-level](#denote-fz-insert-at-level)
* [denote-fz-insert-dwim](#denote-fz-insert-dwim)
* [denote-fz-insert-at-level-dwim](#denote-fz-insert-at-level-dwim)
* [denote-fz-add-signature](#denote-fz-add-signature)
* [denote-fz-add-signature-at-level](#denote-fz-add-signature-at-level)

### Navigation

* [denote-fz-unnumbered-cycle](#denote-fz-unnumbered-cycle)
* [denote-fz-find-note](#denote-fz-find-note)
* [denote-fz-goto-upper-level](#denote-fz-goto-upper-level)
* [denote-fz-goto-nested](#denote-fz-goto-nested)
* [denote-fz-goto-next](#denote-fz-goto-next)
* [denote-fz-goto-previous](#denote-fz-goto-previous)
* [denote-fz-cycle](#denote-fz-cycle)


### Dynamic blocks

* [denote-fz-insert-section-dblock](#denote-fz-insert-section-dblock)
* [denote-fz-insert-full-section-dblock](#denote-fz-insert-full-section-dblock)


## Dired integration

* [denote-fz-dired-mode](#denote-fz-dired-mode)
* [denote-fz-dired-section](#denote-fz-dired-section)
* [denote-fz-dired-section-up](#denote-fz-dired-section-up)
* [denote-fz-dired-top-level-notes](#denote-fz-dired-top-level-notes)


# Documentation

## Note creation

### denote-fz-first-note

Creates a note with signature "1".

### denote-fz-unnumbered

Creates a note with signature "un".

### denote-fz-insert

Prompts for a target note and creates a nested note based in the
target note's signature. A target note with a signature 2a4 would
create the note 2a4a, if that note is already created it keeps looking
for a valid signature for note creation.

### denote-fz-insert-at-level

Prompts for a target note and creates a note at level with the target
note's signature. A target note with a signature 2a4 would create a
note 2a5, if that note is already created it keeps looking for a valid
signature for note creation.

### denote-fz-insert-dwim

Same as denote-fz-insert but uses the current buffer's signature as
the target.

### denote-fz-insert-at-level-dwim 

Same as denote-fz-insert-at-level but uses the current buffer's
signature as the target.

### denote-fz-add-signature

Add a signature to an unnumbered note selecting a target note and
creating a nested note using its signature.

### denote-fz-add-signature-at-level

Add a signature to an unnumbered note selecting a target note and
creating a note at level using its signature.

## Navigation

### denote-fz-unnumbered-cycle

Cycle between unnumbered notes.

### denote-fz-find-note

Find a note using the minibuffer, the notes are sorted and pretty
printed.

### denote-fz-goto-upper-level

Find the upper level note corresponding with the current buffer's
signature. Example: Using it in a 2a4 signature would find the 2a
signature.

### denote-fz-goto-nested

Find the first nested note corresponding with the current buffer's
signature. Example: Using it in a 2a4 signature would find the 2a4a
signature.

### denote-fz-goto-next

Find the next note at level with the current buffer's signature.
Example: Using it in a 2a4 signature would find the 2a5 signature.

### denote-fz-goto-previous

Find the previous note at level with the current buffer's signature.
Example: Using it in a 2a4c signature would find the 2a4b signature.

### denote-fz-cycle

Like `denote-fz-goto-next` but it moves to the first note of the
sequence if it reaches the end.

## Dynamic blocks

### denote-fz-insert-section-dblock 

Insert dblock with a regexp corresponding with the section of the
current buffer's id.

### denote-fz-insert-full-section-dblock 

Insert dblock with a regexp corresponding with the full-section of the
current buffer's id.


## Dired integration

### denote-fz-dired-mode

Opens a dired buffer with the notes sorted by the folgezettel
id.

### denote-fz-dired-section

Opens a dired buffer with the notes corresponding with all the
descendants of the dired file at point.

### denote-fz-dired-section-up

Opens a dired buffer with the notes corresponding with all the
descendants of the upper level of the dired file at point.

### denote-fz-dired-top-level-notes

Opens a dired buffer with all the top level notes.
