# Denote Folgezettel

denote folgezettel (abbreviated denote-fz) is a minor mode that provides commands for
automatic signature creation and convenient navigation through a
zettelkasten.

Creating signatures that correspond with a folgezettel id can be
cumbersome and error prone, providing a solution for this is the
primary motivation of denote-fz.

# Requirements

Denote 2.0.0 or above is required.

# Installation

## Manual

denote-fz is not available on MELPA. To install manually, download
`denote-fz.el`.

```console
$ git clone https://github.com/MirkoHernandez/denote-folgezettel
```

Then the package can be loaded using `(load <path-to-the-package> t)`
or installed using M-x `package-install-file`.

M-x `package-initialize` may be required to immediately recognize the
package after installation (just once after the installation).

# Usage

denote-fz is primarily a set of commands, each can be used
independently after the mode is activated. The minor mode provides a
means for setting keybindings and, more importantly, to set
`denote-rename-buffer-mode` so that it displays the signature in the
modeline.

`dired-jump` is adviced so that it calls `denote-fz-dired-mode`
instead, this mode creates a dired buffer with the files sorted by
folgezettel. To disable this you can set `denote-fz-replace-dired-mode`to
nil.

All the commands use the current directory (`default-directory`) they
are supposed to be used from a folgezettel note or a directory
containing one. It is expected that all the folgezettel notes are
inside the same directory.

## Enabling the mode in a denote silo

To enable the mode in a zettelkasten silo (maybe as a subdirectory of
a denote silo) you can add the following variable in the `.dir-locals.el` file.

``` emacs-lisp
((nil . ((eval . (denote-fz-mode  t))))
```

If the directory itself is set as a silo, `.dir-locals.el`  should look similar to
this.

``` emacs-lisp
((nil . ((denote-directory . default-directory)
	    (eval . (denote-fz-mode t))))
```

## Keybinding Configuration

The `denote-fz-command-map` includes all the denote-fz bindings.

``` emacs-lisp
(define-key denote-fz-mode-map (kbd "C-c z") denote-fz-command-map)
```

| command                                | binding      |
|:---------------------------------------|:------------:|
| `denote-fz-insert`                     | <kbd>I</kbd> |
| `denote-fz-insert-dwim`                | <kbd>i</kbd> |
| `denote-fz-insert-at-level`            | <kbd>L</kbd> |
| `denote-fz-insert-at-level-dwim`       | <kbd>l</kbd> |
| `denote-fz-new`                        | <kbd>o</kbd> |
| `denote-fz-unnumbered`                 | <kbd>U</kbd> |
| `denote-fz-select-command`             | <kbd>S</kbd> |
| `denote-fz-find-note`                  | <kbd>f</kbd> |
| `denote-fz-find-note-in-full-section`  | <kbd>F</kbd> |
| `denote-fz-unnumbered-cycle`           | <kbd>u</kbd> |
| `denote-fz-goto-previous`              | <kbd>k</kbd> |
| `denote-fz-goto-next`                  | <kbd>j</kbd> |
| `denote-fz-goto-nested`                | <kbd>n</kbd> |
| `denote-fz-goto-upper-level`           | <kbd>p</kbd> |
| `denote-fz-cycle`                      | <kbd>c</kbd> |
| `denote-fz-follow-through`             | <kbd>.</kbd> |
| `denote-fz-backward-follow-through`    | <kbd>,</kbd> |
| `denote-fz-dired-top-level-notes`      | <kbd>m</kbd> |
| `denote-fz-dired-signature-buffer`     | <kbd>a</kbd> |
| `denote-fz-dired-section`              | <kbd>s</kbd> |
| `denote-fz-insert-section-dblock`      | <kbd>q</kbd> |
| `denote-fz-insert-full-section-dblock` | <kbd>w</kbd> |

## Other configuration options

`denote-fz-commands` can be set to a list of valid denote-fz commands
for note creation, these commands must take a signature as an argument
and create a denote note in the proper directory (`denote-user-enforced-denote-directory` can be used for this purpose).

The variable `denote-fz-create-function` can be used to permanently
change the default note creation function.

``` emacs-lisp
(setq denote-fz-commands
      '(denote-fz-create
	my/denote-fz-citar))
```

The default note creation function `denote-fz-create` (used in the
following example) creates the note using
`denote-user-enforced-denote-directory` otherwise this step should be
included manually.

Here is an example of a command that creates a note and also uses
`citar-denote-add-citekey`, from the citar-denote package, to add a
citekey.

``` emacs-lisp
(defun my/denote-fz-citar (signature)
  (interactive) 
  (let ((denote-after-new-note-hook '(denote-rename-buffer-rename-function-or-fallback
				      save-buffer 
				      citar-denote-add-citekey)))
    (funcall 'denote-fz-create signature)))
```

## Documentation

> [!NOTE] 
> Nested notes in a zettelkasten are not necessarily organized
> in a hierarchy. The commands are named in a neutral way to avoid
> denoting a specific relationship between the notes.

### Note creation
* [denote-fz-new](#denote-fz-new)
* [denote-fz-unnumbered](#denote-fz-unnumbered)
* [denote-fz-insert](#denote-fz-insert)
* [denote-fz-insert-at-level](#denote-fz-insert-at-level)
* [denote-fz-insert-dwim](#denote-fz-insert-dwim)
* [denote-fz-insert-at-level-dwim](#denote-fz-insert-at-level-dwim)
* [denote-fz-add-signature](#denote-fz-add-signature)
* [denote-fz-add-signature-nested](#denote-fz-add-signature-nested)
* [denote-fz-add-signature-at-level](#denote-fz-add-signature-at-level)
* [denote-fz-rename-unnumbered](#denote-fz-rename-unnumbered)
* [denote-fz-select-command](#denote-fz-select-command)

### Navigation

* [denote-fz-unnumbered-cycle](#denote-fz-unnumbered-cycle)
* [denote-fz-find-note](#denote-fz-find-note)
* [denote-fz-find-note-in-full-section](#denote-fz-find-note-in-full-section)
* [denote-fz-goto-upper-level](#denote-fz-goto-upper-level)
* [denote-fz-goto-nested](#denote-fz-goto-nested)
* [denote-fz-goto-next](#denote-fz-goto-next)
* [denote-fz-goto-previous](#denote-fz-goto-previous)
* [denote-fz-cycle](#denote-fz-cycle)
* [denote-fz-follow-through](#denote-fz-follow-through)
* [denote-fz-backward-follow-through](#denote-fz-backward-follow-through)

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

### denote-fz-new

Create a new top level note (its folgezettel will always be a number); if there are
none top level notes, it creates the first note, using "1" as the signature.

### denote-fz-unnumbered

Create a note with a signature named "unnumbered".

### denote-fz-insert

Prompt for a target note and create a nested note based in the target
note's signature. A target note with a signature 2a4 would create the
note 2a4a, if that note is already created it keeps looking for a
valid signature for note creation.

### denote-fz-insert-at-level

Prompt for a target note and create a note at level with the target
note's signature. A target note with a signature 2a4 would create a
note 2a5, if that note is already created it keeps looking for a valid
signature for note creation.

### denote-fz-insert-dwim

Same as `denote-fz-insert` but uses the current buffer's signature as
the target.

### denote-fz-insert-at-level-dwim 

Same as `denote-fz-insert-at-level` but uses the current buffer's
signature as the target.

### denote-fz-add-signature

Add a signature to an unnumbered note selecting a target note and
prompting which kind of signature to add.

### denote-fz-add-signature-nested

Add a signature to an unnumbered note selecting a target note and
creating a nested note using its signature.

### denote-fz-add-signature-at-level

Add a signature to an unnumbered note selecting a target note and
creating a note at level using its signature.

> [!NOTE]
> Notes should not generally change signature.The following command is
> used to correct the premature numbering of notes (I made this
> mistake in the initial stages of a few zettelkastens). 

### denote-fz-rename-unnumbered

Rename dired marked files' signature to unnumbered. Included for
convenience; proper use of `denote-fz-unnumbered` should eliminate the
need for this command.

### denote-fz-select-command

Change the note creation function to one from the list
`denote-fz-commands`.

## Navigation

### denote-fz-unnumbered-cycle

Cycle between unnumbered notes.

### denote-fz-find-note

Find a note using the minibuffer, the notes are sorted and pretty
printed.

### denote-fz-find-note-in-full-section

Find a note using the minibuffer, the completion list consist of all
the notes that start from the base of the current note's signature.
Using it on signature 20a5c would display all descendants of note 20.

### denote-fz-goto-upper-level

Find the upper level note corresponding with the current buffer's
signature. Example: Using it on a 2a4 signature would find the 2a
signature.

### denote-fz-goto-nested

Find the first nested note corresponding with the current buffer's
signature. Example: Using it on a 2a4 signature would find the 2a4a
signature.

### denote-fz-goto-next

Find the next note at level with the current buffer's signature.
Example: Using it on a 2a4 signature would find the 2a5 signature.

### denote-fz-goto-previous

Find the previous note at level with the current buffer's signature.
Example: Using it on a 2a4c signature would find the 2a4b signature.

### denote-fz-cycle

Like `denote-fz-goto-next` but it moves to the first note of the
sequence if it reaches the end.

### denote-fz-follow-through

Find the next contiguous note.

### denote-fz-backward-follow-through

Find the previous contiguous note.

## Dynamic blocks

> [!NOTE]
> "Section" here means the list of immediate descendants of a given note; "full section"
> means the list of all descendants. 

### denote-fz-insert-section-dblock 

Insert dblock with a regexp corresponding with the section of the
current buffer's signature. A signature 2 would generate a dblock with
the notes 2a 2b 2c, etc.

### denote-fz-insert-full-section-dblock 

Insert dblock with a regexp corresponding with the full-section of the
current buffer's signature. A signature 2 would generate a dblock with
the notes 2a 2a1 2a2a2 2b 2b1a, etc.

## Dired integration

### denote-fz-dired-signature-buffer

Open a dired buffer with the notes sorted by the folgezettel
id.

### denote-fz-dired-section

Open a dired buffer with the notes corresponding with all the
descendants of the dired file at point.

### denote-fz-dired-section-up

Open a dired buffer with the notes corresponding with all the
descendants of the upper level of the dired file at point.

### denote-fz-dired-top-level-notes

Open a dired buffer with all the top level notes (all the notes that
include only numbers).
