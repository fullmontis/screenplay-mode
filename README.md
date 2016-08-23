screenplay-mode 
===============

`screenplay-mode` is an Emacs major mode aiming for simple and
efficient editing of text-based screenplays.

The focus of this mode is simplicity: it takes care of text
indentation and filling in a fast and intuitive way, staying out of
the way as much as possible. This allows the author to just focus on
content.

This software is NOT part of GNU Emacs. 

## Commands

There are only two commands required in this mode:

* `<tab>` for moving indentation forward
* `<backtab>` (which defaults to `Shift-tab`) for moving indentation
backwards

The commands above loop through the following indentations (when the
last indentation is reached, the indentations loop to the first one):

* Action block/slugline
* Dialogue block
* Parenthetical block
* Character Name
* Transition

The filling of each paragraph is adjusted accordingly with the type of
block according to industry standards.

If the line has text in it when changing indentations, the line will
be indented again.

## License

This software is released under the GNU GPL v3.0. See the COPYING file
for details. 