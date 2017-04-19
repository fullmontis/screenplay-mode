`screenplay-mode` 
===============

`screenplay-mode` is an Emacs major mode aiming for simple and
efficient editing of text-based screenplays.

The focus of this mode is simplicity: it takes care of text
indentation and filling with just one command, staying out of the way
as much as possible. This allows the author to just focus on content
creation.

This software is NOT part of GNU Emacs. 

## Advantages

There are dozens of screenwriting software out there, and at least a
few other screenwriting modes for Emacs. Why should you use
`screenplay-mode`?

- *Compatibility*: it saves the screenplays in plain text, meaning they
will be compatible across all platforms, forever

- *Simplicity*: `screenplay-mode` is developed for simplicity and
effectiveness, so that you can focus on creating; you can start using
and mastering this mode in a few minutes

- *Fast*: you can write a screenplay at the speed of though with just
one command to move through the various indentations. 

- *Freedom*: free software under the GNU GPL v3

## Commands

The program emulates the way screenplay were written on typewriters,
with a few added benefits.

Typewriters had a system that allowed for tabulations to be saved at
specific spots across the line. This meant that the writer just had to
press tab a specific number of times to get to the right indentation
for the block.

This is the approach of screenplay-mode. It allows you to move along
the page, getting to the correct margin for the block you need to
use. The added benefits of using a computer instead of a typewriter is
that auto formatting is in place, allowing to create blocks of the
correct width, besides of course the added benefit of faster
editing. This simplifies formattation of the page and is streamlined
in a way that allows the writer to focus completely on content.

The following tabulations are available when writing (when the last
indentation is reached, the indentation loops to the first one):

* Slugline/Action block
* Dialogue block
* Parenthetical block
* Character Name
* Transition

The writer can loop through the tabulations with the `<tab>` and
`Shift-<tab>` keys. 

Each indentation creates a specific margin on the left of the page,
following industry standard spacing. When starting to write, the
program will automatically adjust the width of the block as you
write, going to newline when the required length is reached.

You can see what is the current indentation that is being used by
looking in the lower right part of the command line. 

*Important*: the program uses only white spaces for indentation, to be
able to recognize what indentation we are currently on. This means
that any tab should be converted back to whitespace.

Note that parenthesis are automatically and removed when entering or
exiting the Parenthetical block, if not already present.

When in the Character Name and Transition block, the text will
automatically be written uppercase.

There are only a handful of commands in this mode:

* `<tab>` for selecting the next tabulation. E.g. if you are in the
Dialogue block, pressing tab will move the cursor to the Parenthetical
block, adjusting the margin accordingly.
* `<backtab>` (which defaults to `Shift-TAB`) for the previous
tabulation. E.g. if you are in the Transition block, pressing backtab
will put you in the Slugline/Action block.
* `C-u <tab>` to move the current line indentation to the active
tabulation. E.g. if the line is indented as a Dialogue block and we
have the Slugline/Action tabulation active, pressing the combination
will indent the current line (and only the current line) to the
Slugline/Action margin
* `C-c C-c` to set the tabulation mode to the current line
indentation. E.g. if you are in the Character Name tabulation and the line
has the margin of the Dialogue block, pressing this key combination
will move the tabulation mode to Dialogue. This is useful when editing
the script.
* `C-c C-u` to upcase the current line. This is useful when for
example you forgot to upcase a slugline
* `C-<up>` and `C-<down>` to go to previous/next block. The start of a
block is the first line, before or after, with a different indentation
than the current one. 

The filling of each paragraph is adjusted accordingly with the type of
block according to industry standards.

If the line has text in it when changing indentations, the line will
be indented again.

If you accidentally started writing a paragraph in the wrong
tabulation, you can just go to the correct tabulation and press `M-q`
(`fill-paragraph`) to adjust to the new indentation.

## File Extension

`screenplay-mode` automatically loads `.scp` files as screenplay files.

## Removed Functions

Some functions are still present in the program to scrape the script
for character names, add them to a list and quickly access them in the
script. However, considering the bugs the functions have and the
minimal gain that such a function allows and the general added
slugginess, the use of these functions is discouraged. They may be
improved and made "official" in the future, but no plans are made for
it.

## Future Improvements (TODO)

* PDF export

A script is already present in the `screenplaypdf.sh`
script to convert the text script into pdf. However, you still need to
do this manually, and the pdf is missing some important qualities like
a title page. The command require the `iconv`, `enscript` and `ps2pdf`
programs to be present on the system. The system should be streamlined
inside the program itself.

* Adding title page
* Conversion from/to fountain
* Scene numbers on the side of the sluglines

## Known Bugs (TOFIX)

* currently only one line parentheticals are supported
* using fill-paragraph depends on the current intent state which can be confusing
* program should find the indentation state depending on the current line indentation
* filling should fill all parts that don't fit the current indentation
* erratic behaviour of screenplay-fill-paragraph when on empty lines

## License

This software is released under the GNU GPL v3.0. See the COPYING file
for details. 
