;; screenplay-mode: an emacs major mode for editing text-based screenplays
;; Copyright (C) 2016 Andrea Montagna <montagna.and@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO: 
;; - PDF export
;; - title page
;; - conversion from/to fountain
;; - scene numbers

;; TOFIX:
;; - currently only one line parentheticals are supported
;; - using fill-paragraph depends on the current intent state which can be confusing
;; - program should find the indentation state depending on the current line indentation
;; - filling should fill all parts that don't fit the current indentation
;; - erratic behaviour of screenplay-fill-paragraph when on empty lines

(defconst screenplay-version "0.1.2"
  "Current screenplay-mode version number")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scr\\'" . screenplay-mode))

(defgroup screenplay nil
  "Screenplay editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "screenplay"))

;; Automatically hook to auto-fill-mode for filling text
(defcustom screenplay-mode-hook 'auto-fill-mode
  "Hook for Screenplay mode"
  :type 'hook
  :group 'screenplay)

(define-derived-mode screenplay-mode fundamental-mode "Screenplay"
  "Major mode for editing screenplays.

There are five indentations that can be used for formatting the
text of the screenplay:

- Slugline/Action block
- Dialogue
- Parenthetical
- Character Name
- Transition

It is possible to loop through the commands by using the <tab>
and <backtab> (Shift+TAB) keys. The current indentation is shown
at the end of the mode line.

\\{screenplay-mode-map}"
  (define-key screenplay-mode-map (kbd "<tab>") 'screenplay-next-indent)
  (define-key screenplay-mode-map (kbd "<backtab>") 'screenplay-previous-indent)
  (define-key screenplay-mode-map (kbd "C-c C-u") 'screenplay-upcase-line)
  (define-key screenplay-mode-map (kbd "C-<up>") 'screenplay-backward-paragraph)
  (define-key screenplay-mode-map (kbd "C-<down>") 'screenplay-forward-paragraph)
  (define-key screenplay-mode-map (kbd "C-c C-c") 'screenplay-get-indent-from-margin)
  (define-key screenplay-mode-map (kbd "C-c C-e") 'screenplay-get-characters)
  (define-key screenplay-mode-map (kbd "C-M-c") 'screenplay-previous-character)
  (define-key screenplay-mode-map (kbd "RET") 'screenplay-newline-and-indent)
  (define-key screenplay-mode-map (kbd "DEL") 'screenplay-delete-backward-char)
  (add-hook 'post-self-insert-hook 'screenplay-post-self-insert-hook nil t)
  (setq indent-tabs-mode nil)
  (screenplay-mode-line-show)
  (screenplay-update-indent))

(setq adaptive-fill-mode nil)

(setq screenplay-margin-ring (make-ring 6))
(ring-insert screenplay-margin-ring 5)
(ring-insert screenplay-margin-ring 4) 
(ring-insert screenplay-margin-ring 3) 
(ring-insert screenplay-margin-ring 2)
(ring-insert screenplay-margin-ring 1) 
(ring-insert screenplay-margin-ring 0)

(setq sp-indent-names '("Slugline/Action" "Dialogue" "Parenthetical" "Character Name" "Transition"))
(setq sp-indent-margins '(0 10 15 20 40))
(setq sp-indent-fills '(60 35 25 40 20))

(setq sp-action 0)
(setq sp-dialogue 1)
(setq sp-parenthetical 2)
(setq sp-character 3)
(setq sp-transition 4)

(setq screenplay-current-indent 0)
(setq screenplay-last-indent 0)

(setq screenplay-mode-line "")

(defun screenplay-get-indent (indent-n)
  (ring-ref screenplay-margin-ring indent-n))

(defun screenplay-indent-name (indent-n)
  (nth (screenplay-get-indent indent-n) sp-indent-names))

(defun screenplay-indent-margin (indent-n)
  (nth (screenplay-get-indent indent-n) sp-indent-margins))

(defun screenplay-mode-line-show ()
  (add-to-list 'mode-line-format 'screenplay-mode-line t))

(defun screenplay-mode-line-hide ()
  (setq mode-line-format (delq 'screenplay-mode-line mode-line-format)))

(defun screenplay-set-margins (left fill)
  "Set margins depending on current block."
  (setq left-margin left)
  (setq fill-column (+ left fill)))

(defun screenplay-point-at-indentation ()
  (save-excursion
    (back-to-indentation)
    (point)))

(defun screenplay-point-in-line-indented ()
  (save-excursion
    (let ((this-point (point)))
      (back-to-indentation)
      (- this-point (point)))))

(defun screenplay-update-indent ()
  (let* ((indent-n (screenplay-get-indent screenplay-current-indent))
	 (margin-name (screenplay-indent-name indent-n))
	 (margin (screenplay-indent-margin indent-n))
	 (fill (nth indent-n sp-indent-fills)))
    (screenplay-set-margins margin fill)
    (indent-to-left-margin)
    (setq screenplay-mode-line margin-name)
    (force-mode-line-update)
    (if (eq indent-n sp-parenthetical)
	(let ((pos (screenplay-point-in-line-indented))
	      line-length-indented)
	  (screenplay-add-parenthesis)
	  (if (looking-at "(.*)$")
	      (forward-char)))
      (if (eq (screenplay-get-indent screenplay-last-indent) sp-parenthetical)
	  (screenplay-remove-parenthesis)))))

(defun screenplay-add-parenthesis ()
  (save-excursion
    (back-to-indentation)
    (when (not (looking-at "(.*)$"))
      (insert "(")
      (move-end-of-line nil)
      (insert ")"))))
				    
(defun screenplay-remove-parenthesis ()
  "Delete parenthesis at the beginning and end of curent line"
  (save-excursion
    (if (progn
	  (back-to-indentation)
	  (looking-at "(.*)$"))
	(progn
	  (delete-forward-char 1)
	  (move-end-of-line nil)
	  (backward-delete-char 1)))))

(defun screenplay-next-indent (arg)
  "Loops forward through the indentations available"
  (interactive "P")
  (setq screenplay-last-indent screenplay-current-indent)
  (if (not arg) (setq screenplay-current-indent (+ screenplay-current-indent 1)))
  (screenplay-update-indent))

(defun screenplay-previous-indent ()
  "Loops backwards through the indentations available"
  (interactive)
  (setq screenplay-last-indent screenplay-current-indent)
  (setq screenplay-current-indent (- screenplay-current-indent 1))
  (screenplay-update-indent))

(defun screenplay-upcase-line ()
  (interactive)
  (upcase-region (line-beginning-position) (line-end-position)))

(defun screenplay-post-self-insert-hook ()
  (let ((indent (screenplay-get-indent screenplay-current-indent)))
    (when (or (eq indent sp-character)
	      (eq indent sp-transition))
      (insert (upcase (delete-and-extract-region (- (point) 1) (point)))))))

(defun screenplay-on-last-line-p ()
  (eq (line-end-position) (point-max)))

(defun screenplay-on-first-line-p ()
  (eq (line-beginning-position) (point-min)))

(defun screenplay-match-indentation-p (&optional arg)
  "Tells if the indentation between the current line and the line
above matches. 

Returns t if we are in the first line of the file."
  (if (not (screenplay-on-first-line-p))
      (save-excursion
	(if arg (next-line arg))
	(let (this-line-indent)
	  (back-to-indentation)
	  (setq this-line-indent (- (point) (line-beginning-position))) 
	  (previous-line)
	  (back-to-indentation)
	  (if (eq (- (point) (line-beginning-position)) this-line-indent)
	      t
	    nil)))
    t))

(defun screenplay-is-on-paragraph-start-p ()
  (if (looking-at "[ \t]*$") ; empty line, start of paragraph
      t
    (if (not (screenplay-match-indentation-p))
	t
      nil)))

(defun screenplay-is-line-empty-p (&optional arg)
  (if (not arg) (setq arg 0))
  (save-excursion
    (next-line arg)
    (move-beginning-of-line nil)
    (looking-at "[ \t]*$")))

;; Since in the case of screenplays a block or paragraph is defined
;; not by simple regexp but also by indentation, these custom
;; functions have been created to allow this.

;; The screenplay-forward-paragraph and screenplay-backward-paragraph
;; search for the first line before or after point that is not empty
;; and that either has an empty line before, or has a different
;; intentation from the previous line.

;; These functions are used to fill correctly the different blocks. 

(defun screenplay-forward-paragraph ()
  (interactive)
  (let ((is-par-start nil))
    (while (not is-par-start)
      (if (screenplay-on-last-line-p) ; end of file
	  (progn
	    (move-end-of-line nil)
	    (setq is-par-start t))
	(next-line 1)
	(if (not (screenplay-is-line-empty-p))
	    (progn
	      (if (screenplay-is-line-empty-p -1)
		  (progn
		    (back-to-indentation)
		    (setq is-par-start t))
		(if (not (screenplay-match-indentation-p))
		    (progn
		      (back-to-indentation)
		      (setq is-par-start t)))))))))
  (point))

(defun screenplay-backward-paragraph ()
  (interactive)
  (let ((is-par-start nil))
    (while (not is-par-start)
      (if (screenplay-on-first-line-p) 
	  (progn
	    (back-to-indentation)
	    (setq is-par-start t))
	;; if we are already at the beginning of line, go to previous line
	(if (eq (point)
		(progn
		  (back-to-indentation)
		  (point)))
	    (previous-line))
	(if (not (screenplay-is-line-empty-p))
	    (progn
	      (if (or (screenplay-on-first-line-p)
		      (screenplay-is-line-empty-p -1))
		  (progn
		    (back-to-indentation)
		    (setq is-par-start t))
		(if (not (screenplay-match-indentation-p))
		    (progn
		      (back-to-indentation)
		      (setq is-par-start t)))))))))
  (point))

(defun screenplay-start-of-paragraph ()
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (screenplay-backward-paragraph)))

(defun screenplay-end-of-paragraph ()
  (interactive)
  (save-excursion
    (while (if (or (screenplay-on-last-line-p)
		   (and
		    (not (screenplay-is-line-empty-p))
		    (or
		     (not (screenplay-match-indentation-p 1))
		     (screenplay-is-line-empty-p 1))))
	       (move-end-of-line nil)
	     t)
      (next-line))
  (point)))

(defun screenplay-remove-indentation ()
  (goto-char (screenplay-start-of-paragraph))
  (indent-to-left-margin))

(defun screenplay-fill-paragraph (&optional justify)
  "Fills the following or current block according to the current
indentation state.

Returns t as required by fill-paragraph-function."
  (interactive "P")
  (let* ((end (goto-char (screenplay-end-of-paragraph)))
	(start (screenplay-start-of-paragraph)))
    (screenplay-remove-indentation)
    (fill-region-as-paragraph start end)
    t))

(setq fill-paragraph-function 'screenplay-fill-paragraph)

(defun screenplay-newline-and-indent ()
  (interactive)
  (and
   (eq (screenplay-get-indent screenplay-current-indent) sp-parenthetical)
   (looking-at ")$")
   (move-end-of-line nil))
  (newline)
  (indent-to-left-margin))

(defun screenplay-delete-backward-char (N &optional KILLFLAG)
  "Custom delete function for screenplay-mode.

It deletes an empty line if it is empty, otherwise just calls delete-backward-char."
  (interactive "p\nP")
  (if (screenplay-is-line-empty-p)
      (let ((end (line-end-position)))
	(previous-line)
	(move-end-of-line nil)
	(delete-region (point) end))
    (delete-backward-char N KILLFLAG)))

(defun screenplay-current-line-indentation ()
  (- (screenplay-point-at-indentation) (line-beginning-position)))

(defun screenplay-forward-paragraph-correct ()
  (interactive)
  (screenplay-forward-paragraph)
  (while (and (not (eq (point) (point-max)))
	      (not (eq (screenplay-current-line-indentation)
		      (screenplay-indent-margin screenplay-current-indent))))
      (screenplay-forward-paragraph)))

(defun screenplay-find-paragraph-indent-forward (indent)
  (interactive)
  (screenplay-forward-paragraph)
  (let ((found nil))
    (while (and (not (eq (point) (point-max)))
		(not (setq found
			   (eq (screenplay-current-line-indentation)
			       (nth indent sp-indent-margins)))))
      (screenplay-forward-paragraph))
    found))

(setq screenplay-character-list '())

(defun screenplay-get-char-name-parenthetical (line)
  "Divides character name from parentheticals in a string"
  (let* ((pos (string-match " *(.*)$" line))
	 (pos (if pos pos (length line)))
	 (char-name (substring line 0 pos))
	 (parens (substring line pos)))
    (cons char-name parens)))

(defun screenplay-get-characters ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (not (eq (point) (point-max)))
      (if (screenplay-find-paragraph-indent-forward sp-character)
	  (let* ((this-line (buffer-substring
			     (screenplay-point-at-indentation)
			     (line-end-position)))
		 (char-name (car (screenplay-get-char-name-parenthetical this-line))))
	    (setq screenplay-character-list
		  (if (not (member char-name screenplay-character-list))
		      (reverse (cons char-name (reverse screenplay-character-list)))
		    screenplay-character-list)))))))

(defun screenplay-previous-character ()
  (interactive)
  (if (and (eq screenplay-current-indent sp-character)
	   (eq (screenplay-current-line-indentation)
	       (nth sp-character sp-indent-margins)))
      (let* ((this-line (delete-and-extract-region
			 (screenplay-point-at-indentation)
			 (line-end-position)))
	     (char-name-new (car screenplay-character-list))
	     (line-div (screenplay-get-char-name-parenthetical this-line))
	     (char-name-old (car line-div))
	     (line-parens (cdr line-div)))
	(if (and (not (string-equal char-name-old "")) 
		 (not (member char-name-old screenplay-character-list)))
	    (setq screenplay-character-list
		  (reverse (cons char-name-old (reverse screenplay-character-list)))))
	(setq screenplay-character-list
	      (reverse (cons char-name-new (reverse (cdr screenplay-character-list)))))
	(indent-to-left-margin)
	(insert char-name-new line-parens))))

(defun screenplay-get-indent-from-margin ()
  (interactive)
  (let ((indent 0)
	(this-indent (screenplay-current-line-indentation))
	(found nil))
    (while (and (not found)
		(<= indent (length sp-indent-margins)))
      (if (eq this-indent (nth indent sp-indent-margins))
	  (setq found indent))
      (setq indent (+ 1 indent)))
    (if found
	(progn
	  (setq screenplay-current-indent found)
	  (screenplay-update-indent))
      (message "Current intentation doesn't correspond to any known margin! Please check for any spurious tabs."))))

(provide 'screenplay)
