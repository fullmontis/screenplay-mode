;; screenplay-mode: an emacs major mode for editing text-based screenplays
;; Copyright (C) 2016 Andrea Montagna

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
;; - make ctrl-j default (always go to left-margin)
;; - PDF export
;; - title page
;; - conversion from/to fountain

;; TOFIX:
;; - When using fill-paragraph on dialogue block, the name and
;;   dialogue box get mixed together
;; - do not verify indent by string but by value for clearer results
;; - when adding text in uppercase states the last word gets upcased 
;; - when filling paragraphs, the hanging indents are not removed
;; - currently only one line parentheticals are supported

;; Since the blocks are not regular paragraphs and are defined not by
;; some character strings but only by the left margin, we have to
;; develop custom functions to move around paragraphs and such. The
;; most important ones are backward-paragrah and forward-paragraph.

;; This is important for correctly filling out the paragraphs and
;; moving around.

(defconst screenplay-version "0.1.0"
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

There are six indentations that can be used for formatting the
text of the screenplay:

- Slugline
- Action block
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
  (add-hook 'post-self-insert-hook 'screenplay-post-self-insert-hook nil t)
  (screenplay-mode-line-show)
  (screenplay-update-indent))

(setq adaptive-fill-mode nil)

(defvar screenplay-margin-ring (make-ring 6))
(ring-insert screenplay-margin-ring '("Transition" 40 20))
(ring-insert screenplay-margin-ring '("Character Name" 20 40)) 
(ring-insert screenplay-margin-ring '("Parenthetical" 15 25)) 
(ring-insert screenplay-margin-ring '("Dialogue" 10 35))
(ring-insert screenplay-margin-ring '("Action" 0 60)) 
(ring-insert screenplay-margin-ring '("Slugline" 0 60))

(setq sp-slugline 0)
(setq sp-action 1)
(setq sp-dialogue 2)
(setq sp-parenthetical 3)
(setq sp-character 4)
(setq sp-transition 5)

(defvar screenplay-current-indent 0)
(defvar screenplay-last-indent 0)

(setq screenplay-mode-line "")

(defun screenplay-indent-name (indent-n)
  (nth 0 (ring-ref screenplay-margin-ring indent-n)))

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
  (let* ((indent (ring-ref screenplay-margin-ring screenplay-current-indent))
	 (margin-name (nth 0 indent))
	 (margin (nth 1 indent))
	 (fill (nth 2 indent)))
    (screenplay-set-margins margin fill)
    (indent-to-left-margin)
    (setq screenplay-mode-line margin-name)
    (force-mode-line-update)
    (if (string-equal margin-name "Parenthetical")
	(let ((pos (screenplay-point-in-line-indented))
	      line-length-indented)
	  (screenplay-add-parenthesis)
	  (back-to-indentation)
	  (forward-char (+ 1 pos)))
      (if (string-equal (screenplay-indent-name screenplay-last-indent) "Parenthetical")
	  (screenplay-remove-parenthesis)))))

(defun screenplay-add-parenthesis ()
  (back-to-indentation)
  (when (not (looking-at "()$"))
    (insert "(")
    (move-end-of-line nil)
    (insert ")")))
				    
(defun screenplay-remove-parenthesis ()
  "Delete parenthesis at the beginning and end of curent line"
  (save-excursion
    (if (progn
	  (back-to-indentation)
	  (looking-at "(.*)$"))
	(progn
	  (delete-forward-char 1 nil)
	  (move-end-of-line nil)
	  (backward-delete-char 1 nil)))))
	      
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
  (let ((indent (screenplay-indent-name screenplay-current-indent)))
    (when (or (string-equal indent "Slugline")
	      (string-equal indent "Character Name")
	      (string-equal indent "Transition"))
      (upcase-word -1))))

(defun screenplay-remove-indentation ()
  (backward-paragraph)
  (skip-chars-forward " \n\t")
  (indent-to-left-margin))

(defun screenplay-fill-paragraph (&optional justify)
  (interactive "P")
  (screenplay-remove-indentation)
  (fill-paragraph))

(setq fill-paragraph-function 'screenplay-fill-paragraph)

(provide 'screenplay)
