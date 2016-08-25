;; screenplay-mode: an emacs major mode for editing screenplays
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
;; - make text go uppercase automatically when in slugline and
;;   transition indentation
;; - make ctrl-j default

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

There are five indentations that can be used for formatting the
text of the screenplay:

- Action block/Slugline
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
  (screenplay-mode-line-show)
  (screenplay-update-indent))

(setq adaptive-fill-mode nil)

(defvar screenplay-margin-ring (make-ring 5))
(ring-insert screenplay-margin-ring '("Transition" 40 20))
(ring-insert screenplay-margin-ring '("Character Name" 20 40)) 
(ring-insert screenplay-margin-ring '("Parenthetical" 15 25)) 
(ring-insert screenplay-margin-ring '("Dialogue" 10 35)) 
(ring-insert screenplay-margin-ring '("Action/Slugline" 0 60)) 

(defvar screenplay-current-indent 0)

(setq screenplay-mode-line "")

(defun screenplay-mode-line-show ()
  (add-to-list 'mode-line-format 'screenplay-mode-line t))

(defun screenplay-mode-line-hide ()
  (setq mode-line-format (delq 'screenplay-mode-line mode-line-format)))

(defun screenplay-set-margins (left fill)
  "Set margins depending on current block."
  (setq left-margin left)
  (setq fill-column (+ left fill)))

(defun screenplay-update-indent ()
  (let* ((indent (ring-ref screenplay-margin-ring screenplay-current-indent))
	 (margin-name (nth 0 indent))
	 (margin (nth 1 indent))
	 (fill (nth 2 indent)))
  (screenplay-set-margins margin fill)
  (indent-to-left-margin)
  (setq screenplay-mode-line margin-name)
  (force-mode-line-update)))

(defun screenplay-next-indent ()
  "Loops forward through the indentations available"
  (interactive)
  (setq screenplay-current-indent (+ screenplay-current-indent 1))
	(screenplay-update-indent))

(defun screenplay-previous-indent ()
  "Loops backwards through the indentations available"
  (interactive)
  (setq screenplay-current-indent (- screenplay-current-indent 1))
  (screenplay-update-indent))

(provide 'screenplay)
