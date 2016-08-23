(defconst screenplay-version "0.1.0"
  "Current Screenplay version number")

(add-to-list 'auto-mode-alist '("\\.scr\\'" . screenplay-mode))

(defgroup screenplay nil
  "Screenplay editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "screenplay"))

(defcustom screenplay-mode-hook 'auto-fill-mode
  "Hook for Screenplay mode"
  :type 'hook
  :group 'screenplay)

(define-derived-mode screenplay-mode fundamental-mode "Screenplay"
  "Major mode for editing screenplays.

\\{screenplay-mode-map}"
  (define-key screenplay-mode-map (kbd "<tab>") 'screenplay-next-indent)
  (define-key screenplay-mode-map (kbd "<backtab>") 'screenplay-previous-indent))

(setq adaptive-fill-mode nil)

(defvar screenplay-margin-ring (make-ring 5))
(ring-insert screenplay-margin-ring '("Transition" 40 20))
(ring-insert screenplay-margin-ring '("Character Name" 20 40)) 
(ring-insert screenplay-margin-ring '("Parenthetical" 15 25)) 
(ring-insert screenplay-margin-ring '("Dialogue" 10 35)) 
(ring-insert screenplay-margin-ring '("Action/Slugline" 0 60)) 

(defvar screenplay-current-indent 0)

(defun screenplay-set-margins (left fill)
  "Set margins depending on current block."
  (setq left-margin left)
  (setq fill-column (+ left fill)))

(defun screenplay-change-indent ()
  (let* ((margin (ring-ref screenplay-margin-ring screenplay-current-indent))
	 (indent-name (nth 0 margin))
	 (left (nth 1 margin))
	 (fill (nth 2 margin)))
    (screenplay-set-margins left fill)
    (indent-to-left-margin)
    (message indent-name)
  ))

(defun screenplay-next-indent ()
  (interactive)
  (setq screenplay-current-indent (+ screenplay-current-indent 1))
	(screenplay-change-indent))

(defun screenplay-previous-indent ()
  (interactive)
  (setq screenplay-current-indent (- screenplay-current-indent 1))
	(screenplay-change-indent))

(provide 'screenplay)
