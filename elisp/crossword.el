(require 'matrix)

(defun make-crossword (size)
  "Make a crossword grid with SIZE rows and colums."
  (if (zerop (% size 2));;偶数
      (error "make-crossword: size must be odd")
    (if (< size 3)
        (error "make-crossword: size must be greater than 3")
      (make-matrix size size nil))))

(defsubst crossword-size (crossword)
  "Number of rows and columns in CROSSWORD"
  (matrix-rows crossword))

(defsubst crossword-ref (crossword row column)
  "Get the element of CROSSWORD at ROW and COLUMN."
  (matrix-ref crossword row column))

;;private function
(defsubst crossword--set (crossword row column elt)
  "Internal function for setting a crossword grid square."
  (matrix-set crossword row column elt))

(defun crossword-cousin-position (crossword row column)
  "Give the cousin position for CROSSWORD ROW and COLUMN."
  (let ((size (crossword-size crossword)))
    (cons (- size row 1) (- size column 1))))

(defun crossword-cousin-ref (crossword row column)
  "Get the cousin of CROSSWORD's ROW,COLUMN position."
  (let ((cousin-position (crossword-cousin-position crossword
                                                    row
                                                    column)))
    (crossword-ref crossword
                   (car cousin-position)
                   (cdr cousin-position))))

(defun crossword--cousin-set (crossword row column elt)
  "Internal function for setting the cousin of a cell."
  (let ((cousin-position (crossword-cousin-position crossword
                                                    row
                                                    column)))
    (crossword--set crossword
                    (car cousin-position)
                    (cdr cousin-position)
                    elt)))

(defun crossword-store-letter (crossword row column letter)
  "Given CROSSWORD, ROW, and COLUMN, put LETTER there."
  (crossword--set crossword row column 'letter)
  (if (numberp (crossword-cousin-ref crossword row column))
      nil
    (crossword--cousin-set crossword row column 'letter)))

(defun crossword-store-block (crossword row column)
  "Given CROSSWORD, ROW, and COLUMN, put a block there."
  (crossword--set crossword row column 'block)
  (crossword--cousin-set crossword row column 'block))

(defun crossword-clear-cell (crossword row column)
  "Erase the CROSSWORD cell at ROW,COLUMN."
  (if (numberp (crossword-cousin-ref crossword row column))
      (crossword--set crossword row column 'letter)
    (crossword--set crossword row column nil)
    (crossword--cousin-set crossword row column nil)))

(defun crossword-clear-cell (crossword row column)
  "Erase the CROSSWORD cell at ROW,COLUMN."
  (let ((cousin-position (crossword-cousin-position crossword
                                                    row
                                                    column)))
    (if (and (not (equal cousin-position
                         (cons row column)))
             (numberp (crossword-ref crossword
                                     (car cousin-position)
                                     (cdr cousin-position))))
        (crossword--set crossword row column letter)
      (crossword--set crossword row column nil)
      (crossword--set crossword
                      (car cousin-position)
                      (cdr cousin-position)
                      nil ))))

(defun crossword-block-p (crossword row column)
  "Does CROSSWORD's ROW,COLUMN cell contain a block?"
  (or (< row 0)
      (>= row (crossword-size crossword))
      (< column 0)
      (>= column (crossword-size crossword))
      (eq (crossword-ref crossword row column) 'block)))

(defun crossword-one-letter-p (crossword row column)
  "Is CROSSWORD cell at ROW,COLUMN a one-letter word?"
  (and (not (eq (crossword-ref crossword row column)
                'block))
       (or (and (crossword-block-p crossword (- row 1) column)
                (crossword-block-p crossword (+ row 1) column))
           (and (crossword-block-p crossword row (- column 1))
                (crossword-block-p crossword row (+ column 1))))))


(defun crossword-insert-grid (crossword)
  "Insert CROSSWORD into the current buffer."
  (mapcar 'crossword-insert-row crossword))

(defun crossword-insert-row (row)
  "Insert ROW into the current buffer."
  (mapcar 'crossword-insert-cell row)
  (insert "\n"))

(defun crossword-insert-cell (cell)
  "Insert CELL into the current buffer."
  (insert (cond ((null cell) ".")
                ((eq cell 'letter) "?")
                ((eq cell 'block) "#")
                ((numberp cell) cell))
          " "))

(defun crossword-place-cursor (row column)
  "Move point to ROW,COLUMN."
  (goto-char (point-min))
  (forward-line row)
  (forward-char (* column 2)))


(defun crossword-cursor-coords ()
  "Compute (ROW . COLUMN) from cursor position."
  (cons (- (current-line) 1)
        (/ (current-column) 2)))

(defun current-line ()
  "Return line number containing point."
  (let ((result 1)) ; Emacs counts lines startingfrom 1.
    (save-excursion
      (beginning-of-line) so bobp will work
      (while (not (bobp))
        (forward-line -1)
        (setq result (+ result 1))))
    result) )


(defun crossword-update-display (crossword)
  "Called after a change, keeps the display up to date."
  (let* ((coords (crossword-cursor-coords))
         (cousin-coords (crossword-cousin-position crossword
                                                   (car coords)
                                                   (cdr coords))))
    (save-excursion
      (crossword-place-cursor (car coords)
                              (cdr coords))
      (delete-char 2)
      (crossword-insert-cell (crossword-ref crossword
                                            (car coords)
                                            (cdr coords)))
      (crossword-place-cursor (car cousin-coords)
                              (cdr cousin-coords))
      (delete-char 2)
      (crossword-insert-cell (crossword-ref crossword
                                            (car cousin-coords)
                                            (cdr cousin-coords))))))

(defun crossword-erase-command ()
  "Erase current crossword cell."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-clear-cell crossword-grid
                          (car coords)
                          (cdr coords)))
  (crossword-update-display crossword-grid))

(defun crossword-block-command ()
  "Insert a block in current cell and cousin."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-store-block crossword-grid
                           (car coords)
                           (cdr coords)))
  (crossword-update-display crossword-grid))

(defun crossword-self-insert ()
  "Self-insert letter in current cell."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-store-letter crossword-grid
                            (car coords)
                            (cdr coords)
                            (aref (this-command-keys) 0)))
  (crossword-update-display crossword-grid))

(defun crossword-cursor-right (arg)
  "Move ARG cells to the right."
  (interactive "p") ;prefix arg as number
  (let* ((coords (crossword-cursor-coords))
         (new-column (+ arg (cdr coords))))
    (if (or (< new-column 0)
            (>= new-column (crossword-size crossword-grid)))
        (error "Out of bounds"))
    (crossword-place-cursor (car coords)
                            new-column)))

(defun crossword-cursor-left (arg)
  "Move ARG cells to the left."
  (interactive "p")
  (crossword-cursor-right (- arg)))

(defun crossword-cursor-down (arg)
  "Move ARG cells down."
  (interactive "p")
  (let* ((coords (crossword-cursor-coords))
         (new-row (+ arg (car coords))))
    (if (or (< new-row 0)
            (>= new-row (crossword-size crossword-grid)))
        (error "Out of bounds"))
    (crossword-place-cursor new-row
                            (cdr coords))))


defun crossword-cursor-up (arg)
"Move ARG cells up."
(interactive "p")
(crossword-cursor-down (- arg)))

(defun crossword-beginning-of-row ()
  "Move to beginning of current row."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor (car coords) 0)))

(defun crossword-end-of-row ()
  "Move to end of current row."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor (car coords)
                            (- (crossword-size crossword-grid)
                               1))))

(defun crossword-top-of-column ()
  "Move to top of current column."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor 0 (cdr coords))))

(defun crossword-bottom-of-column ()
  "Move to bottom of current row."
  (interactive)
  (let ((coords (crossword-cursor-coords)))
    (crossword-place-cursor (- (crossword-size crossword-grid)
                               1)
                            (cdr coords))))

(defun crossword-beginning-of-grid ()
  "Move to beginning of grid."
  (interactive)
  (crossword-place-cursor 0 0))

defun crossword-end-of-grid ()
"Move to end of grid."
(interactive)
(let ((size (crossword-size crossword-grid)))
  (crossword-place-cursor size size)))

(defun crossword-jump-to-cousin ()
  "Move to cousin of current cell."
  (interactive)
  (let* ((coords (crossword-cursor-coords))
         (cousin (crossword-cousin-position crossword-grid
                                            (car coords)
                                            (cdr coords))))
    (crossword-place-cursor (car cousin)
                            (cdr cousin))))

(defun crossword--mode-setup (grid)
  "Auxiliary function to set up crossword mode."
  (kill-all-local-variables)
  (setq major-mode 'crossword-mode)
  (setq mode-name "Crossword")
  (use-local-map crossword-mode-map)
  (make-local-variable crossword-grid)
  (setq crossword-grid grid)
  (crossword-place-cursor 0 0)
  (run-hooks 'crossword-mode-hook))

(defun crossword (size)
  "Create a new buffer with an empty crossword grid."
  (interactive "nGrid size: ")
  (let* ((grid (make-crossword size))
         (buffer (generate-new-buffer "*Crossword*")))
    (switch-to-buffer buffer)
    (crossword-insert-grid grid)
    (crossword--mode-setup grid)))

(defun crossword-mode ()
  "Major mode for editing crossword puzzles.
Special commands:
\\{crossword-mode-map}"
  (interactive)
  (crossword--mode-setup (crossword-parse-buffer)))

(defvar crossword-mode-map nil
  "Keymap for Crossword mode.")
(if crossword-mode-map
    nil
  (setq crossword-mode-map (make-keymap)))

(define-key crossword-mode-map "\C-a"
  crossword-beginning-of-row)

(define-key crossword-mode-map "\C-e"
  crossword-end-of-row)

(substitute-key-definition 'beginning-of-line
                           crossword-beginning-of-row
                           crossword-mode-map
                           (current-global-map))

(let ((equivs
       ((forward-char . crossword-cursor-right)
        (backward-char . crossword-cursor-left)
        (previous-line . crossword-cursor-up)
        (next-line . crossword-cursor-down)
        (beginning-of-line . crossword-beginning-of-row)
        (end-of-line . crossword-end-of-row)
        (beginning-of-buffer . crossword-beginning-of-grid)
        (end-of-buffer . crossword-end-of-grid))))
  (while equivs
    (substitute-key-definition (car (car equivs))
                               (cdr (car equivs))
                               crossword-mode-map
                               (current-global-map))
    (setq equivs (cdr equivs))))
  
