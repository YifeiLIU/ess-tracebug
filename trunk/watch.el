
(defvar ess-watch-command
;; assumes that every expression is a structure of length 1 as returned by parse.
  "  if(!exists(\".ess_watch_expressions\") || length(.ess_watch_expressions) == 0){
  assign(\".ess_watch_expressions\", list(Exmaple = expression(\"Empty watch list!\")), envir = globalenv())
  }
  .ess_watch_eval <- function(){
  .essWEnames <- allNames(.ess_watch_expressions)
  len0p <- !nzchar(.essWEnames)
  .essWEnames[len0p] <- seq_along(len0p)[len0p]
  for(i in seq_along(.ess_watch_expressions)){
  cat(\"\n@---- \", .essWEnames[[i]], \" \", rep.int(\"-\", max(0, 30 - nchar(.essWEnames[[i]]))), \"@\n\", sep = \"\")
  cat( paste(\"@--->\", deparse(.ess_watch_expressions[[i]][[1L]])), \" \n\", sep = \"\")
  tryCatch(print(eval(.ess_watch_expressions[[i]])),
                error = function(e) cat(\"Error:\", e$message, \"\n\" ),
                warning = function(w) cat(\"warning: \", w$message, \"\n\"))
  }
  }
  .ess_watch_eval()
"
  )


(defun ess-watch-mode ()
  "Major mode for output from `ess-rdired'.
`ess-rdired' provides a dired-like mode for R objects.  It shows the
list of current objects in the current environment, one-per-line.  You
can then examine these objects, plot them, and so on.
\\{ess-rdired-mode-map}"
  (let ((cur-block ess-watch-current-block))
    (kill-all-local-variables )
    (make-local-variable 'revert-buffer-function)
    (setq revert-buffer-function 'ess-watch-revert-buffer)
    (use-local-map ess-watch-mode-map)
    (setq major-mode 'ess-watch-mode)
    (setq ess-watch-current-block 1)
    (setq mode-name (concat "watch " ess-current-process-name))
    (setq font-lock-defaults
          '(inferior-ess-font-lock-keywords nil nil ((?' . "."))))
    (turn-on-font-lock)
    (setq ess-watch-current-block-overlay
          (make-overlay (point-min) (point-max)))
    (overlay-put ess-watch-current-block-overlay
                 'face  'ess-watch-current-block-face)
    (ess-watch-set-current cur-block) ;;
    ))

(defun ess-watch ()
  "Run ess-watch mode on R objects.
This is the main function.  See documentation for `ess-watch-mode' though
for more information!"
  (interactive)
  (let ((wbuf (get-buffer-create ess-watch-buffer)))
    (ess-watch-buffer-show wbuf) ;;ess-watch-buffer-show displays the wbuf in accordance to custom settings
    (pop-to-buffer wbuf)
    (setq buffer-read-only nil)
    (ess-execute ess-watch-command nil
                 (substring ess-watch-buffer 1 (- (length ess-watch-buffer) 1)))
    ;; When definiting the function .watch.objects(), a "+ " is printed on the first line.
    ;; these are deleted:
    (goto-char (point-min))
    (delete-region (point-at-bol) (+ 1 (point-at-eol)))
    (ess-watch-mode)
    (setq buffer-read-only t)
    )
  )

(defvar ess-watch-mode-map nil
  "Keymap for the *R watch* buffer.")
(if ess-watch-mode-map
    ()
  (setq ess-watch-mode-map (make-sparse-keymap))
  (define-key ess-watch-mode-map "?" 'ess-watch-help)
  (define-key ess-watch-mode-map "k" 'ess-watch-kill)
                                        ;  (define-key ess-watch-mode-map "u" 'ess-watch-undelete)
  ;; editing requires a little more work.
  (define-key ess-watch-mode-map "a" 'ess-watch-add)
  (define-key ess-watch-mode-map "i" 'ess-watch-insert)
  (define-key ess-watch-mode-map "e" 'ess-watch-edit-expression)
  (define-key ess-watch-mode-map "r" 'ess-watch-rename)
  (define-key ess-watch-mode-map "q" 'ess-watch-quit)
  (define-key ess-watch-mode-map "u" 'ess-watch-move-up)
  (define-key ess-watch-mode-map "U" 'ess-watch-move-down)
  (define-key ess-watch-mode-map " " 'ess-watch-next-block)
  (define-key ess-watch-mode-map "n" 'ess-watch-next-block)
  (define-key ess-watch-mode-map [backspace] 'ess-watch-previous-block)
  (define-key ess-watch-mode-map "p" 'ess-watch-previous-block)
  ;; R mode keybindings.
  (define-key ess-watch-mode-map "\C-c\C-s" 'ess-watch-switch-process)
  (define-key ess-watch-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-watch-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
  (define-key ess-watch-mode-map "g" 'revert-buffer)
  )

(defface ess-watch-current-block-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "tan"))
    (((class color)
      (background dark))  (:background "gray15"))
    )
  "Face used to highlight currently debugged line."
  :group 'ess-debug
  )

(defvar  ess-watch-current-block-overlay nil
  "The overlay for currently selected block in the R watch buffer .")
(make-variable-buffer-local 'ess-watch-current-block-overlay)

(defvar ess-watch-current-block 1
  "Count of currently selected watch block.")
(make-variable-buffer-local 'ess-watch-current-block)

(defvar ess-watch-buffer "*R watch*"
  "Name of the watch buffer.")

(defcustom  ess-watch-start-block "@----"
  "String indicating the beginning of a block in watch buffer."
  :group 'ess-debug
  :type 'string)

(defcustom ess-watch-start-expression "@--->"
  "String indicating the beginning of an R expression in watch buffer."
  :group 'ess-debug
  :type 'string))

(defcustom ess-watch-height-threshold split-height-threshold
  "Minimum height for splitting R windwow sensibly to make space for watch window.
Has exactly the same meaning and initial value as `split-height-threshold'."
  :group 'ess-debug
  :type 'integer)

(defcustom ess-watch-width-threshold split-width-threshold
  "Minimum width for splitting R windwow sensibly to make space for watch window.
Has exactly the same meaning and initial value as `split-width-threshold'."
  :group 'ess-debug
  :type 'integer)


(defun ess-watch-block-limits-at-point ()
  "Return start and end positions of the watch block."
  (interactive)
  (save-excursion
    (let ((curr (point))
          start-pos end-pos)
      (end-of-line)
      (setq start-pos
            (if (re-search-backward ess-watch-start-block nil t )
                (point)
              (point-min)))
      (goto-char curr)
      (beginning-of-line)
      (setq end-pos
            (if (re-search-forward ess-watch-start-block nil t)
                (match-beginning 0)
              (point-max)))
      (list start-pos end-pos)
      )))

(defun ess-watch-block-number-at-point ()
  "return the current block's order count, 0 if no block was found."
  (save-excursion
    (let ((cur-point (point))
          (count 0))
    (goto-char (point-min))
    (while (re-search-forward ess-watch-start-block cur-point t)
      (setq count (1+ count)))
    count
    )))

(defun ess-watch-set-current (nr)
  "Move the overlay over the block with count NR in current watch buffer"
  (goto-char (point-min))
  (re-search-forward ess-watch-start-expression nil t nr)
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point))
  (setq ess-watch-current-block (ess-watch-block-number-at-point))
  )


(defun ess-watch-make-alist ()
  "Create an association list of expression from current watch buffer.
Each element of assoc list is of the form (pos name expr) where
pos is an unique integer identifying watch blocks by position,
name is a string giving the name of expression block, expr is a
string giving the actual R expression."
  (interactive)
  (save-excursion
    (let* ((reg-name (concat "^" ess-watch-start-block " *\\(\\S-*\\).*$"))
           (reg-expr (concat "^" ess-watch-start-expression "\\s-*\\(.*\\)$"))
           (reg-all (concat "\\(" reg-name "\\)\n\\(" reg-expr "\\)"))
           (pos 0) wal name expr)
      (goto-char (point-min))
      (while (re-search-forward reg-all nil t)
        (setq pos  (+ 1 pos))
        (setq name (match-string-no-properties 2))
        (setq expr (match-string-no-properties 4))
        (if (not (eq (string-to-number name) 0))  ;;if number of any kind set the name to ""
            (setq name ""))
        (setq wal
              (append wal (list (list pos name expr)))
              )
        )
      wal
      )))

(defun ess-watch-parse-assoc (al)
  "Return a string of the form 'assign(\".ess_watch_expressions\", list(a = parse(expr_a), b= parse(expr_b)), envir = .GlobalEnv)'
ready to be send to R process. AL is an association list as return by `ess-watch-make-alist'"
  (setq tte (concat "assign(\".ess_watch_expressions\", list("
          (mapconcat '(lambda (el)
                (if (> (length  (cadr el) ) 0)
                    (concat "`" (cadr el) "` = parse(text = '" (caddr el) "')")
                  (concat "parse(text = '" (caddr el) "')")))
             al ", ")
          "), envir = .GlobalEnv)\n"))
  )

(defun ess-watch-install-.ess_watch_expressions ()
  ;; this is used when ever watches are added/deleted/modified in any fashion.
  ;; there is no other way to insert info into R's .ess_watch_expressions object'
  ;; !! assumes R watch being the current buffer, otherwise will most likely install empty list.
  (interactive)
  (ess-eval-linewise (ess-watch-parse-assoc (ess-watch-make-alist)) t nil nil t)
  )

(defun ess-watch-revert-buffer (ignore noconfirm)
  "Update the watch buffer
Arguments IGNORE and NOCONFIRM currently not used."
  (ess-watch)
  (message "Watch reverted"))

(defun ess-watch-refresh-buffer-visibly ()
  "Eval `ess-watch-command' if `ess-watch-buffer'  exists, else do nothing.
Is much more efficient than `ess-watch-revert-buffer', and don't
switch to the buffer.
If the buffer is not visible call `ess-watch-buffer-show' to make
it visible.
This function is intended for internal use during the debugging."
  (interactive)
  (let ((wbuff (get-buffer ess-watch-buffer)))
    (when wbuff
      (ess-watch-buffer-show wbuff) ;; if visible do nothing
      (with-current-buffer wbuff
        (setq buffer-read-only nil)
        (ess-command  ess-watch-command wbuff))
        ;; delete the ++++++> line
        (goto-char (point-min))
        (delete-region (point-at-bol) (+ 1 (point-at-eol)))
        (ess-watch-set-current ess-watch-current-block)
        (setq buffer-read-only t)
        )
      )
    )
)

(defun ess-watch-buffer-show (buffer-or-name)
  "This is the main function to make watch buffer BUFFER-OR-NAME visible.
If already visible, do nothing.

Currently the only positioning rule implemented si to split the R
process window in half.  The behavior is controlled by
`split-window-sensibly' with parameters `split-height-threshold'
and `split-width-threshold' replaced by
`ess-watch-height-threshold' and `ess-watch-width-threshold'
respectively."
  (interactive)
  (unless (get-buffer-window ess-watch-buffer 'visible)
    (save-selected-window
      (ess-switch-to-ESS t)
      (let* ((split-width-threshold ess-watch-width-threshold)
             (split-height-threshold ess-watch-height-threshold)
             (win (split-window-sensibly (selected-window))))
        (if win
            (set-window-buffer win buffer-or-name)
          (display-buffer buffer-or-name) ;; resort to usual mechanism if could not split
          ))
      ))
  )


(defun ess-watch-quit ()
  "Kill the watch buffer.
If watch buffer exists, it will be displayed during the debug
process. The only way to avoid the display is to kill the
buffer."
  (interactive)
  (kill-buffer)
  )

;;;_ MOTION
(defun ess-watch-next-block (&optional n)
  "Move the overlay over the next block.
Optional N if supplied gives the number of steps forward backward-char."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-end ess-watch-current-block-overlay))
  (if (re-search-forward ess-watch-start-expression nil t n)
      (setq ess-watch-current-block (+ ess-watch-current-block n))
    (goto-char (point-min)) ;;circular but always moves to start!
    (re-search-forward ess-watch-start-expression nil t 1)
    (setq ess-watch-current-block 1)
    )
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point))
  )

(defun ess-watch-previous-block (&optional n)
  "Move the overlay over the previous block.
Optional N if supplied gives the number of backward steps."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-start ess-watch-current-block-overlay))
  (unless (re-search-backward ess-watch-start-expression nil t n)
    (goto-char (point-max)) ;;circular but always moves to last!
    (re-search-backward ess-watch-start-expression nil t 1)
    )
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point))
  (setq ess-watch-current-block (ess-watch-block-number-at-point))
  )

;;;_ BLOCK MANIPULATION and EDITING
(defun ess-watch-rename ()
  "Rename the currently selected watch block. "
  (interactive)
  (end-of-line)
  (unless (re-search-backward ess-watch-start-block nil 1)
    (error "Can not find a watch block"))
  (let ((reg-name (concat ess-watch-start-block " *\\(\\S-*\\).*$"))
        name start end)
    ;; (reg-expr (concat "^" ess-watch-start-expression "\\s-*\\(.*\\)$"))
    ;; (reg-all (concat "\\(" reg-name "\\)\n\\(" reg-expr "\\)"))
    ;; (pos 0) wal name expr)
    (unless (re-search-forward reg-name (point-at-eol) 1)
      (error "Can not find the name substring in the current watch block "))
    (setq name (match-string-no-properties 1))
    (setq start (match-beginning 1))
    (setq end (match-end 1))
    (goto-char start)
    ;; todo: highlight the name in R-watch here
    (setq name (read-string (concat "New name (" name "): ") nil nil name) )
    (setq buffer-read-only nil)
    (delete-region start end)
    (insert name)
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly)
    )
  )

(defun ess-watch-edit-expression ()
  "Edit in the minibuffer the R expression from the current watch block. "
  (interactive)
  (end-of-line)
  (unless (re-search-backward ess-watch-start-block nil 1)
    (error "Can not find a watch block"))
  (let ((reg-expr (concat ess-watch-start-expression " *\\(.*\\)$"))
        expr start end)
    (unless (re-search-forward reg-expr nil 1)
      (error "Can not find an expression string in the watch block"))
    (setq expr (match-string-no-properties 1))
    (setq start (match-beginning 1))
    (setq end (match-end 1))
    (goto-char start)
    ;; todo: highlight the name in R-watch here
    (setq expr (read-string  "New expression: " expr nil expr) )
    (setq buffer-read-only nil)
    (delete-region start end)
    (insert expr)
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly)
    )
  )

(defun ess-watch-add ()
  "Ask for new R expression and name and append it to the end of the list of watch expressions"
  (interactive)
  (let (nr expr name)
    (goto-char (point-max))
    (setq nr (number-to-string (1+ (ess-watch-block-number-at-point))))
    (setq name (read-string (concat "Name (" nr "):") nil nil nr ))
    (setq expr (read-string "New expression: " nil nil "\"Empty watch!\""))
    (setq buffer-read-only nil)
    (insert (concat "\n" ess-watch-start-block " " name " -@\n" ess-watch-start-expression " " expr "\n"))
    (setq buffer-read-only t)
    (setq ess-watch-current-block (ess-watch-block-number-at-point))
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly)
    ))


(defun ess-watch-insert ()
  "Ask for new R expression and name and insert it in front of current watch block"
  (interactive)
  (let (nr expr name)
    (setq nr (number-to-string (ess-watch-block-number-at-point)))
    (re-search-backward ess-watch-start-block nil 1) ;;point-min if not found
    (setq name (read-string (concat "Name (" nr "):") nil nil nr ))
    (setq expr (read-string "New expression: " nil nil "\"Empty watch!\""))
    (setq buffer-read-only nil)
    (insert (concat "\n" ess-watch-start-block " " name " -@\n" ess-watch-start-expression " " expr "\n"))
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly)
    ))


(defun ess-watch-move-up ()
  "Move the current block up."
  (interactive)
  (let ((nr (ess-watch-block-number-at-point))
        wbl)
    (when (> nr 1)
      (setq buffer-read-only nil)
      (setq wbl (apply 'delete-and-extract-region  (ess-watch-block-limits-at-point)))
      (re-search-backward ess-watch-start-block nil t 1) ;; current block was deleted, point is at the end of previous block
      (insert wbl)
      (setq ess-watch-current-block (1- nr))
      (ess-watch-install-.ess_watch_expressions)
      (ess-watch-refresh-buffer-visibly)
      (setq buffer-read-only t)
      )
    )
  )


(defun ess-watch-move-down ()
  "Move the current block down."
  (interactive)
  (let ((nr (ess-watch-block-number-at-point))
        (nr-all (save-excursion (goto-char (point-max))
                                (ess-watch-block-number-at-point)))
        wbl)
    (when (< nr nr-all)
      (setq buffer-read-only nil)
      (setq wbl (apply 'delete-and-extract-region  (ess-watch-block-limits-at-point)))
      (end-of-line)
      (when (re-search-forward ess-watch-start-block nil 1 1) ;; current block was deleted, point is at the end of previous block or point-max
        (goto-char (match-beginning 0)))
      (insert wbl)
      (setq ess-watch-current-block (1+ nr))
      (ess-watch-install-.ess_watch_expressions)
      (ess-watch-refresh-buffer-visibly)
      (setq buffer-read-only t)
      )
    )
  )

(defun ess-watch-kill ()
  "Kill the current block"
  (interactive)
  (setq buffer-read-only nil)
  (apply 'delete-region (ess-watch-block-limits-at-point))
  (ess-watch-install-.ess_watch_expressions)
  (ess-watch-refresh-buffer-visibly)
  )
