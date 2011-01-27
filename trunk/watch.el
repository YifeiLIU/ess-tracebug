
(if ess-watch-mode-map
    ()
  (setq ess-watch-mode-map (make-sparse-keymap))
  (define-key ess-watch-mode-map "?" 'ess-watch-help)
  (define-key ess-watch-mode-map "k" 'ess-watch-kill)
                                        ;  (define-key ess-watch-mode-map "u" 'ess-watch-undelete)
  ;; editing requires a little more work.
  ;; (define-key ess-watch-mode-map "e" 'ess-watch-edit)
  (define-key ess-watch-mode-map "s" 'ess-watch-sort)
  (define-key ess-watch-mode-map "q" 'ess-watch-quit)
  (define-key ess-watch-mode-map " " 'ess-watch-next-block)
  (define-key ess-watch-mode-map [backspace] 'ess-watch-previous-block)
  ;; (define-key ess-watch-mode-map "\C-n" 'ess-watch-next-line)
  ;; (define-key ess-watch-mode-map "\C-p" 'ess-watch-previous-line)
  ;; R mode keybindings.
  (define-key ess-watch-mode-map "\C-c\C-s" 'ess-watch-switch-process)
  (define-key ess-watch-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-watch-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)

  (define-key ess-watch-mode-map [down] 'ess-watch-next-expr)
  (define-key ess-watch-mode-map [up] 'ess-watch-previous-expr)
  (define-key ess-watch-mode-map "g" 'revert-buffer)
  )

(defvar ess-watch-execute
"  if(!exists(\".ess_watch_expressions\") || length(.ess_watch_expressions) == 0){
  assign(\".ess_watch_expressions\", list(Exmaple = expression(\"Empty watch list!\")), envir = globalenv())
  }
  if(!exists(\".ess_watch_list\")){
  .ess_watch_list <- function(){
  .essWEnames <- allNames(.ess_watch_expressions)
  len0p <- !nzchar(.essWEnames)
  .essWEnames[len0p] <- seq_along(len0p)[len0p]
  for(i in seq_along(.ess_watch_expressions)){
  cat(\"\n@---- \", .essWEnames[[i]], \"   \", rep.int(\"-\", max(0, 30 - length(.essWEnames[[i]]))), \"@\n\", sep = \"\")
  cat( paste(\"@--->\", deparse(.ess_watch_expressions[[i]][[1]])), \" \n\", sep = \"\")
  tryCatch(print(eval(.ess_watch_expressions[[i]])),
                error = function(e) cat(\"Error:\", e$message, \"\n\" ),
                warning = function(w) cat(\"warning: \", w$message, \"\n\"))
  }
  }
  }
  .ess_watch_list()
"
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

(defvar ess-watch-mode-map nil
  "Keymap for the *R watch* buffer.")

(defvar ess-watch-start-block "@----"
  "String indicating the beginning of a block in watch buffer")
(defvar ess-watch-start-expression "@--->"
  "String indicating the beginning of an R expression in watch buffer")

(defun ess-watch-block-at-point ()
  "return start and end positions of the watch block."
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

(defun ess-watch-set-current (nr)
  "Move the overlay over the block with count NR in current watch buffer"
  (goto-char (point-min))
  (re-search-forward ess-watch-start-expression nil t nr)
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-at-point))
  )

(defun ess-watch-next-block (&optional n)
  "Move the overlay over the next block.
Optional N if supplied gives the number of steps forward backward-char"
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-end ess-watch-current-block-overlay))
  (unless (re-search-forward ess-watch-start-expression nil t n)
    (goto-char (point-min)) ;;circular but always moves to start!
    (re-search-forward ess-watch-start-expression nil t n)
    )
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-at-point))
  )

(defun ess-watch-previous-block (&optional n)
  "Move the overlay over the previous block.
Optional N if supplied gives the number of backward steps."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-start ess-watch-current-block-overlay))
  (unless (re-search-backward ess-watch-start-expression nil t n)
    (goto-char (point-max)) ;;circular but always moves to start!
    (re-search-backward ess-watch-start-expression nil t n)
    )
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-at-point))
  )


(defun ess-watch-mode ()
  "Major mode for output from `ess-rdired'.
`ess-rdired' provides a dired-like mode for R objects.  It shows the
list of current objects in the current environment, one-per-line.  You
can then examine these objects, plot them, and so on.
\\{ess-rdired-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'ess-watch-revert-buffer)
  (use-local-map ess-watch-mode-map)
  (setq major-mode 'ess-watch-mode)
  (setq ess-watch-current-block 1)
  (unless (local-variable-p ess-watch-current-block-overlay)
    (setq ess-watch-current-block-overlay
          (make-overlay (point-min) (point-max))))
  (overlay-put ess-watch-current-block-overlay
               'face  'ess-watch-current-block-face)
  (setq mode-name (concat "watch " ess-local-process-name))
  )

(defun ess-watch ()
  "Run ess-watch mode on R objects.
This is the main function.  See documentation for `ess-watch-mode' though
for more information!"
  (interactive)
  (pop-to-buffer ess-watch-buffer)
  (set-buffer ess-watch-buffer)
  (setq buffer-read-only nil)
  (ess-execute ess-watch-execute nil
               (substring ess-watch-buffer 1 (- (length ess-watch-buffer) 1)))
  ;; When definiting the function .watch.objects(), a "+ " is printed on the first line.
  ;; these are deleted:
  (goto-char (point-min))
  (delete-region (point-at-bol) (+ 1 (point-at-eol)))
  (ess-watch-mode)
  (setq buffer-read-only t)
  )

(defun ess-watch-assoc ()
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
  "Get an association list as return by `ess-watch-assoc' and return a string
of the form 'assign(\".ess_watch_list\", list(a = parse(expr_a), b= parse(expr_b)), envir = .GlobalEnv)' ready to be
send to R process."
  (setq tte (concat "assign(\".ess_watch_list\", list("
          (mapconcat '(lambda (el)
                (if (> (length  (cadr el) ) 0)
                    (concat "`" (cadr el) "` = parse(text = \"" (caddr el) "\")")
                  (concat "parse(text = \"" (caddr el) "\")")))
             al ", ")
          "), envir = .GlobalEnv)\n"))
  )
(defun ess-watch-install-.ess_watch_list ()
  ;; this is used when ever watches are added/deleted/modified in any fashion.
  ;; there is no other way to insert info into R's .ess_watch_list object'
  ;; !! assumes R watch being the current buffer, otherwise will most likely install empty list.
  (interactive)
  (ess-command (ess-watch-parse-assoc (ess-watch-assoc)))
  )

(defun ess-watch-revert-buffer (ignore noconfirm)
  "Update the watch buffer
Arguments IGNORE and NOCONFIRM currently not used."
  (ess-watch))

(defun ess-watch-refresh-buffer ()
  "Must be more efficient than `ess-watch-revert-buffer', and to not switch to the buffer.
To be used during the debugging."
  (ess-watch))
