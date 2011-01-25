
(defvar ess-watch-execute
  "if(!exists(\".ess_watch_expressions\")){
  cat(\"Can not find watch expressions. \nHave you defined a watch?\")
  }else{
  if(!exists(\".ess_watch_execute\")){
  .ess_watch_execute <- function(){
  .essWEnames <- allNames(.ess_watch_expressions)
  len0p <- !nzchar(.essWEnames)
  .essWEnames[len0p] <- seq_along(len0p)[len0p]
  for(i in seq_along(.ess_watch_expressions)){
  cat(\"\n@-- \", .essWEnames[[i]], \" \", rep.int(\"-\", max(0, 30 - length(.essWEnames[[i]]))), \"@\n\", sep = \"\")
  cat( paste(\"@->\", deparse(.ess_watch_expressions[[i]][[1]])), \" \n\", sep = \"\")
  tryCatch(print(eval(.ess_watch_expressions[[i]])),
                error = function(e) cat(\"Error:\", e$message, \"\n\" ),
                warning = function(w) cat(\"warning: \", w$message, \"\n\")) } } }
  .ess_watch_execute()
  }"
  )

(defvar ess-watch-buffer "*R watch*"
  "Name of the watch buffer.")

(defvar ess-watch-mode-map nil
  "Keymap for the *R watch* buffer.")

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
  (define-key ess-watch-mode-map " " 'ess-watch-next-expr)
  (define-key ess-watch-mode-map [backspace] 'ess-watch-previous-expr)
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
  (setq mode-name (concat "watch " ess-local-process-name)))



(defun ess-watch ()
  "Run ess-watch mode on R objects.
This is the main function.  See documentation for `ess-watch-mode' though
for more information!"
  (interactive)
  (when (get-buffer ess-watch-buffer)
	(set-buffer ess-watch-buffer)
	(setq buffer-read-only nil))
  (ess-execute ess-watch-execute nil
               (substring ess-watch-buffer 1 (- (length ess-watch-buffer) 1)))
  (pop-to-buffer ess-watch-buffer)
  ;; When definiting the function .watch.objects(), a "+ " is printed on the first line.
  ;; these are deleted:
  (goto-char (point-min))
  (delete-region (point-at-bol) (+ 1 (point-at-eol)))
  (setq buffer-read-only t)
  (ess-watch-mode)
  )
