;;; ess-tracebug.el --- Tracing and debugging facilities for ESS.
;;
;; Filename: ess-tracebug.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2010, Spinu Vitalie, all rights reserved.
;; Created: Oct  14 14:15:22 2010
;; Version: 0.1a
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Keywords: debug, traceback, ESS, R
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Features that might be required by this library:
;;
;;   ESS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defgroup ess-tracebug nil
  "Traceback and debug facilities for ess (currently tested only with R)."
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess
  )

;;;_* TRACEBACK

(require 'compile)
(defgroup ess-traceback nil
  "Tracing facilities for ess."
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess-tracebug
  :prefix "ess-tb-"
  )


(defface ess-tb-last-input-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:overline "medium blue" ))
    (((class color)
      (background dark))  (:overline "deep sky blue" ))
    )
  "Face to highlight currently debugged line."
  :group 'ess-traceback )

(defface ess-tb-last-input-fringe-face
  '((((background light)) (:foreground "medium blue" :overline "medium blue"))
    (((background dark))  (:foreground "deep sky blue" :overline "deep sky blue"))
    )
  "Face for fringe bitmap for last-input position."
  :group 'ess-traceback)

(define-fringe-bitmap 'last-input-arrow
  [#b00011111
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b11010111
   #b01111100
   #b00111000
   #b00010000] nil nil 'top)

(defvar ess-tb-last-input (make-marker)
  "Marker pointing to the last user input position in iESS buffer.
This is the place where `ess-tb-last-input-overlay' is moved.
Local in iESS buffers with `ess-traceback' mode enabled.")
(defcustom inferior-ess-split-long-prompt t
  "If non-nil, long prompt '>>>>>>>>++++> ' will be split."
  :group 'ess-traceback)


;; (defvar  ess-traceback-minor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mouse-2] 'compile-goto-error)
;;     (define-key map [follow-link] 'mouse-face)
;;     (define-key map "\M-]" 'next-error-no-select)
;;     (define-key map "\M-[" 'previous-error-no-select)
;;     map)
;;   "Keymap for `ess-traceback-minor-mode'.")


(defmacro ess-copy-key (from-map to-map fun)
  `(define-key ,to-map
     (car (where-is-internal ,fun  ,from-map))
     ,fun
     ))

;;;_ + traceback functions
(defun ess-tb-make-last-input-overlay (beg end)
  "Create an overlay to indicate the last input position."
  (let   ((ove (make-overlay beg end)))
    (overlay-put ove 'before-string
                 (propertize "*last-input-start*"
                             'display (list 'left-fringe 'last-input-arrow 'ess-tb-last-input-fringe-face)))
    (overlay-put ove 'face  'ess-tb-last-input-face)
    (overlay-put ove 'evaporate t)
    ove
    )
  )

(defun test (&optional arg)
  (interactive "P")
  (print (> (prefix-numeric-value arg) 0)))

(defun ess-traceback (&optional arg)
  "Toggle ess-traceback mode.
With ARG, turn ess-traceback mode on in the current iESS buffer,
if and only if ARG is positive.  All the error-parsing commands
of the Compilation major mode are available in iESS
buffers. Particularly the keys \\[next-error] and
\\[previous-error] could be used to navigate to next/previous
errors.  See `compilation-mode'."
  (interactive "P")
  (if arg
      (setq arg (prefix-numeric-value arg)))
  (ess-force-buffer-current "R process to activate the traceback-mode: ")
  (with-current-buffer (process-buffer (get-process ess-local-process-name))
    (unless arg
      (setq arg (if ess-traceback-p -1 1)))
    (if (> arg 0)
        ;; activate the mode
        (progn
          (make-local-variable 'compilation-error-regexp-alist)
          (setq compilation-error-regexp-alist ess-R-tb-regexp-alist)
          (compilation-setup t)
          (setq next-error-function 'ess-tb-next-error-function)
          (make-local-variable 'ess-tb-last-input)
          (make-local-variable 'ess-tb-last-input-overlay)
          (save-excursion
            (goto-char comint-last-input-start)
          (setq ess-tb-last-input (point))
          (setq ess-tb-last-input-overlay (ess-tb-make-last-input-overlay  (point-at-bol) (max (1+ (point)) (point-at-eol))))
          )
          (ad-activate 'ess-eval-region)
          ;; (ad-activate 'ess-eval-linewise)
          (ad-activate 'inferior-ess-send-input)
          (setq ess-traceback-p t)
          (message "ESS-traceback mode enabled")
          )
      ;; else deactivate
      (ad-deactivate 'inferior-ess-send-input)
      (ad-deactivate 'ess-eval-region)
      ;; (ad-deactivate 'ess-eval-linewise)
      (delete-overlay ess-tb-last-input-overlay)
      (kill-local-variable 'ess-tb-last-input-overlay)
      (kill-local-variable 'ess-tb-last-input)
      (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
      (font-lock-fontify-buffer)
      (kill-local-variable 'compilation-error-regexp-alist)
      (setq ess-traceback-p nil)
      (message "ESS-traceback mode desabled")
      )
    )
)

(defvar ess-R-tb-regexp-alist '(R R3 R-recover)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(R "^.* \\(at \\(.+\\)#\\([0-9]+\\)\\)"  2 3 nil 2 1))
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(R2 "\\(?:^ +\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\)"  1 2 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(R3 "\\(?:Error .*: +\n? +\\)\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):"  1 2 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(R-recover "^ *[0-9]+: +\\(.+\\)#\\([0-9]+:\\)" 1 2 nil 2 1))

;; (setq ess-R-tb-regexp-alist '(R R2 R3 R-recover))
;; (pop compilation-error-regexp-alist-alist)

(defun ess-show-R-traceback ()
  "Display R traceback and last error message.
Pop up a compilation/grep/occur like buffer. Usual global key
bindings are available \(\\[next-error] and \\[previous-error]\)
for `next-error' and `previous-error' respectively.

You can bound 'no-select' versions of this commands  for convenience:
\(define-key compilation-minor-mode-map [(?n)] 'next-error-no-select\)
\(define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select\)
"
  (interactive)
  ;; (when (or ess-current-process-name
  ;;           (interactive-p)))
  (ess-force-buffer-current "R process to use: ")
  (let ((trbuf  (get-buffer-create "*ess-traceback*")))
    (setq next-error-last-buffer trbuf) ;; need to avoid
    (set-buffer trbuf)
    (toggle-read-only -1)
    (ess-command  "try(traceback(), silent=TRUE);cat(\n\"---------------------------------- \n\", geterrmessage(), fill=TRUE)\n" trbuf)
    (if (string= "No traceback available" (buffer-substring 1 23))
        (message "No traceback available")
      (ess-dirs)
      (goto-char (point-min))
                                        ;(setq font-lock-defaults '(ess-R-mode-font-lock-keywords)) :todo: solve font-lock
      (make-local-variable 'compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist ess-R-tb-regexp-alist)
      (compilation-minor-mode 1)
      ;(use-local-map ess-traceback-minor-mode-map)
      (pop-to-buffer trbuf)
      (toggle-read-only 1)
      )
    )
  )

(defun ess-tb-next-error-goto-process-marker ()
  ;; assumes current buffer is the process buffer with compilation enabled
  ;; used in ess-tb-next-error-function
;  (with-current-buffer (process-buffer (get-process ess-local-process-name)) ; already in comint buffer .. no need
    (comint-goto-process-mark)
    (set-window-point (get-buffer-window) (point))  ;moves the cursor
    ;; FIXME: Should jump to current-debug-position,  but will mess the things if in recover
    ;; (when (ess-dbg-is-active)
    ;;   (ess-dbg-goto-current-debug-position)
    ;;   )
)

(defun ess-tb-next-error-function (n &optional reset)
    "Advance to the next error message and visits the file.
This is the value of `next-error-function' in iESS buffers."
;; Modified version of `compilation-next-error-function'.
    (interactive "p")
    (if reset  (goto-char (point-max)))
    (let* ((columns compilation-error-screen-columns) ; buffer's local value
           ;; (proc (or (get-buffer-process (current-buffer))
           ;;                         (error "Current buffer has no process")))
           (last 1) timestamp
           (n (or n 1))
           (beg-pos  ; from where the search for next error starts
            (if (and (>= n 0)
                     (comint-after-pmark-p))
                ess-tb-last-input
              (point)
              )
            )
           (loc (condition-case err
                    (compilation-next-error n  nil beg-pos)
                  (error
                   (ess-tb-next-error-goto-process-marker)
                   (error "Passed beyond last reference");(error-message-string err))
                   )))
           (loc (if (or (eq n 0)
                        (> (point) ess-tb-last-input))
                    loc
                  (ess-tb-next-error-goto-process-marker)
                  (error "Passed beyond last-input marker")))
           (end-loc (nth 2 loc))
           (marker (point-marker))
           )
      (setq compilation-current-error (point-marker)
            overlay-arrow-position (if (bolp)
                                       compilation-current-error
                                     (copy-marker (line-beginning-position)))
            loc (car loc))
      ;; If loc contains no marker, no error in that file has been visited.
      ;; If the marker is invalid the buffer has been killed.
      ;; If the file is newer than the timestamp, it has been modified
      ;; (`omake -P' polls filesystem for changes and recompiles when needed
      ;;  in the same process and buffer).
      ;; So, recalculate all markers for that file.
      (unless (and (nth 3 loc) (marker-buffer (nth 3 loc))
                   ;; There may be no timestamp info if the loc is a `fake-loc'.
                   ;; So we skip the time-check here, although we should maybe
                   ;; change `compilation-fake-loc' to add timestamp info.
                   (nth 4 loc)          ;++
                                        ; VS: here  4th loc is always nil,  and changes are not recoreded
                                        ; can't figure out when the markers (3rd loc) and timestamps (4th loc) are set
                                        ; so recalculate if 4th loc is nil.
                   (or (null (nth 4 loc))
                       (equal (nth 4 loc)
                              (setq timestamp
                                    (with-current-buffer
                                        (marker-buffer (nth 3 loc))
                                      (visited-file-modtime))))))
        (with-current-buffer
            (compilation-find-file marker (caar (nth 2 loc))
                                   (cadr (car (nth 2 loc))))
          (save-restriction
            (widen)
            (goto-char (point-min))
            ;; Treat file's found lines in forward order, 1 by 1.
            (dolist (line (reverse (cddr (nth 2 loc))))
              (when (car line)		; else this is a filename w/o a line#
                (beginning-of-line (- (car line) last -1))
                (setq last (car line)))
              ;; Treat line's found columns and store/update a marker for each.
              (dolist (col (cdr line))
                (if (car col)
                    (if (eq (car col) -1)	; special case for range end
                        (end-of-line)
                      (compilation-move-to-column (car col) columns))
                  (beginning-of-line)
                  (skip-chars-forward " \t"))
                (if (nth 3 col)
                    (set-marker (nth 3 col) (point))
                  (setcdr (nthcdr 2 col) `(,(point-marker)))))))
          ))
      (compilation-goto-locus marker (nth 3 loc) (nth 3 end-loc))
      (setcdr (nthcdr 3 loc) (list timestamp))
      (setcdr (nthcdr 4 loc) t))
    )

(print "<- traceback done")

;;;_ + ADVICING
;;; (needed to implement the last user input functionality)
;;; redefining eval-region is needed to avoid messing the debugger.
;;; New version flushes all blank lines and trailing \n's.
(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior ESS process.
With prefix argument toggle the meaning of `ess-eval-visibly-p';
this does not apply when using the S-plus GUI, see `ess-eval-region-ddeclient'."
  (interactive "r\nP")
  ;;(untabify (point-min) (point-max))
  ;;(untabify start end); do we really need to save-excursion?
  (ess-force-buffer-current "Process to load into: ")
  (message "Starting evaluation...")

  (if (ess-ddeclient-p)
      (ess-eval-region-ddeclient start end 'even-empty)
    ;; else: "normal", non-DDE behavior:
    (let ((visibly (if toggle (not ess-eval-visibly-p) ess-eval-visibly-p))
          (string (buffer-substring-no-properties start end)))
      (setq string (replace-regexp-in-string
                    "\\(\n+\\s *\\'\\)\\|\\(^\\s *\n\\)" "" string))  ;; delete empty lines and trailing \ns
      (if visibly
	  (ess-eval-linewise string)
	(if ess-synchronize-evals
	    (ess-eval-linewise string
			       (or message "Eval region"))
	  ;; else [almost always!]
	  (let ((sprocess (get-ess-process ess-current-process-name)))
	    (process-send-string sprocess (concat string "\n")))))))

  (message "Finished evaluation")
  (if (and (fboundp 'deactivate-mark) ess-eval-deactivate-mark)
      (deactivate-mark))
  ;; return value
  (list start end)
  )

;; this advice is probably not very useful. :TOTHINK:
;; (defadvice ess-eval-linewise (around move-last-input)
;;   "Move the `ess-tb-last-input' marker and
;;   `ess-tb-last-input-overlay' to apropriate positions.'"
;;   (ess-force-buffer-current "Process to load into: ")
;;   (let* ((last-input-process (get-process ess-local-process-name))
;;          (last-input-mark (copy-marker (process-mark last-input-process))))
;;     ad-do-it
;;     (with-current-buffer (process-buffer last-input-process)
;;       (when (local-variable-p 'ess-tb-last-input)
;;           (setq ess-tb-last-input last-input-mark)
;;           (goto-char last-input-mark)
;;           (move-overlay ess-tb-last-input-overlay (point-at-bol) (point))
;;           )
;;       )
;;     )
;;   )

                                        ;(ad-activate 'ess-eval-linewise)
                                        ;(ad-unadvise 'ess-eval-linewise)

(defun inferior-ess-move-last-input-overlay ()
  (let ((pbol (point-at-bol))
        (pt (point)) )
    (move-overlay ess-tb-last-input-overlay pbol (max (- pt 2) (+ pbol 2)))
    )
  )

(defadvice  ess-eval-region (around move-last-input)
  "Move the `ess-tb-last-input' marker and
  `ess-tb-last-input-overlay' to apropriate positions.'"
  (ess-force-buffer-current "Process to load into: ")
  (let* ((last-input-process (get-process ess-local-process-name))
         (last-input-mark (copy-marker (process-mark last-input-process))))
    ad-do-it
    (with-current-buffer (process-buffer last-input-process)
      (when (local-variable-p 'ess-tb-last-input) ;; TB might not be active in all processes
        (setq ess-tb-last-input last-input-mark)
        (goto-char last-input-mark)
        (inferior-ess-move-last-input-overlay)
        (comint-goto-process-mark)
        )
      (when (and inferior-ess-split-long-prompt
                 (> (current-column) 2)
                 (looking-back "> "))
        (backward-char 2)
        (insert "> \n")
        )
      )
    )
  )

                                        ; (ad-activate 'ess-eval-region)
                                        ; (ad-unadvise 'ess-eval-region)

(defadvice inferior-ess-send-input (after move-last-input)
  "Move the `ess-tb-last-input' marker and
  `ess-tb-last-input-overlay' to apropriate positions.'"
  (save-excursion
    (goto-char comint-last-input-start)
     (setq ess-tb-last-input (point))
    (inferior-ess-move-last-input-overlay)
    )
  )

;;ad-activate 'inferior-ess-send-input)
;;(ad-deactivate 'inferior-ess-send-input)


(print "<- advising done")


;;;_* DEBUGER

(defgroup ess-debug nil
  "Debug facilities for ess (currently tested only with R)."
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess-tracebug
  :prefix "ess-dbg-"
  )


(defcustom ess-dbg-error-action "-"
  "Mode line indicator of the current \"on error\" action.
Customize this variable to change the default behavior.
See `ess-dbg-error-action-alist' for more."
  :type 'string
  :group 'ess-debug)

(defcustom  ess-dbg-error-action-alist
  '(( "-". "NULL" )
    ( "r". "utils::recover")
    ( "t". "base::traceback"))
  "Alist of 'on-error' actions.
  Each element must have the form (SYM . ACTION) where SYM is the
  string to be displayed in the mode line when the action is in
  place and ACTION is the string giving the actual expression to
  be assigned to 'error' user option. See R's help ?options for
  more details."
  :type '(alist :key-type string :value-type string)
  :group 'ess-debug)


(defvar ess-dbg-output-buf-prefix "*ess.dbg"
  "The prefix of the buffer name the R debug output is directed to."  )

(defvar ess-dbg-current-ref (make-marker)
  "Current debug reference in *ess.dbg* buffers (a marker).")
(make-variable-buffer-local 'ess-dbg-current-ref)

(defvar ess-dbg-last-ref-marker (make-marker)
  "Last debug reference in *ess.dbg* buffers (a marker).")

;; (defvar ess-dbg-is-active nil
;;   "Indicates whether the debuger is active, i.e. is in the
;;   browser, debuger or alike R session. This variable could be
;;   toggled by [\M-c t]. Might be useful if you want to iterate
;;   manually at R's prompt and debugger's active line not to jump
;;   around.")

(defcustom ess-dbg-search-path '(nil)
  "List of directories to search for source files
given by debug/browse references.
Elements should be directory names, not file names of directories.
"
  :type '(repeat (choice (const :tag "Default" nil)
			 (string :tag "Directory")))
  :group 'ess-debug)


(defvar ess-dbg-buf-p nil
  "This is t in ess.dbg buffers.")
(make-variable-buffer-local 'ess-dbg-buf-p)


(defvar ess-dbg-current-debug-position (make-marker)
  "Marker to the current debugged line.
 It always point to the
beginning of the currently debugged line and is used by  overlay-arrow.")
(add-to-list 'overlay-arrow-variable-list 'ess-dbg-current-debug-position)

(defface ess-dbg-current-debug-line-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "tan"))
    (((class color)
      (background dark))  (:background "gray20"))
    )
  "Face used to highlight currently debugged line."
  :group 'ess-debug
  )


(defvar  ess-dbg-current-debug-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'ess-dbg-current-debug-line-face)
    (overlay-put overlay 'evaporate t)
    overlay
    )
  "The overlay for currently debugged line.")


(defcustom ess-dbg-blink-interval .2
  "Time in seconds to blink the background
 of the current debug line on exceptional events.
 Currently two exceptional events are defined 'ref-not-found'
 and 'same-ref'. Blinking colors for these events can be
 customized by corresponding faces."
  :group 'ess-debug
  :type 'float)

(defface ess-dbg-blink-ref-not-found-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "IndianRed4"))
    (((class color)
      (background dark))  (:background "dark red"))
    )
  "Face used to blink currently debugged line's background
 when the reference file is not found. See also `ess-dbg-ask-for-file'"
  :group 'ess-debug )

(defface ess-dbg-blink-same-ref-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "steel blue"))
    (((class color)
      (background dark))  (:background "midnight blue"))
    )
  "Face used to highlight currently debugged line when new debug
reference is the same as the preceding one. It is highlighted for
`ess-dbg-blink-interval' seconds."
  :group 'ess-debug )


(defvar ess-dbg-active-p nil
  "t  if the process is in active debug state.

Local in iESS buffers with active `ess-debug' mode.
Active debug states are usually those, in which prompt start with Browser[d]> .")
(make-variable-buffer-local 'ess-dbg-active-p)

(defcustom ess-dbg-indicator "db"
  "String to be displayed in mode-line alongside the process
  name. Indicates that ess-debug-mode is turned on. When the
  debuger is in active state this string is showed in upper case
  and highlighted."
  :group 'ess-debug
  :type 'string)


(defcustom ess-tb-indicator " TB"
  "String to be displayed in mode-line alongside the process
  name. Indicates that ess-traceback-mode is turned on. "
  :group 'ess-debug
  :type 'string)

(defvar ess-traceback-p nil
  "Non nil if traceback is active.
Use ess-traceback function to toggle this variable")
(make-variable-buffer-local 'ess-traceback-p)
(add-to-list 'minor-mode-alist '(ess-traceback-p ess-tb-indicator))

(defcustom ess-dbg-ask-for-file nil
  "If non nil, ask for file
if the current debug reference is not find. If nil, the currently
debugged line will be highlighted for `ess-dbg-blink-interval'
seconds."
  :group 'ess-debug
  :type 'boolean)

(defvar ess-dbg-buffer nil
  "Local in every iESS buffer with the debuger enabled. It's
  value is the associate *ess.dbg* buffer.")
(make-variable-buffer-local 'ess-dbg-buffer)

(defcustom ess-debug-command-prefix "\M-c"
  "*Key to be used as prefix in ess-debug command key bindings.

The postfix keys are defined in `ess-debug-map'.
The overwritten binding is `capitalize-word' and is bound to 'M-c M-c'.
You can set this to \"M-t\" for example,  which would rebind the
default binding `transpose-words'. In this case make sure to
rebind `M-t` to transpose-words command in the `ess-debug-map'."
  :type 'string
  :group 'ess-debug)

;(makunbound 'ess-debug-map)
(defvar ess-debug-map
  (let ((map (make-sparse-keymap)))
    (define-prefix-command 'map)
    (define-key map "i" 'ess-dbg-goto-input-point)
    (define-key map "I" 'ess-dbg-insert-in-input-ring)
    (define-key map "d" 'ess-dbg-goto-debug-point)
    (define-key map "b" 'ess-bp-set)
    (define-key map "t" 'ess-bp-toggle-state)
    (define-key map "k" 'ess-bp-kill)
    (define-key map "K" 'ess-bp-kill-all)
    (define-key map "\C-n" 'ess-bp-next)
    (define-key map "\C-p" 'ess-bp-previous)
    (define-key map "e" 'ess-dbg-toggle-error-action)
    (define-key map "c" 'ess-dbg-easy-command)
    (define-key map "n" 'ess-dbg-easy-command)
    (define-key map "p" 'ess-dbg-easy-command)
    (define-key map "q" 'ess-dbg-easy-command)
    (define-key map "u" 'ess-dbg-easy-command)
    (define-key map "s" 'ess-dbg-source-curent-file)
    (define-key map "\M-c" 'capitalize-word)
    map)
  "Keymap used as a binding for `ess-debug-command-prefix' key
 in ESS and iESSmode."
  )

(defvar ess-debug-easy-map
  (let ((map (make-sparse-keymap)))
    (define-prefix-command 'map)
    (define-key map "c" 'ess-dbg-command-c)
    (define-key map "n" 'ess-dbg-command-n)
    (define-key map "p" 'previous-error)
    (define-key map "q" 'ess-dbg-command-Q)
    (define-key map "u" 'ess-dbg-command-u)
    map)
  "Keymap used to define commands for easy input mode.
This commands are triggered by `ess-dbg-easy-command' ."
  )


(defvar ess-dbg-input-ring (make-ring 10)
  "Ring of markers to the positions of last user inputs
 when the  debugger has started.  It is used in
 `ess-dbg-goto-input-point'.")

(defvar ess-dbg-debug-ring (make-ring 10)
  "Ring of markers to the last debugging positions.

Last debugging positions are those from where
`ess-dbg-goto-input-point' was called. See the documentation for
`ess-dbg-goto-debug-point'")

(print "<- debug-vars done")

;;;_ + debug functions
(defun ess-dbg-set-error-action (spec)
  "Set the on-error action. The ACTION should be  one
of components of `ess-dbg-error-action-alist' (a cons!)."
  (let ( (proc (get-process ess-current-process-name)))
    (if spec
        (progn
          (setq ess-dbg-error-action (car spec))
          (send-string proc (concat "options(error=" (cdr spec) ")\n"))
          )
      (error "Unknown action.")
      )
    )
  )



(defun ess-dbg-toggle-error-action ()
  "Toggle the 'on-error' action.
The list of actions are specified in `ess-dbg-error-action-alist'."
  (interactive)
  (let* ( (alist ess-dbg-error-action-alist)
          (ev last-command-event)
          (com-char  (event-basic-type ev))
          actions
          )
    (setq actions (cdr (member (assoc ess-dbg-error-action ess-dbg-error-action-alist)
                               ess-dbg-error-action-alist)))
    (unless actions
      (setq actions ess-dbg-error-action-alist))
    (ess-dbg-set-error-action (pop actions))
    (while  (eq (setq ev (read-event)) com-char)
      (unless actions
        (setq actions ess-dbg-error-action-alist))
      (ess-dbg-set-error-action (pop actions))
      )
    (push ev unread-command-events)
    )
  )

(defun ess-dbg-activate-overlays ()
  "Initialize active debug line."
  (move-overlay ess-dbg-current-debug-overlay (point-at-bol) (1+ (point-at-eol)) (current-buffer))
  (move-marker ess-dbg-current-debug-position (point-at-bol)) ;; used by overlay-arrow,  should be bol
  )

(defun ess-dbg-deactivate-overlays ()
  "Deletes markers and overlays. Overlay arrow stays, indicating the last debug position."
  (delete-overlay ess-dbg-current-debug-overlay)
  ;; overlay-arrow stays, to indicate the last debugged position!!
  )

(defun ess-dbg-goto-input-point ()
  "Jump to the last input point.

   This is useful during/after debugging, to return to the place
from where the code was executed.  This is an easy-command. "
  (interactive)
  (let* ((input-point (ring-ref ess-dbg-input-ring 0))
        (ev last-command-event)
        (com-char  (event-basic-type ev))
        (ring-el 0))
    (ring-insert ess-dbg-debug-ring (point-marker))
    (switch-to-buffer (marker-buffer input-point))
    (goto-char (marker-position input-point))
    (while  (eq (event-basic-type (setq ev (read-event))) com-char)
      (if (memq 'shift (event-modifiers ev))
          (setq ring-el (1- ring-el))
        (setq ring-el (1+ ring-el))
        )
      (setq input-point (ring-ref ess-dbg-input-ring ring-el)) ;
      (switch-to-buffer (marker-buffer input-point))
      (goto-char (marker-position input-point))
      )
    (push ev unread-command-events)
    )
  )

(defun ess-dbg-goto-debug-point ()
  "Returns to the debugging position.
Jump to markers stored in `ess-dbg-debug-ring'. If
debug session is active, first jump to current debug line.

This is an easy-command. Shift triggers the opposite traverse
of the ring."
  (interactive)
  (let* ((debug-point (ring-ref ess-dbg-debug-ring 0))
        (ev last-command-event)
        (com-char  (event-basic-type ev))
        (ring-el 0))
    (if (ess-dbg-is-active)
        (progn
          (switch-to-buffer (marker-buffer ess-dbg-current-debug-position))
          (goto-char (marker-position ess-dbg-current-debug-position ))
          (back-to-indentation)
          )
      (switch-to-buffer (marker-buffer debug-point))
      (goto-char (marker-position debug-point))
      )
    (while  (eq (event-basic-type (setq ev (read-event))) com-char)
      (if (memq 'shift (event-modifiers ev))
          (setq ring-el (1- ring-el))
        (setq ring-el (1+ ring-el))
        )
      (setq debug-point (ring-ref ess-dbg-debug-ring ring-el))
      (switch-to-buffer (marker-buffer debug-point))
      (goto-char (marker-position debug-point))
      )
    (push ev unread-command-events)
    )
  )

(defun ess-dbg-insert-in-input-ring ()
  (interactive)
  "Inserts point-marker into the input-ring."
  (ring-insert ess-dbg-input-ring (point-marker))
  (message "Point inserted into the input-ring")
)

(defun ess-debug (&optional arg)
  "Toggle ess-debug mode.
With arg, turn ess-debug mode on if and only if arg is positive.
This mode adds to ESS the interactive debugging and breakpoints.
Strictly speaking ess-debug is not a minor mode. It integrates
into ESS and iESS modes, providing global functionality.
"
  (interactive)
  (ess-force-buffer-current "Process to activate/deactivate the debuger: ")
  (if (number-or-marker-p arg)
      (if (>= arg 0)
          (ess-dbg-start-session)
        (ess-dbg-end-session)
        )
    (if (local-variable-p 'ess-dbg-buffer (process-buffer (get-process ess-current-process-name)))
        (ess-dbg-end-session)
      (ess-dbg-start-session)
      )))

(defun ess-dbg-start-session ()
  "Start the debug session."
  (interactive)
  (let ((dbuff (get-buffer-create (concat ess-dbg-output-buf-prefix "." ess-current-process-name "*")))
        (proc (get-process ess-current-process-name))
        (inhibit-read-only t))
    (with-current-buffer (process-buffer proc)
      (if (member ess-dialect '("XLS" "SAS" "STA"))
        (error "Can not activate the debuger for %s dialect" ess-dialect)
        )
      (setq ess-dbg-buffer dbuff)
      (setq ess-dbg-active-p nil)
      (setq mode-line-process '(" ["
                                ess-local-process-name
                                " "
                                (:eval (if ess-dbg-active-p
                                           (propertize  (upcase ess-dbg-indicator) 'face '(:foreground "white" :background "red"))
                                         ess-dbg-indicator)
                                       )
                                " "
                                ess-dbg-error-action
                                "]: %s"))
      )
    (with-current-buffer dbuff
      (buffer-disable-undo)
      (make-local-variable 'overlay-arrow-position) ;; indicator for next-error functionality in the *ess.dbg*,  useful??
      (goto-char (point-max))
      (setq ess-dbg-buf-p t  ;; true if in  *ess.dbg* buffer
            ess-dbg-current-ref (point-marker)  ;; used by goto-error functionality
            ess-dbg-last-ref-marker (point-marker)  ;; gives the last debugged line
          xb)
      (beginning-of-line)
      (set-process-filter proc 'inferior-ess-dbg-output-filter)
      (toggle-read-only t)
      (message "Started debug session successfully")
      )
    (define-key ess-mode-map ess-debug-command-prefix ess-debug-map)
    (define-key inferior-ess-mode-map ess-debug-command-prefix ess-debug-map)
    )
  )

(defun ess-dbg-end-session ()
  "End the debug session.
Kill the *ess.dbg.[R_name]* buffer."
  (interactive)
  (let ((proc (get-process ess-current-process-name)))
    (with-current-buffer (process-buffer proc)
      (if (member ess-dialect '("XLS" "SAS" "STA"))
          (error "Can not deactivate the debuger for %s dialect" ess-dialect)
        )
      (when (buffer-live-p ess-dbg-buffer)
        (with-current-buffer ess-dbg-buffer
          (set-buffer-modified-p nil)
          )
        (kill-buffer ess-dbg-buffer)
        )
      (kill-local-variable 'ess-dbg-buffer)
      (kill-local-variable 'ess-dbg-active-p)
      (set-process-filter proc 'inferior-ess-output-filter)
      (setq mode-line-process '(" [" ess-local-process-name "] %s"))
      (message "Finished debug session")
      )
    )
  (define-key ess-mode-map ess-debug-command-prefix nil)
  (define-key inferior-ess-mode-map ess-debug-command-prefix nil)
  )


(defun ess-dbg-is-active ()
  "Return t if the current R process is in active debugging state."
  (buffer-local-value 'ess-dbg-active-p (process-buffer (get-process ess-current-process-name)))
  )

(setq ess-dbg-regexp-reference "debug at +\\(.+\\)#\\([0-9]+\\):")

(setq ess-dbg-regexp-jump "debug at ")
(setq ess-dbg-regexp-active
      (concat "\\(\\(?:Called from: \\)\\|\\(?:debugging in: \\)\\)\\|"
              "\\(\\(?:Browse[][0-9]+\\)\\|\\(?:debug: \\)\\)"))

(defun inferior-ess-dbg-output-filter (proc string)
  "Standard output filter for the inferior ESS process
when `ess-debug' is active. Runs
`inferior-ess-output-filter', checks for activation
expressions (defined in `ess-dbg-regexp-action) and if in
debugging state puts the output in *ess.dbg* buffer"
  (let* ((is-iess (equal major-mode 'inferior-ess-mode))
        (pbuff (process-buffer proc))
        (dbuff (buffer-local-value 'ess-dbg-buffer pbuff))
        (dactive (buffer-local-value 'ess-dbg-active-p pbuff))
        (input-point (point-marker))
        (match-jump (string-match ess-dbg-regexp-jump string))
        (match-active (string-match ess-dbg-regexp-active string))
        (match-skip (and match-active
                         (match-string 1 string)))
        );; current-buffer is still the user input buffer here
    (when match-jump
      (with-current-buffer dbuff              ;; insert string in *ess.dbg* buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (concat "|-" string "-|"))
          ))
      (if is-iess
          (save-selected-window
            (ess-dbg-goto-last-ref-and-mark dbuff t))
        (ess-dbg-goto-last-ref-and-mark dbuff)
        ))
    (when match-skip
      (process-send-string proc  "n \n")  ;; skips first requiest
      )
    (when (and dactive
               (not (or match-jump match-active))
               (string-match "^> " string) ;check for main  prompt!! the process splits the output and match-end == nil might indicate this only
               )
      ;; (with-current-buffer dbuff
      ;;   (let ((inhibit-read-only t))
      ;;     (goto-char (point-max))
      ;;     (insert (concat " ---\n " string "\n ---"))
      ;;     ))
      (ess-dbg-deactivate-overlays)
      (with-current-buffer pbuff
        (setq ess-dbg-active-p nil))
      (message "|<-- exited debug -->|")
      )
    (when (and (not dactive)
               (or match-jump match-active))
      (unless is-iess
             (ring-insert ess-dbg-input-ring input-point))
      (with-current-buffer pbuff
        (setq ess-dbg-active-p t)
        )
      (setq dactive t)
      )
    (inferior-ess-output-filter proc string)
    (when (and match-skip (not is-iess))
      (ess-dbg-easy-command t))
    )
  )


(defun ess-dbg-goto-last-ref-and-mark (dbuff &optional other-window)
  "Open the most recent debug reference, and set all the
necessary marks and overlays.

It's called from `inferior-ess-dbg-output-filter'.  DBUFF must be
the *ess.dbg* buffer associated with the process. If OTHER-WINDOW
is non nil, attempt to open the location in a different window."

  (interactive)
  (let (t-debug-position ref)
    (with-current-buffer  dbuff
      (setq ref  (ess-dbg-get-next-ref -1 (point-max) ess-dbg-last-ref-marker ess-dbg-regexp-reference)) ; sets point at the end of found ref
      (when ref
        (move-marker ess-dbg-last-ref-marker (point-at-eol))
        (move-marker ess-dbg-current-ref ess-dbg-last-ref-marker) ;; each new step repositions the current-ref!
        )
      )
    (when ref
      (if (apply 'ess-dbg-goto-ref other-window ref)
          (progn ;; if referenced  buffer is found
            (setq t-debug-position (copy-marker (point-at-bol)))
            (if (equal t-debug-position ess-dbg-current-debug-position)
                (progn ;; highlights the overlay for ess-dbg-blink-interval seconds
                  (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-blink-same-ref-face)
                  (run-with-timer ess-dbg-blink-interval nil
                                  '(lambda ()
                                     (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-current-debug-line-face)
                                     )
                                  )
                  )
                                        ;else
              (ess-dbg-activate-overlays)
              )
            )
        ;;else, buffer is not found: highlight and give the corresponding message
        (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-blink-ref-not-found-face)
        (run-with-timer ess-dbg-blink-interval nil
                        '(lambda ()
                           (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-current-debug-line-face)
                           )
                        )
        (message "Referenced file %s is not found" (car ref))
        )
      )
    )
  )

(defun ess-dbg-goto-ref (other-window file  &optional line col)
  "Opens the reference given by FILE, LINE and COL,
Try to open in a different window  if OTHER-WINDOW is nil.
Return the buffer if found, or nil otherwise be found.
`ess-dbg-find-file' is used to find the FILE and open the
associated buffer. If FILE is nil returns nil.
"
  (if (stringp line) (setq line (string-to-number line)))
  (if (stringp col) (setq col (string-to-number col)))
  (let ((buffer (ess-dbg-find-file  file))
        (line (or line 1))
        )
    (when buffer
      (if (not other-window)
          (switch-to-buffer buffer t)
        (pop-to-buffer buffer t t))
      (save-restriction  ;; don't want to push-mark so not calling goto-line directly
        (widen)
        (goto-char 1)
        (forward-line (1- line))
        )
      (if col
          (goto-char (+ (point-at-bol) col))
        (back-to-indentation))
      buffer
      )
    )
  )

(defun ess-dbg-find-file (filename &rest directory formats)
  "Find a buffer for file FILENAME.
If FILENAME is not found at all, ask the user where to find it if
`ess-dbg-ask-for-file' is non-nil.  Search the directories in
`ess-dbg-search-path' and in DIRECTORY if supplied.  If DIRECTORY
is relative, it is combined with `default-directory'.FORMATS, if
given, is a list of formats to reformat FILENAME when looking for
it: for each element FMT in FORMATS, this function attempts to
find a file whose name is produced by (format FMT FILENAME)."
  ;; VS: modified version of compilation-find-file
  (or formats (setq formats '("%s")))
  (let ((dirs ess-dbg-search-path)
        (spec-dir (if directory
                      (expand-file-name directory)
                    default-directory)
                  ;; add current dir of ess here :TODO:
                  )
        ;; (ess-get-words-from-vector "getwd()\n")))) <- does not work,
        ;; the after-revert-hook calls it recursively! tothink:
        buffer thisdir fmts name)
    (if (file-name-absolute-p filename)
        ;; The file name is absolute.  Use its explicit directory as
        ;; the first in the search path, and strip it from FILENAME.
        (setq filename (abbreviate-file-name (expand-file-name filename))
              dirs (cons (file-name-directory filename) dirs)
              filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) spec-dir)
            fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
        (setq name (expand-file-name (format (car fmts) filename) thisdir)
              buffer (and (file-exists-p name)
                          (find-file-noselect name))
              fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    (if ess-dbg-ask-for-file
        (save-excursion            ;This save-excursion is probably not right.
          (let* ((pop-up-windows t)
                 (name (read-file-name
                        (format "Find next line in (default %s): "  filename)
                        spec-dir filename t nil
                        ))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (expand-file-name filename name)))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name)))))
          )
      )
    buffer);; nil if not found
  )

(defun ess-dbg-get-next-ref (n &optional pt BOUND REG nF nL nC)
  "Move point to the next reference in the *ess.dbg* buffer.

Must be called from *ess.dbg* buffer.
It returns the reference in the form (file line col) /all strings/ ,
or NIL if not found .  Prefix arg N says how many error messages
to move forwards (or backwards, if negative).  Optional arg PT,
if non-nil, specifies the value of point to start looking for the
next message, default to (point).  BOUND is the limiting position
of the search.  REG is the regular expression to search with.  nF
- sub-expression of REG giving the 'file'; defaults to 1.  nL -
giving the 'line'; defaults to 2.  nC - sub-expr giving the
'column'; defaults to 3.
"
  (interactive "p")
  (unless ess-dbg-buf-p
    (error "Not in *ess.dbg* buffer."))
  (setq nF (or nF 1)
        nL (or nL 2)
        nC (or nC 3))
  (or pt (setq pt (point)))
  ;; (message "ess-dbg-last-ref-marker%s vs  pt%s vs point-max%s" ess-dbg-last-ref-marker pt (point-max))
  (goto-char pt)
  (if (search-forward-regexp REG BOUND t n)
      (list (match-string nF) (match-string-no-properties nL) (match-string-no-properties nC))
    nil)
  )

(defun ess-dbg-next-ref-function (n &optional reset)
  "Advance to the next reference and visit the location
given by the reference.  This is the value of
`next-error-function' in *ess.dbg* buffers."
  (interactive "p")
  (if reset
       (set-marker ess-dbg-current-ref ess-dbg-last-ref-marker))
  (let ((loc (ess-dbg-get-next-ref n nil ess-dbg-current-ref))  ;; moves point to next/prev ref if any
                                        ; loc is  (file . line_nr)
         dbuff
        )
    (if loc
        (progn
          (set-marker ess-dbg-current-ref (line-end-position))
          (set-marker overlay-arrow-position (line-beginning-position))
          (setq dbuff (ess-dbg-find-file  (car loc)))
          (switch-to-buffer dbuff)
          (goto-line (cdr loc))
          (move-marker ess-dbg-current-debug-position (line-beginning-position))
          (back-to-indentation)
          )
      (if (>= 0 (or n 1))
          (error "Moved past first debug line")
        (error "Moved past last debug line")
        )
      )
    )
  )

(defun ess-dbg-easy-command (&optional wait)
  "Call commands defined in `ess-debug-easy-map'.
Easy input commands are those, which once executed do not requre
the prefix command for subsequent invocation.

 For example, if the prefix command is 'M-c' and
`ess-dbg-command-n' is bound to 'n' and `ess-dbg-command-c' is
bound to 'c' then 'M-c n n c' will execute `ess-dbg-command-n'
twise and `ess-dbg-command-c' once. Any other input not defined
in `ess-debug-easy-map' will cause the exit from easy input mode.
If WAIT is t, wait for next input and ignore the keystroke which
triggered the command."
  (interactive)
  (let* ((ev last-command-event)
         (command (lookup-key ess-debug-easy-map (vector ev))))
    (unless wait
      (call-interactively command))
    (while (setq command
                 (lookup-key ess-debug-easy-map
                             (vector (setq ev (read-event))))
                 )
      (call-interactively command)
      )
    (push ev unread-command-events)
    )
  )


(defun ess-dbg-command-n ()
  "Step next in debug mode.
Equivalent to 'n' at the R prompt."
  (interactive)
  (if (ess-dbg-is-active)
      (process-send-string (get-process ess-current-process-name) "\n")
    (message "Debugging is not active")
    )
  )


(defun ess-dbg-command-Q ()
  "Quits the browser/debug in R process.
 Equivalent to 'Q' at the R prompt."
  (interactive)
  (if (ess-dbg-is-active)
      (process-send-string (get-process ess-current-process-name) "Q\n")
    (message "Debugging is not active")
    )
  )

(defun ess-dbg-command-c ()
  "Continue the code execution.
 Equivalent of 'c' at the R prompt."
  (interactive)
  (if (ess-dbg-is-active)
      (process-send-string (get-process ess-current-process-name) "c\n")
    (message "Debugging is not active")
    )
  )

(defun ess-dbg-command-u ()
  "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
  (interactive)
  (message "not implemented yet")
  )

(defun ess-dbg-set-last-input ()
  "Set the `ess-tb-last-input' to point to the current process-mark"
  (interactive)
  (ess-force-buffer-current "R process to use: ")
  (let* ((last-input-process (get-process ess-local-process-name))
         (last-input-mark (copy-marker (process-mark last-input-process))))
    (with-current-buffer (process-buffer last-input-process)
      (when (local-variable-p 'ess-tb-last-input) ;; TB might not be active in all processes
        (setq ess-tb-last-input last-input-mark)
        (goto-char last-input-mark)
        (inferior-ess-move-last-input-overlay)
        (comint-goto-process-mark)
        )
      )
    )
  )

(defun ess-dbg-source-curent-file ()
  "Save current file and source it in the .R_GlobalEnv environment."
  ;; make it more elaborate :todo:
  (interactive)
  (unless ess-current-process-name
    (ess-force-buffer-current "R process to use: ")
    )
  (when buffer-file-name
      (save-buffer)
      (save-selected-window
        (ess-switch-to-ESS t)
        )
      (ess-dbg-set-last-input)
      (process-send-string (get-process ess-current-process-name)
                           (concat "\ninvisible(eval({source(file=\"" buffer-file-name
                                   "\")\n cat(\"Sourced: " buffer-file-name "\\n\")}, env=globalenv()))\n")
                           )
      )
  )

;;;_ + BREAKPOINTS

(defface ess-bp-fringe-inactive-face
  '((((background light)) (:foreground "DimGray"))
    (((background dark))  (:foreground "LightGray"))
    )
  "Face used to highlight inactive breakpoints in the fringe."
  :group 'ess-debug)

(defface ess-bp-fringe-browser-face
  '((((background light)) (:foreground "medium blue"))
    (((background dark))  (:foreground "deep sky blue"))
    )
  "Face used to highlight 'browser' breakpoints in the fringe."
  :group 'ess-debug)

(defface ess-bp-fringe-recover-face
  '((((background light)) (:foreground "dark magenta"))
    (((background dark))  (:foreground "magenta"))
    )
  "Face used to highlight 'recover' breakpoints in the fringe."
  :group 'ess-debug)


;;; elipses ... not yet
;; (defcustom ess-bp-show-elipses nil
;;   "If t elipses are shown for hiden debug text in ess-buffers.")

;; (defun ess-bp-set-invisibility-specs ()
;;   "Sets specs according to ess-bp-show-elipses"
;;   (add-to-list 'buffer-invisibility-spec (if ess-bp-show-elipses
;;                                              '(ess-bp . t)
;;                                            'ess-bp))
;;   (remove-from-invisibility-spec (if ess-bp-show-elipses
;;                                      'ess-bp
;;                                    '(ess-bp . t))))

(defcustom ess-bp-type-spec-alist
      '((browser "browser()" "B>\n"   filled-square  ess-bp-fringe-browser-face)
        (recover "recover()" "R>\n"   filled-square  ess-bp-fringe-recover-face)
        )
      "List of lists of breakpoint types.
Each sublist  has five elements:
1- symbol giving the type of bp.
2- R expression to be inserted
3- string to be displayed instead of the expression
4- fringe bitmap to use
5- face for fringe and displayed string."
      :group 'ess-debug
      :type '(alist :key-type symbol
                    :value-type (group string string symbol face)
                    )
      )

(defcustom ess-bp-inactive-spec
      '( nil     "##"    filled-square  ess-bp-fringe-inactive-face)
      "List for inactive breakpoint specifications.
List format is identical that of `ess-bp-type-spec-alist'.
TODO: nil??."
      :group 'ess-debug)


(defun ess-bp-create (&optional type)
  "Set breakpoint for the current line.
 Returns the begging position of the hidden text."
  (let* ((bp-specs (or (assoc type ess-bp-type-spec-alist)
                       (error "Undefined breakpoint type %s" type)))
         (init-pos (point-marker))
         (fringe-bitmap (nth 3 bp-specs))
         (fringe-face (nth 4 bp-specs))
         (displ-string (nth 2 bp-specs))
         (bp-command (concat  (nth 1 bp-specs) "##:ess-bp-end:##\n"))
         (bp-length (length bp-command))
         (dummy-string (concat "##:ess-bp-start::" (prin1-to-string (car bp-specs)) ":##\n"))
         (dummy-length (length dummy-string))
         insertion-pos
         )
    (set-marker init-pos (1+ init-pos))
    (setq displ-string (propertize displ-string
                                   'face fringe-face
                                   'font-lock-face fringe-face))
    (setq bp-command (propertize bp-command
                                 'ess-bp t
                                 'intangible 'ess-bp
                                 'rear-nonsticky '(intangible ess-bp bp-type)
                                 'bp-type type
                                 'bp-substring 'command
                                 'display displ-string
                                 ))
    (setq dummy-string (propertize dummy-string
                                   'ess-bp t
                                   'intangible 'ess-bp
                                   'bp-type type
                                   'bp-substring 'dummy
                                   'display (list 'left-fringe fringe-bitmap fringe-face)
                                   ))
    (back-to-indentation)
    (setq insertion-pos (point) )
    (insert (concat   dummy-string bp-command))
    (indent-for-tab-command)
    (goto-char (1- init-pos))  ;; sort of save-excursion
    insertion-pos
    )
  )

(defun ess-bp-get-bp-position-nearby ()
  "Return the cons (beg . end) of breakpoint limit points
closest to the current position.  Only currently visible region of the
buffer is searched.  This command is intended for use in
interactive commands like `ess-bp-toggle-state' and `ess-bp-kill'.
Use `ess-bp-previous-position' in programs."
  (interactive)
  (let* ( (pos-end (if (get-char-property (1- (point)) 'ess-bp)
                       (point)
                     (previous-single-property-change (point) 'ess-bp nil (window-start))))
          (pos-start (if (get-char-property (point) 'ess-bp)    ;;check for bobp
                         (point)
                       (next-single-property-change (point) 'ess-bp nil (window-end))))
          pos dist-up dist-down)
    (if (not (eq pos-end (window-start)))
        (setq dist-up (- (point) pos-end))
      )
    (if (not (eq pos-start (window-end)))
        (setq dist-down (- pos-start (point)))
      )
    (if (and dist-up dist-down)
        (if (< dist-up dist-down)
            (cons (previous-single-property-change pos-end 'ess-bp nil (window-start)) pos-end)
          (cons pos-start (next-single-property-change pos-start 'ess-bp nil (window-end))))
      (if dist-up
          (cons (previous-single-property-change pos-end 'ess-bp nil (window-start)) pos-end)
        (if dist-down
            (cons pos-start (next-single-property-change pos-start 'ess-bp nil (window-end)))
          )
          ;(message "No breakpoints in the visible area"))
        )
      )
    )
  )


(defun ess-bp-previous-position ()
  "Returns the cons (beg . end) of breakpoint limit points closest
to the current position, nil if not found. "
  (let* ( (pos-end (if (get-char-property (1- (point)) 'ess-bp)
                       (point)
                     (previous-single-property-change (point) 'ess-bp ))))
    (if pos-end
        (cons (previous-single-property-change pos-end 'ess-bp) pos-end)
      )
    )
  )

(defun ess-bp-kill ()
  "Remove the breakpoint nearby"
  (interactive)
  (let ((pos (ess-bp-get-bp-position-nearby))
        (init-pos (make-marker)))
    (if (null pos)
        (if (called-interactively-p) (message "No breakpoint nearby"))
      (set-marker init-pos (1+ (point)))
      (goto-char (car pos))
      (delete-region (car pos) (cdr pos))
      (indent-for-tab-command)
      (goto-char (1- init-pos))
      (if (eq (point) (point-at-eol)) (forward-char))
  ))
  )

(defun ess-bp-kill-all nil
  "Delete all breakpoints in current buffer."
  (interactive)
  (let ((count 0)
        (init-pos (make-marker))
        pos)
    (set-marker init-pos (1+ (point)))
    (save-excursion   ;; needed if error
      (goto-char (point-max))
      (while (setq pos (ess-bp-previous-position))
        (goto-char (car pos))
        (delete-region (car pos) (cdr pos))
        (indent-for-tab-command)
        (setq count (1+ count))
        )
      (message "Killed %d breakpoints" count)
      )
    (goto-char (1- init-pos))
    ))


(defun ess-bp-toggle-state ()
  "Toggle the breakpoint between active and inactive states."
  (interactive)
  (save-excursion
    (let ((pos (ess-bp-get-bp-position-nearby))
          (fringe-face (nth 3 ess-bp-inactive-spec))
          (inhibit-point-motion-hooks t)  ;; deactivates intangible property
          beg-pos-dummy end-pos-comment bp-specs)
      (if (null pos)
          (message "No breakpoints in the visible region")
        (goto-char (car pos))
        (setq beg-pos-command (previous-single-property-change (cdr pos) 'bp-substring nil (car pos)))
        (goto-char beg-pos-command)
        (if (equal (get-char-property (1- beg-pos-command) 'bp-substring) 'comment)
            (progn
              ;; not use beg-pos-command here ## is deleted
              (delete-region (previous-single-property-change beg-pos-command 'bp-substring nil (car pos)) beg-pos-command)
              (setq bp-specs (assoc (get-text-property (point) 'bp-type) ess-bp-type-spec-alist))
              (put-text-property  (car pos) (point)
                                  'display (list 'left-fringe (nth 3 bp-specs) (nth 4 bp-specs)))
              )
          (put-text-property  (car pos) beg-pos-command   ;; dummy display change
                              'display (list 'left-fringe (nth 2 ess-bp-inactive-spec) fringe-face))
          (insert (propertize "##"
                              'ess-bp t
                              'intangible 'ess-bp
                              'display (propertize (nth 1 ess-bp-inactive-spec) 'face fringe-face)
                              'bp-type (get-char-property (point) 'bp-type)
                              'bp-substring 'comment
                              )))))))


(defun ess-bp-make-visible ()
  "Makes bp text visible."
  (interactive)
  (let ((pos (ess-bp-get-bp-position-nearby)))
    (set-text-properties (car pos) (cdr pos) (list 'display nil))
    )
  )

(defun ess-bp-set ()
  (interactive)
  (let* ((pos (ess-bp-get-bp-position-nearby))
         (same-line (if pos
                        (and (<=  (point-at-bol) (cdr pos)) (>= (point-at-eol) (car pos)))))
         (types ess-bp-type-spec-alist)
         bp-type
         (ev last-command-event)
         (com-char  (event-basic-type ev))
         )
    (when same-line
      (setq bp-type (get-text-property (car pos) 'bp-type))  ;; the meaning of bp-type changes latter (confounding slightly)
      (setq types (cdr (member (assq bp-type types) types))) ; nil if bp-type is last in the list
      (if (null types) (setq types ess-bp-type-spec-alist))
      (ess-bp-kill)
      (indent-for-tab-command)
      )
    (setq bp-type (pop types))
    (ess-bp-create (car bp-type))
    (while  (eq (setq ev (read-event)) com-char)
      (if (null types) (setq types ess-bp-type-spec-alist))
      (setq bp-type (pop types))
      (ess-bp-kill)
      (ess-bp-create (car bp-type))
      (indent-for-tab-command)
      )
    (push ev unread-command-events)
    )
  )


(defun ess-bp-next nil
  "Goto next breakpoint."
  (interactive)
  (let ((cur-pos (point))
        (bp-pos (next-single-property-change (point) 'ess-bp)))
    (if bp-pos
            (goto-char bp-pos)
      ;; else start searching from the beggining of buffer
      (setq bp-pos (next-single-property-change (point-min) 'ess-bp nil (point)))
      (if (equal bp-pos (point))
          (message "No breakpoints found")
        (goto-char bp-pos))
      )
    )
  )


(defun ess-bp-previous nil
  "Goto previous breakpoint."
  (interactive)
  (let ((cur-pos (point))
        (bp-pos (previous-single-property-change (point) 'ess-bp)))
    (if bp-pos
        (goto-char bp-pos)
      ;; else start searching from the end of buffer
      (setq bp-pos (previous-single-property-change (point-max) 'ess-bp nil (point)))
      (if (equal bp-pos (point))
          (message "No breakpoints found")
        (goto-char bp-pos))
      )
    )
  )

;;;_* Kludges and Fixes

;;; delete-char and delete-backward-car do not delete whole intangible text
(defadvice delete-char (around delete-backward-char-intangible activate)
  "When about to delete a char that's intangible, delete the whole intangible region
Only do this when #chars is 1"
  (if (and (= (ad-get-arg 0) 1)
           (get-text-property (point) 'intangible))
      (progn
       (kill-region (point) (next-single-property-change (point) 'intangible))
       (indent-for-tab-command)
       )
    ad-do-it
    ))

(defadvice delete-backward-char (around delete-backward-char-intangible activate)
  "When about to delete a char that's intangible, delete the whole intangible region
Only do this when called interactively and  #chars is 1"
  (if (and (= (ad-get-arg 0) 1)
           (> (point) (point-min))
           (get-text-property (1- (point)) 'intangible))
      (progn
        (kill-region (previous-single-property-change (point) 'intangible) (point))
        (indent-for-tab-command)
        )
    ad-do-it
    ))

;;; previous-line gets stuck if next char is intangible
(defadvice previous-line (around solves-intangible-text-kludge activate)
  "When about to move to previous line when next char is
intanbible, step char backward first"
  (if (and (or (null (ad-get-arg 0))
               (= (ad-get-arg 0) 1))
           (get-text-property (point) 'intangible))
      (backward-char 1)
    )
  ad-do-it
  )

;;TODO
;; (defun ess-bp-remove-all-all-buffers nil
;;   "Delete all visible breakpoints in all open buffers."
;;   (interactive)
;;   (let ((buffers (buffer-list)))
;;     (save-excursion
;;       (while buffers
;;         (set-buffer (car buffers))
;;         (ess-bp-remove-all-current-buffer)
;;         (setq buffers (cdr buffers))))))



(print "<- debug done")
(provide 'ess-tracebug)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ess-tracebug.el ends here