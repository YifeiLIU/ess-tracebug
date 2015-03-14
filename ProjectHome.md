An emacs package for interactive debugging and error tracing for [ESS](http://ess.r-project.org/).


From ESS v12.02 ess-tracebug is part of ESS. This repository is kept for documentation only.



# Activation #

From ESS13.05 tracebug is on by default. To deactivate use either `M-x
ess-tracebug` or put the following into your .emacs:

```
(setq ess-use-tracebug nil)
```

# Keys #

Tracebug commands are all on `ess-dev-map` (C-c C-t). During the debugging
`ess-debug-minor-mode-map` is active.

```

* Breakpoints (`ess-dev-map'):
  
  b   . Set BP (repeat to cycle BP type) . `ess-bp-set'*
  B   . Set conditional BP               . `ess-bp-set-conditional'
  k   . Kill BP                          . `ess-bp-kil'
  K   . Kill all BPs                     . `ess-bp-kill-all'
  o   . Toggle BP state                  . `ess-bp-toggle-state'
  l   . Set logger BP                    . `ess-bp-set-logger'
  n   . Goto next BP                     . `ess-bp-next'
  p   . Goto previous BP                 . `ess-bp-previous'
  
  (C- prefixed equivalents are also defined)
  
* Debugging (`ess-dev-map'):
  
  `   . Show traceback                       . `ess-show-traceback' (also on C-c `)
  ~   . Show callstack                       . `ess-show-call-stack' (also on C-c ~)
  e   . Toggle error action (repeat to cycle). `ess-debug-toggle-error-action'*
  d   . Flag for debugging                   . `ess-debug-flag-for-debugging'
  u   . Unflag for debugging                 . `ess-debug-unflag-for-debugging'
  w   . Watch window                         . `ess-watch'
  
  (C- prefixed equivalents are also defined)
  
* Interactive Debugging (`ess-debug-minor-mode-map'):
  
  M-C   . Continue                  . `ess-debug-command-continue'
  M-C-C . Continue multi            . `ess-debug-command-continue-multi'
  M-N   . Next step                 . `ess-debug-command-next'
  M-C-N . Next step multi           . `ess-debug-command-next-multi'
  M-U   . Up frame                  . `ess-debug-command-up'
  M-Q   . Quit debugging            . `ess-debug-command-quit'
  
* Navigation to errors (general emacs functionality):
  
  C-x `, M-g n   . `next-error'
  M-g p          . `previous-error'
  
```

# Error Tracing #

## Navigation ##

Ess-tracebug provides compilation-like functionality in inferior-ess buffer. The errors with
location references are highlighted like this:

> <img src='https://ess-tracebug.googlecode.com/svn/trunk/img/error_in_iess.png' width='500'></li></ul>

To navigate between errors, you can use usual binding for next/previous-error<br>
("M-g n" , "M-g p"). Error navigation scope is limited to the most recent<br>
errors located between the last user input mark (indicated by the small arrow)<br>
and the end of buffer:<br>
<br>
<blockquote><img src='https://ess-tracebug.googlecode.com/svn/trunk/img/last_input_arrow.png' width='500'></blockquote>

Often ESS eval commands result in a long series of errors, but only the first
being the most relevant one. Now you can quickly find the first error by
locating the fringe arrow.

When files are sourced with incomplete file names (`source("my_file.R")`), R
reports only the file name in the error references. Ess-traceback reconstructs
the whole path by using the current directory associated with the current
buffer. Usually this is not a problem because all the project files are in the
same directory.

To make ess-tracebug aware of different directories, customize the variable
`ess-tracebug-search-path`:

```
   (setq ess-tracebug-search-path '("~/projects/R/project_A" "~/projects/R/project_B"))
```


## Traceback Buffer ##

Whenever the traceback is available in R (usually immediately after the error
occurred) the [``C-c ```](`ess-show-R-traceback`) displays `*ess-traceback*`
compilation-like buffer:

> <img src='https://ess-tracebug.googlecode.com/svn/trunk/img/ess_traceback.png' width='500'></li></ul>

You might want simple keys bound for navigation in <b><code>*ess-traceback*</code></b> buffer:<br>
<br>
<pre><code>  (define-key compilation-minor-mode-map [(?n)] 'next-error-no-select)<br>
  (define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select)<br>
</code></pre>

<h1>Visual Debugger #

For the visual debugging you need to set the breakpoints in your source file and
source it either interactively with `C-c C-l` key, or directly with `source()`
command.

Evaluation of functions (`C-c C-c`, `M-C-x`, `C-c C-f`) also automatically
inserts source references.

## Breakpoints ##

  * "b" 'ess-bp-set `*` _(set/toggle the breakpoint  at point)_
  * "o" 'ess-bp-toggle-state `*`  _(toggle between active/inactive states of the closest breackpoint in the **visible** part of the buffer)_
  * "k" 'ess-bp-kill  _(remove the closest bp in the **visible** area of the buffer)_
  * "K" 'ess-bp-kill-all _(removes all the bps in the buffer)_
  * "n/" 'ess-bp-next  _(navigate to next bp)_
  * "p" 'ess-bp-previous _(navigate to previous bp)_

`ess-bp-set` is an single-key command and multiple invocation of "b" key
cycles through the breakpoint types defined in
`ess-bp-type-spec-alist`. Currently only `browser()` and `recover()` types
are defined. The `recover` breakpoint is useful whenever you want to examine
the call stack.

See the documentation for `ess-bp-type-spec-alist` for more
details. Available fringe bitmaps are in the variable `fringe-bitmaps`.

`C-c C-t o` toggles the active state of the breakpoint, i.e. comment it out.

Here is a sample of breakpoint types so far discussed (`browser`, `recover`, `commented browser` and the custom `my-browser`):

> <img src='https://ess-tracebug.googlecode.com/svn/trunk/img/breakpoint_types.PNG' width='500'></li></ul>

<h2>Conditional Breakpoints ##

Use 'C-c C-t B' to set the conditional breackpoint:

> <img src='https://ess-tracebug.googlecode.com/svn/trunk/img/CB.png' width='500'></li></ul>


<h2>Loggers ##

  * "l" ess-bp-set-logger

Loggers are like breakpoints but don't break the execution. They log the
returned value of the watched expressions (see [Watch](#Watch_Window.md)) into
the variable (a list) with the user provided name:

> <img src='https://ess-tracebug.googlecode.com/svn/trunk/img/logger.png' width='500'></li></ul>


<h2>Debugging ##

<img src='https://ess-tracebug.googlecode.com/svn/trunk/img/Debugging.png' width='500'>


Ess-tracebug automatically detects when the R session enters the debugger and<br>
turns <code>ess-debug-minor-mode</code> on. This makes the commands in  <code>ess-debug-minor-mode-map</code> active:<br>
<br>
<br>
<ul><li>M-C   . Continue                  . `ess-debug-command-continue'<br>
</li><li>M-C-C . Continue multi            . `ess-debug-command-continue-multi'<br>
</li><li>M-N   . Next step                 . `ess-debug-command-next'<br>
</li><li>M-C-N . Next step multi           . `ess-debug-command-next-multi'<br>
</li><li>M-U   . Up frame                  . `ess-debug-command-up'<br>
</li><li>M-Q   . Quit debugging            . `ess-debug-command-quit'</li></ul>

Ess-tracebug also detects when the session enters the "recover" state. Pressing<br>
a digit produces the same effect as typing it in the terminal followed by<br>
RET. Also for convenience, "q","c" and "n" have the same effect as "0" - not to<br>
interrupt the debugging flow when the recover breackpoint is used.<br>
<br>
<br>
<h1>Flag/Unflag for Debugging</h1>

<ul><li>"d" 'ess-debug-flag-for-debugging<br>
</li><li>"u" 'ess-debug-unflag-for-debugging</li></ul>

Ess-tracebug uses <a href='http://www.emacswiki.org/emacs/InteractivelyDoThings'>IDO</a>
mechanism (if available) to provide the user with the <i>on the fly</i> debugging<br>
and undebugging of functions and methods.<br>
<br>
<i><b>Debug</b></i>

'C-c C-t d' asks the user for the function to debug:<br>
<br>
<blockquote><img src='https://ess-tracebug.googlecode.com/svn/trunk/img/debug_at_point.png' width='700'></blockquote>

If the selected function is a generic, the signature of the method is requested as well:<br>
<br>
<blockquote><img src='https://ess-tracebug.googlecode.com/svn/trunk/img/debug_at_point_method.png' width='700'></blockquote>

The following usual <a href='http://www.emacswiki.org/emacs/InteractivelyDoThings'>IDO</a> keys are available in the mini-buffer:<br>
<br>
<ul><li>'C-s'/'C-r' <i>to cycle forward/backward through completions</i>
</li><li>'C-space' <i>to restrict the subsequent search (for example 'mat C-space def' displays all the functions with mat and def in their names)</i></li></ul>

<blockquote>Library <i>Matrix</i> has a number of methods and S4 classes defined. Load it and play with the methods</blockquote>

<pre><code>    library(Matrix)<br>
</code></pre>

<blockquote><i><b>Undebug</b></i></blockquote>

<blockquote>To unflag the debugged function or traced methods use 'C-c C-t u':</blockquote>

<blockquote><img src='https://ess-tracebug.googlecode.com/svn/trunk/img/undebug.png' width='700'></blockquote>


<h1>Watch Window</h1>

<ul><li>"w" 'ess-watch</li></ul>

In the watch window you can monitor any valid <b>R</b> expression:<br>
<br>
<blockquote><img src='https://ess-tracebug.googlecode.com/svn/trunk/img/watch.png' width='700'></blockquote>

<blockquote>Start with 'C-c C-t w' to see all available commands in the watch window.<br>
</blockquote><ul><li><code>a       </code>: append new expression<br>
</li><li><code>i       </code>: insert new expression<br>
</li><li><code>k       </code>: kill<br>
</li><li><code>e       </code>: edit the expression<br>
</li><li><code>r       </code>: rename<br>
</li><li><code>n/p     </code>: navigate<br>
</li><li><code>u/U     </code>: move the expression up/down<br>
</li><li><code>q       </code>: kill the buffer</li></ul>


The text in the watch window is scaled down by <code>'text-scale-mode-step'</code>
times <code>'ess-watch-scale-amount'</code>. Customize the latter to adjust the display<br>
size of the text. The default is '-1'.<br>
<br>
<pre><code>    (setq ess-watch-scale-amount -2)<br>
</code></pre>

Whenever the watch buffer already exists, it is displayed during the<br>
debugging. To prohibit the display, quit the watch ("q") or just kill the<br>
buffer. Note what watched expressions are not lost. They are registered in<br>
the R session and will be available on the next invocation of 'C-c C-t w'.<br>
<br>
By default ess-tracebug tries to split the R process window reasonably. You<br>
can control that behavior by customizing 'ess-watch-height-threshold' and<br>
'ess-watch-width-threshold'. See the documentation for those variables.<br>
<br>
<h1>On-error Actions</h1>

<ul><li>"e" 'ess-debug-toggle-error-action</li></ul>

When ess-tracebug is turned on, mode line for the current process is changed<br>
into <code> (iESS [R db - ]</code>, where "-" is an indicator of an "on-error" action,<br>
and can be by default one of:<br>
<br>
#  "-" (none - the default behavior),<br>
#  "t" (traceback) or<br>
#  "r" (recover).<br>
<br>
To switch between on-error states press 'C-c C-t e', and keep pressing 'e'<br>
till the desired action is set.<br>
<br>
You can add your own custom on-error actions to the <code>ess-debug-error-action-alist</code>. See also R documentation for <code>options(error=)</code>.<br>
<br>
