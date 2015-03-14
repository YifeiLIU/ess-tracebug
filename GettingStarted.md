

## Sourcing Errors ##

Download the [`test.R`](http://ess-tracebug.googlecode.com/svn/trunk/test.R) file
from the repository.  It contains one function, `lm_test`, with an erroneous ","
at line 6.

Open `test.R` and press `C-c C-l` (`ess-load-file`), or evaluate the function
with `C-c C-c` or `C-M-x`. You should get an error `unexpected ','` with the
reference to the error position highlighted:

<img src='http://ess-tracebug.googlecode.com/svn/trunk/img/tut1_comma_error.png' width='500'>

Now you can jump directly to the error location with default emacs keys "C-x `"<br>
or "M-g n". The error position is highlighted for <code>next-error-highlight</code>
seconds. Mouse-click on the highlighted error reference also jumps to the error<br>
location.<br>
<br>
The above navigation functionality is based on next-error and<br>
next-error-no-select capabilities which you get in compilation, grep<br>
and occur modes.<br>
<br>
<h2>Interactive Debugger</h2>

Correct the "," error in the test.R and place the break-point (<code>C-c C-t b</code>) at<br>
some arbitrary position. Eval the function (<code>C-c C-c</code>).<br>
<br>
Create a new file, <a href='http://ess-tracebug.googlecode.com/svn/trunk/main.R'>`main.R`</a>, which contains the following code:<br>
<pre><code>require(stats)<br>
require(graphics)<br>
lm_test(Fertility ~ . , data = swiss)<br>
</code></pre>

Execute the above code interactively (<code>C-c C-c</code>). You should be brought to the<br>
break-point in the <code>test.R</code> file. Fringe should display a small triangle and the<br>
current line is highlighted:<br>
<br>
<img src='http://ess-tracebug.googlecode.com/svn/trunk/img/tut2_break_point.png' width='500'>

Immediately after entering the interactive debugging start pressing <code>M-N</code> (aka<br>
<code>Alt-Shift-n</code>) to jump through you code.<br>
<br>
When process is in the debugging mode (that is, there is a big red DB in the<br>
mode-line), a special keymap <code>ess-debug-minor-mode-map</code> is active. See the<br>
inline docstring for the available commands. Currently the following commands are bound:<br>
<br>
<code>`</code>
M-C   . Continue                  . `ess-debug-command-continue'<br>
M-C-C . Continue multi            . `ess-debug-command-continue-multi'<br>
M-N   . Next step                 . `ess-debug-command-next'<br>
M-C-N . Next step multi           . `ess-debug-command-next-multi'<br>
M-U   . Up frame                  . `ess-debug-command-up'<br>
M-Q   . Quit debugging            . `ess-debug-command-quit'<br>
<code>`</code>

<h2>Breakpoints</h2>

Try placing multiple breakpoints in the buffer:<br>
<br>
<ul><li>C-c C-t b - browser breakpoint<br>
</li><li>C-c C-t b b - recover breakpoint<br>
</li><li>C-c C-t B - conditional browser breakpoint</li></ul>

Toggle their state (C-c C-t 0) and kill them one by one (C-c C-t k). These<br>
operations apply to the closest breakpoint to the cursor in the visible area<br>
of the buffer. Kill all breakpoints in the buffer with <code>C-c C-t K</code>.<br>
<br>
The recover breackpoint insert the <code>recover()</code> command in your code. It stops<br>
the execution of the code and offers to browser the call stack. Ess-tracebug<br>
automatically recognizes the "Selection:" prompt and activates single digit<br>
shortcuts. You can jump to a desired frame by just pressing it's number 1-9, or<br>
exit the recover with "0".<br>
<br>
<h2>Debug/ Undebug at point</h2>

Go to <code>test.R</code> and remove all the breakpoints (<code>C-c C-t K</code>). Eval the function<br>
(<code>C-M-x</code>).<br>
<br>
Press <code>C-c C-t d</code> (or <code>C-c C-t C-d</code>) and enter lm_test:<br>
<br>
<img src='http://ess-tracebug.googlecode.com/svn/trunk/img/flag2.png' width='500'>

This will flag the "lm_test" function for the debugging (equivalent to<br>
<code>debug(lm_test)</code> at R's prompt). Next time you call <code>lm_test()</code> the debugger<br>
will start jumping again.<br>
<br>
To undebug press <code>C-c C-t u</code>.<br>
<br>
You also can trace/untrace S4 methods in this way.<br>
<br>
Note that the interactive mechanism of this feature is based on<br>
<a href='http://www.emacswiki.org/emacs/InteractivelyDoThings'>IDO</a> which is part of<br>
emacs.<br>
<br>
<h2>Traceback Buffer</h2>

After an error occurred, press <code>C-c `</code> (back-tick) to show the <code>*ess-traceback*</code> buffer:<br>
<br>
<img src='http://ess-tracebug.googlecode.com/svn/trunk/img/tut3_traceback.png' width='500'>

It displays the complete call stack and the most recent error with the<br>
references to the source file positions highlighted.<br>
<br>
In <code>*ess-traceback*</code> buffer, <code>compilation-minor-mode</code> is active. Use <code>C-h b</code> to<br>
see the available navigation keys.<br>
<br>
<br>
<h2>Watch</h2>

Press "C-c C-t w" to open the watch window. First what you see is a list of<br>
available commands. You can add (a), insert (i), move up (u), down (U), kill<br>
(k), edit (e) and rename (r) expressions in that buffer.  Any valid R expression<br>
can be traced in the watch window. To quit the watch press "q".<br>
<br>
<img src='https://ess-tracebug.googlecode.com/svn/trunk/img/watch.png' width='700'>

The watch window is continuously updated to show the current state of the<br>
variables.