;; -*- lexical-binding: t; -*-
;;
;; Tabular:          Align columnar data using regex-designated column boundaries
;; Author:           Andrew Peck (andrew.peck@cern.ch)
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;
;; Sometimes, it's useful to line up text.  Naturally, it's nicer to have the
;; computer do this for you, since aligning things by hand quickly becomes
;; unpleasant.  While there are other plugins for aligning text, the ones I've
;; tried are either impossibly difficult to understand and use, or too simplistic
;; to handle complicated tasks.  This plugin aims to make the easy things easy
;; and the hard things possible, without providing an unnecessarily obtuse
;; interface.  It's still a work in progress, and criticisms are welcome.
;;
;; License:    Copyright (c) 2012, Matthew J. Wozniski
;;             Ported to emacs in 2022, Andrew Peck
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * The names of the contributors may not be used to endorse or promote
;;       products derived from this software without specific prior written
;;       permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;; NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
;; OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;; Code:

;;  dash used for -zip-lists
;;  look for an alternative way to do this
(require 'dash)

(defun tabular--strlen (string)
  ;; Return the number of bytes in a string after expanding tabs to spaces. This
  ;; expansion is done based on the current value of 'tabstop'
  (string-width string))

(defun tabular--right-align  (string fieldwidth)
  "Right align STRING in a field of size FIELDWIDTH.
This does not trim leading and trailing spaces."
  (format (concat "%" (format "%d" fieldwidth) "s") string))

(defun tabular--left-align  (string fieldwidth)
  "Left align STRING in a field of size FIELDWIDTH.
This does not trim leading and trailing spaces."
  (format (concat "%-" (format "%d" fieldwidth) "s") string))

(defun tabular--center-align  (string fieldwidth)
  "Center align STRING in a field of size FIELDWIDTH.
This does not trim leading and trailing spaces."
  (let* ((spaces (max 0 (- fieldwidth (tabular--strlen string))))
         (right (floor (/ spaces 2.0)))
         (left  (ceiling (/ spaces 2.0))))
    (concat (make-string left ? ) string (make-string right ? ))))

;; thanks to https://emacs.stackexchange.com/questions/5729/split-a-string-without-consuming-separators
(defun tabular--split-string-keep-sep (string &optional separators omit-nulls keep-sep)
  "Split STRING into substrings bounded by matches for SEPARATORS."
  (let* ((keep-nulls (not (if separators omit-nulls t)))
         (rexp (or separators split-string-default-separators))
         (start 0)
         this-start this-end
         notfirst
         (list nil)
         (push-one
          (lambda ()
            (when (or keep-nulls (< this-start this-end))
              (let ((this (substring string this-start this-end)))
                (when (or keep-nulls (> (length this) 0))
                  (push this list)))))))
    (while (and (string-match
                 rexp string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< start (length string)))
      (setq notfirst t)
      (setq this-start start this-end (match-beginning 0)
            start (match-end 0))
      (funcall push-one)
      (when keep-sep
        (push (match-string 0 string) list)))
    (setq this-start start this-end (length string))
    (funcall push-one)
    (nreverse list)))

;; (split-string "* one * two * three" "\*" t t)
;; -> ("*" " one " "*" " two " "*" " three")

(defun tabular--split-delim (string delim)
  "Split a string into fields and delimiters. Like split(), but
include the delimiters as elements"
  (tabular--split-string-keep-sep string delim t t))

(defun tabular--set-lines (start len strings)
  "Replace lines from START to START + LEN - 1 with the given STRINGS.
If more lines are needed to show all strings, they will be added.
If there are too few strings to fill all lines, lines will be
removed."
  (save-excursion
    (goto-line start)
    (kill-line len)
    (dolist (string strings)
      (insert (concat  string "\n")))))

(defun tabular--filter-string (lines commandstring)
  "Runs the given COMMANDSTRING argument as an expression.
The COMMANDSTRING expression is expected to reference the LINES
argument. If the COMMANDSTRING expression returns a list the
items of that list will replace the items in LINES, otherwise the
expression is assumed to have modified LINES itself."

  ;; function! s:FilterString(lines, commandstring)
  ;;   exe 'let rv = ' . a:commandstring

  ;;   if type(rv) == type(a:lines) && rv isnot a:lines
  ;;     call filter(a:lines, 0)
  ;;     call extend(a:lines, rv)
  ;;   endif
  ;; endfunction

  )

(defcustom tabular-default-format "l1"
  "Default format for tabular. [lrc][0-9]"
  :type 'string)

(defun tabular--element-format-pattern ()
  "Returns a regex for valid format patterns"
  "^\\(\\([lrc]\\)\\([0-9]+\\)\\)+$")

(defun tabular--validate-formatstr (formatstr)
  "Check that a format string matches the schema"
  (string-match (tabular--element-format-pattern) formatstr))

(defun tabular--parse-formatstr (formatstr)
  "Convert a FORMATSTR into a nested list of format specifications, e.g.
r1c1l0 -> '((r 1) (c 1) (l 0))"
  (-zip-lists
   (split-string formatstr "[0-9]+" t)
   (mapcar #'string-to-number (split-string formatstr "[lrc]" t))))

(defun tabular--split-string-list (string-list delimiter)
  "split the string list into list of lists split on delimiters"
  ;;   let lines = map(a:strings, 's:SplitDelim(v:val, a:delim)')
  ;; TODO
  string-list)

;; let s:do_gtabularize = (get(a:options, 'mode', '') ==# 'GTabularize')
(setq do_tabularize nil)

(defun tabular--strip-line (line &optional gtabularize)
  "Strip spaces out of a LINE, which is a list of strings split by the delimeter
offset

Only from non-delimiters; spaces in delimiters must have been
matched intentionally

Don't strip leading spaces from the first element; we like
indenting."

  ;; Leave non-matching lines unchanged for gtabularize
  (when (not (and gtabularize (= 1 (length line)) line))

    ;; strip trailing (but not leading) spaces from the first element
    (when (not (string-match (nth 0 line) "^\s*$"))
      (setf (nth 0 line)
            (string-trim-right (nth 0 line))))

    ;; length 2 line is just 1st element + delimiter, so skip the 2nd element
    ;; for 3rd element and beyond strip leading and trailing spaces from other elements
    (when (>= 3 (length line))
      (let ((i 2))
        (while (< i (length line))
          (setf (nth i line)
                (string-trim-left
                 (string-trim-right (nth i line))))
          (setq i (+ 1 i))))))
  line)

(defun tabular--get-max-length (line &rest gtabularize)
  "Find the max length of each field"

  ;;   let maxes = []
  ;;   for line in lines
  ;;     if len(line) == 1 && s:do_gtabularize
  ;;       continue " non-matching lines don't affect field widths for GTabularize
  ;;     endif

  ;;     for i in range(len(line))
  ;;       if i == len(maxes)
  ;;         let maxes += [ s:Strlen(line[i]) ]
  ;;       else
  ;;         let maxes[i] = max( [ maxes[i], s:Strlen(line[i]) ] )
  ;;       endif
  ;;     endfor
  ;;   endfor

  )

(defun tabular--concatenate-line (line maxes format &optional lead-blank gtabularize)
  "Concatenate the fields, according to the format pattern."

  ;; GTabularize doesn't change non-matching lines
  (when (not (and gtabularize (= 1 (length line)) line))

    (let ((i 0))
      (while (< i (length line))

        ;; (debug)
        (let ((how (car (nth (mod i (length format)) format))) ;;       let how = format[i % len(format)][0]
              (pad (cadr (nth (mod i (length format)) format))) ;;       let pad = format[i % len(format)][1:-1]
              (field nil))

          (print how)
          (print pad)


          (setq field
                (cond
                 ((string= how "l") (tabular--left-align (nth i line) (nth i maxes)))
                 ((string= how "r") (tabular--right-align (nth i line) (nth i maxes)))
                 ((string= how "c") (tabular--center-align (nth i line) (nth i maxes)))))

          (setf (nth i line)
                (concat field
                        (if (and (= i 0) lead-blank) ""
                          (make-string pad ? ))))
          (setq i (+ 1 i)))))) line)

(defun tabular--tabularize-strings (strings delim &optional formatstr gtabularize)
  "Given a list of strings and a delimiter, split each string on
every occurrence of the delimiter pattern, format each element
according to either the provided format (optional) or the default
format, and join them back together with enough space padding to
guarantee that the nth delimiter of each string is aligned."

  ;;  use default format if one is not provided
  (when (not formatstr)
    (setq formatstr tabular--default-format))

  ;; check the validity of the provided format
  (when (not (tabular--validate-formatstr formatstr))
    (error "Provided format string not valid!"))

  ;;  split format string from e.g. l0 to '(l 0)
  (setq format-list (tabular--parse-formatstr formatstr))

  ;;  split the string list into list of lists split on delimiters
  (setq lines (tabular--split-string-list strings delim))

  ;; strip spaces
  (setq lines (mapcar
               (lambda (line)
                 (tabular--strip-line line gtabularize))
               lines))

  ;; Find the max length of each field
  (setq maxes (mapcar #'tabular--get-max-length lines))


  ;; let lead_blank = empty(filter(copy(lines), 'v:val[0] =~ "\\S"'))

  ;; Realign lines
  (setq lines (mapcar
               (lambda (line)
                 (tabular--concatenate-line line maxes format lead-blank gtabularize))))

  ;;  Strip trailing spaces
  (setq lines (mapcar #'string-trim-right lines)))

(defun tabular--piperange (includepat &optional filterlist)
  "Apply 0 or more filters, in sequence, to selected text in the buffer

The lines to be filtered are determined as follows:
  If the function is called with a range containing multiple lines, then
    those lines will be used as the range.
  If the function is called with no range or with a range of 1 line, then
    if GTabularize mode is being used,
      the range will not be adjusted
    if INCLUDEPAT is not specified,
      that 1 line will be filtered,
    if INCLUDEPAT is specified and that line does not match it,
      no lines will be filtered
    if INCLUDEPAT is specified and that line does match it,
      all contiguous lines above and below the specified line matching the
      pattern will be filtered.

The remaining arguments must each be a filter to apply to the text.
Each filter must either be a String evaluating to a function to be called."


  ;; function! tabular#PipeRange(includepat, ...) range
  ;;   exe a:firstline . ',' . a:lastline
  ;;       \ . 'call tabular#PipeRangeWithOptions(a:includepat, a:000, {})'
  ;; endfunction

  (tabular--piperange-with-options includepat filterlist nil))

(defun tabular--piperange-with-options (includepat filterlist options)
  "Extended version of tabular#PipeRange, which

1) Takes the list of filters as an explicit list rather than as
varargs

2) Supports passing a dictionary of options to control the
routine.

Currently, the only supported option is mode, which determines
whether to behave as :Tabularize or as :GTabularize This allows
me to add new features here without breaking API compatibility in
the future."

  ;; function! tabular#PipeRangeWithOptions(includepat, filterlist, options) range
  ;;   let top = a:firstline
  ;;   let bot = a:lastline

  ;;   let s:do_gtabularize = (get(a:options, 'mode', '') ==# 'GTabularize')

  ;;   if !s:do_gtabularize
  ;;     " In the default mode, apply range extension logic
  ;;     if a:includepat != '' && top == bot
  ;;       if top < 0 || top > line('$') || getline(top) !~ a:includepat
  ;;         return
  ;;       endif
  ;;       while top > 1 && getline(top-1) =~ a:includepat
  ;;         let top -= 1
  ;;       endwhile
  ;;       while bot < line('$') && getline(bot+1) =~ a:includepat
  ;;         let bot += 1
  ;;       endwhile
  ;;     endif
  ;;   endif

  ;;   let lines = map(range(top, bot), 'getline(v:val)')

  ;;   for filter in a:filterlist
  ;;     if type(filter) != type("")
  ;;       echoerr "PipeRange: Bad filter: " . string(filter)
  ;;     endif

  ;;     call s:FilterString(lines, filter)

  ;;     unlet filter
  ;;   endfor

  ;;   call s:SetLines(top, bot - top + 1, lines)
  ;; endfunction
  )

;; " Part of the public interface so interested pipelines can query this and
;; " adjust their behavior appropriately.
;; function! tabular#DoGTabularize()
;;   return s:do_gtabularize
;; endfunction

;; " Dictionary of command name to command
;; let s:TabularCommands = {}

;; " Generate tab completion list for :Tabularize                            {{{2
;; " Return a list of commands that match the command line typed so far.
;; " NOTE: Tries to handle commands with spaces in the name, but Vim doesn't seem
;; "       to handle that terribly well... maybe I should give up on that.
;; function! s:CompleteTabularizeCommand(argstart, cmdline, cursorpos)
;;   let names = keys(s:TabularCommands)
;;   if exists("b:TabularCommands")
;;     let names += keys(b:TabularCommands)
;;   endif

;;   let cmdstart = substitute(a:cmdline, '^\s*\S\+\s*', '', '')

;;   return filter(names, 'v:val =~# ''^\V'' . escape(cmdstart, ''\'')')
;; endfunction

;; " Choose the proper command map from the given command line               {{{2
;; " Returns [ command map, command line with leading <buffer> removed ]
;; function! s:ChooseCommandMap(commandline)
;;   let map = s:TabularCommands
;;   let cmd = a:commandline

;;   if cmd =~# '^<buffer>\s\+'
;;     if !exists('b:TabularCommands')
;;       let b:TabularCommands = {}
;;     endif
;;     let map = b:TabularCommands
;;     let cmd = substitute(cmd, '^<buffer>\s\+', '', '')
;;   endif

;;   return [ map, cmd ]
;; endfunction

;; " Parse '/pattern/format' into separate pattern and format parts.         {{{2
;; " If parsing fails, return [ '', '' ]
;; function! s:ParsePattern(string)
;;   if a:string[0] != '/'
;;     return ['','']
;;   endif

;;   let pat = '\\\@<!\%(\\\\\)\{-}\zs/' . tabular#ElementFormatPattern() . '*$'
;;   let format = matchstr(a:string[1:-1], pat)
;;   if !empty(format)
;;     let format = format[1 : -1]
;;     let pattern = a:string[1 : -len(format) - 2]
;;   else
;;     let pattern = a:string[1 : -1]
;;   endif

;;   return [pattern, format]
;; endfunction

;; " Split apart a list of | separated expressions.                          {{{2
;; function! s:SplitCommands(string)
;;   if a:string =~ '^\s*$'
;;     return []
;;   endif

;;   let end = match(a:string, "[\"'|]")

;;   " Loop until we find a delimiting | or end-of-string
;;   while end != -1 && (a:string[end] != '|' || a:string[end+1] == '|')
;;     if a:string[end] == "'"
;;       let end = match(a:string, "'", end+1) + 1
;;       if end == 0
;;         throw "No matching end single quote"
;;       endif
;;     elseif a:string[end] == '"'
;;       " Find a " preceded by an even number of \ (or 0)
;;       let pattern = '\%(\\\@<!\%(\\\\\)*\)\@<="'
;;       let end = matchend(a:string, pattern, end+1) + 1
;;       if end == 0
;;         throw "No matching end double quote"
;;       endif
;;     else " Found ||
;;       let end += 2
;;     endif

;;     let end = match(a:string, "[\"'|]", end)
;;   endwhile

;;   if end == 0 || a:string[0 : end - (end > 0)] =~ '^\s*$'
;;     throw "Empty element"
;;   endif

;;   if end == -1
;;     let rv = [ a:string ]
;;   else
;;     let rv = [ a:string[0 : end-1] ] + s:SplitCommands(a:string[end+1 : -1])
;;   endif

;;   return rv
;; endfunction

;; " Public Things                                                           {{{1

;; " Command associating a command name with a simple pattern command        {{{2
;; " AddTabularPattern[!] [<buffer>] name /pattern[/format]
;; "
;; " If <buffer> is provided, the command will only be available in the current
;; " buffer, and will be used instead of any global command with the same name.
;; "
;; " If a command with the same name and scope already exists, it is an error,
;; " unless the ! is provided, in which case the existing command will be
;; " replaced.
;; "
;; " pattern is a regex describing the delimiter to be used.
;; "
;; " format describes the format pattern to be used.  The default will be used if
;; " none is provided.
;; com! -nargs=+ -bang AddTabularPattern
;;    \ call AddTabularPattern(<q-args>, <bang>0)

;; function! AddTabularPattern(command, force)
;;   try
;;     let [ commandmap, rest ] = s:ChooseCommandMap(a:command)

;;     let name = matchstr(rest, '.\{-}\ze\s*/')
;;     let pattern = substitute(rest, '.\{-}\s*\ze/', '', '')

;;     let [ pattern, format ] = s:ParsePattern(pattern)

;;     if empty(name) || empty(pattern)
;;       throw "Invalid arguments!"
;;     endif

;;     if !a:force && has_key(commandmap, name)
;;       throw string(name) . " is already defined, use ! to overwrite."
;;     endif

;;     let command = "tabular#TabularizeStrings(a:lines, " . string(pattern)

;;     if !empty(format)
;;       let command .=  ", " . string(format)
;;     endif

;;     let command .= ")"

;;     let commandmap[name] = { 'pattern' : pattern, 'commands' : [ command ] }
;;   catch
;;     echohl ErrorMsg
;;     echomsg "AddTabularPattern: " . v:exception
;;     echohl None
;;   endtry
;; endfunction

;; " Command associating a command name with a pipeline of functions         {{{2
;; " AddTabularPipeline[!] [<buffer>] name /pattern/ func [ | func2 [ | func3 ] ]
;; "
;; " If <buffer> is provided, the command will only be available in the current
;; " buffer, and will be used instead of any global command with the same name.
;; "
;; " If a command with the same name and scope already exists, it is an error,
;; " unless the ! is provided, in which case the existing command will be
;; " replaced.
;; "
;; " pattern is a regex that will be used to determine which lines will be
;; " filtered.  If the cursor line doesn't match the pattern, using the command
;; " will be a no-op, otherwise the cursor and all contiguous lines matching the
;; " pattern will be filtered.
;; "
;; " Each 'func' argument represents a function to be called.  This function
;; " will have access to a:lines, a List containing one String per line being
;; " filtered.
;; com! -nargs=+ -bang AddTabularPipeline
;;    \ call AddTabularPipeline(<q-args>, <bang>0)

;; function! AddTabularPipeline(command, force)
;;   try
;;     let [ commandmap, rest ] = s:ChooseCommandMap(a:command)

;;     let name = matchstr(rest, '.\{-}\ze\s*/')
;;     let pattern = substitute(rest, '.\{-}\s*\ze/', '', '')

;;     let commands = matchstr(pattern, '^/.\{-}\\\@<!\%(\\\\\)\{-}/\zs.*')
;;     let pattern = matchstr(pattern, '/\zs.\{-}\\\@<!\%(\\\\\)\{-}\ze/')

;;     if empty(name) || empty(pattern)
;;       throw "Invalid arguments!"
;;     endif

;;     if !a:force && has_key(commandmap, name)
;;       throw string(name) . " is already defined, use ! to overwrite."
;;     endif

;;     let commandlist = s:SplitCommands(commands)

;;     if empty(commandlist)
;;       throw "Must provide a list of functions!"
;;     endif

;;     let commandmap[name] = { 'pattern' : pattern, 'commands' : commandlist }
;;   catch
;;     echohl ErrorMsg
;;     echomsg "AddTabularPipeline: " . v:exception
;;     echohl None
;;   endtry
;; endfunction

;; " Tabularize /pattern[/format]                                            {{{2
;; " Tabularize name
;; "
;; " Align text, either using the given pattern, or the command associated with
;; " the given name.
;; com! -nargs=* -range -complete=customlist,<SID>CompleteTabularizeCommand
;;    \ Tabularize <line1>,<line2>call Tabularize(<q-args>)

;; function! Tabularize(command, ...) range
;;   let piperange_opt = {}
;;   if a:0
;;     let piperange_opt = a:1
;;   endif

;;   if empty(a:command)
;;     if !exists("s:last_tabularize_command")
;;       echohl ErrorMsg
;;       echomsg "Tabularize hasn't been called yet; no pattern/command to reuse!"
;;       echohl None
;;       return
;;     endif
;;   else
;;     let s:last_tabularize_command = a:command
;;   endif

;;   let command = s:last_tabularize_command

;;   let range = a:firstline . ',' . a:lastline

;;   try
;;     let [ pattern, format ] = s:ParsePattern(command)

;;     if !empty(pattern)
;;       let cmd  = "tabular#TabularizeStrings(a:lines, " . string(pattern)

;;       if !empty(format)
;;         let cmd .= "," . string(format)
;;       endif

;;       let cmd .= ")"

;;       exe range . 'call tabular#PipeRangeWithOptions(pattern, [ cmd ], '
;;                       \ . 'piperange_opt)'
;;     else
;;       if exists('b:TabularCommands') && has_key(b:TabularCommands, command)
;;         let usercmd = b:TabularCommands[command]
;;       elseif has_key(s:TabularCommands, command)
;;         let usercmd = s:TabularCommands[command]
;;       else
;;         throw "Unrecognized command " . string(command)
;;       endif

;;       exe range . 'call tabular#PipeRangeWithOptions(usercmd["pattern"], '
;;                       \ . 'usercmd["commands"], piperange_opt)'
;;     endif
;;   catch
;;     echohl ErrorMsg
;;     echomsg "Tabularize: " . v:exception
;;     echohl None
;;     return
;;   endtry
;; endfunction

;; " GTabularize /pattern[/format]                                           {{{2
;; " GTabularize name
;; "
;; " Align text on only matching lines, either using the given pattern, or the
;; " command associated with the given name.  Mnemonically, this is similar to
;; " the :global command, which takes some action on all rows matching a pattern
;; " in a range.  This command is different from normal :Tabularize in 3 ways:
;; "   1) If a line in the range does not match the pattern, it will be left
;; "      unchanged, and not in any way affect the outcome of other lines in the
;; "      range (at least, normally - but Pipelines can and will still look at
;; "      non-matching rows unless they are specifically written to be aware of
;; "      tabular#DoGTabularize() and handle it appropriately).
;; "   2) No automatic range determination - :Tabularize automatically expands
;; "      a single-line range (or a call with no range) to include all adjacent
;; "      matching lines.  That behavior does not make sense for this command.
;; "   3) If called without a range, it will act on all lines in the buffer (like
;; "      :global) rather than only a single line
;; com! -nargs=* -range=% -complete=customlist,<SID>CompleteTabularizeCommand
;;    \ GTabularize <line1>,<line2>
;;    \ call Tabularize(<q-args>, { 'mode': 'GTabularize' } )

(provide 'tabular)
