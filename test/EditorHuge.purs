module Test.EditorHuge
  ( string
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldMap)

string :: String
string =
  stringStart <> stringMiddle <> stringEnd

stringStart :: String
stringStart =
  """(let
  ((midnight-plus-macros
    '(
"""

stringMiddle :: String
stringMiddle =
  foldMap
    (\n -> "(define foo" <> show n <> example)
    (1 .. 1000)
  where
  example :: String
  example =
    """
  'impl
    (lambda (editor)
      (let
        ((cursor
          (string-concat
            (list
              "cursor: "
              (int->string (grid-posn-column (editor-cursor editor)))
              ", "
              (int->string (grid-posn-row (editor-cursor editor))))))

          (line-count
            (string-append
              "lines: "
              (int->string (editor-line-count editor)))))
        (string-pad-center
          80
          cursor
          line-count))))
"""

stringEnd :: String
stringEnd =
  """; ------------------------------------------------------------------------------
;
;      MIDNIGHT System
;
;          Welcome.
;
; Try changing the "-" in the definition below to ">"
; and then pressing ctrl-ENTER.
;
(define bottom-status-bar
  'impl
    (string-repeat 80 "-"))

(define status-line
  'impl
    (lambda (editor)
      (let
        ((cursor
          (string-concat
            (list
              "cursor: "
              (int->string (grid-posn-column (editor-cursor editor)))
              ", "
              (int->string (grid-posn-row (editor-cursor editor))))))

          (line-count
            (string-append
              "lines: "
              (int->string (editor-line-count editor)))))
        (string-pad-center
          80
          cursor
          line-count))))

(define view-status-area
  'impl
    (lambda (editor)
      (list
        bottom-status-bar
        (status-line editor))))

; ------------------------------------------------------------------------------
; main

(define main
  'impl
    (lambda (input)
      (case input
        ('system-input-start-with-editor-contents src)
          (startup src)

        ('system-input-keypress store ephem k)
          (step-from-ephem store ephem k)

        ('system-input-start-with-store store)
          (start-with-store store))))

(define startup
  'impl
    (lambda (src)
      (let
        ((starting-store (string->editor src)))
        (list
          'output-normal
          (view starting-store)
          starting-store
          (dict-singleton 'step step))))) ; ephem

(define step
  'impl
    (lambda (store ephem k)
      (if
        (restart-key? k)
        (restart store ephem k)
        (let
          ((new-store (update k store)))
          (list
            'output-normal
            (view new-store)
            new-store
            ephem)))))

(define step-from-ephem
  'impl
    (lambda (store ephem k)
      ((dict-lookup-or-crash 'step ephem) store ephem k)))

(define start-with-store
  'impl
    (lambda (store)
      (list
        'output-normal
        (view store)
        store
        (dict-singleton 'step step)))) ; as ephem

(define restart-key?
  'impl
    (lambda (k)
      ; ctrl-<ENTER>
      (and (eq? (car k) 'key-enter) (eq? (cadr k) 'ctrl-or-meta))))

(define restart
  'impl
    (lambda (store ephem k)
      (let
        ((src (text->string (editor-text store))))
        (case (trace-bench 'parse (lambda (_) (sexp-parse src)))
          ('error e)
            (crash e)

          ('ok sexp)
            ; Previously:
            ;
            ; (let
            ;   ((arg (list 'system-input-start-with-store store)))
            ;   (eval (list sexp (list 'quote arg))))))))
            ;
            (let
              ((f (eval sexp))
               (arg (list 'system-input-start-with-store store)))
              (f arg))))))

; ------------------------------------------------------------------------------
; view

(define display-size-rows
  'impl
    28) ; actually 30, but 2 are taken up the the status area

(define view
  'impl
    (lambda (editor)
      (list
        (view-cursor (editor-viewport-row editor) (editor-cursor editor))
        (list
          'text
          (list-append
            (view-lines (editor-viewport-row editor) (editor-text editor))
            (view-status-area editor))))))

(define view-cursor
  'impl
    (lambda (viewport-row cursor)
      (list
        'cursor-position
        (grid-posn-column cursor)
        (- (+ 1 (grid-posn-row cursor)) viewport-row))))

(define view-lines
  'impl
    (lambda (viewport-row text)
      (list-take
        display-size-rows
        (list-drop (nat-decrement viewport-row) text))))

; ------------------------------------------------------------------------------
; update

(define update
  'impl
    (let
      ((csc
        (lambda (ed_)
          (editor-set-shadow-cursor-column none ed_))))

      (lambda (keypress ed)
        (case keypress
          ('key-char        modified c) (csc (editor-insert-char c ed))
          ('key-enter       modified)   (csc (editor-insert-newline ed))
          ('key-backspace   modified)   (csc (editor-backspace ed))
          ('key-delete      modified)   (csc (editor-delete ed))
          ('key-arrow-up    modified)   (arrow-up ed)
          ('key-arrow-down  modified)   (arrow-down ed)
          ('key-arrow-left  modified)   (csc (arrow-left ed))
          ('key-arrow-right modified)   (csc (arrow-right ed))
          ('key-page-up     modified)   (page-up ed)
          ('key-page-down   modified)   (page-down ed)
          ('key-tab         modified)   (csc (insert-tab ed))
          ('key-home        modified)   (csc (cursor-to-start-of-line ed))
          ('key-end         modified)   (csc (cursor-to-end-of-line ed))))))

(define insert-tab
  'impl
    (lambda (ed)
      (editor-insert-char
        codepoint-space
        (editor-insert-char
          codepoint-space
          ed))))

(define editor-insert-char
  'impl
    (lambda (c ed)
      (let
        ((cursor (editor-cursor ed)))
        (editor-set-cursor
          (grid-posn-modify-column increment cursor)
          (editor-modify-text
            (lambda (text)
              (text-insert-at-location cursor c text))
            ed)))))

(define editor-insert-newline
  'impl
    (lambda (ed)
      (let
        ((cursor (editor-cursor ed))
         (row (grid-posn-row cursor)))
        (editor-modify-line-count
          increment
          (editor-set-cursor
            (column-and-row-to-grid-posn 1 (increment row))
            (editor-modify-text
              (lambda (text) (text-insert-newline cursor text))
              ed))))))

(define editor-backspace
  'impl
    (lambda (ed)
      (cond
        ((> (grid-posn-column (editor-cursor ed)) 1)
          (editor-backspace-delete-char ed))

        ((> (grid-posn-row (editor-cursor ed)) 1)
          (editor-backspace-delete-line ed))

        ('t
          ed))))

(define editor-backspace-delete-char
  'impl
    (lambda (ed)
      (arrow-left
        (editor-modify-text
          (lambda (text)
            (text-delete-at-location
              (grid-posn-modify-column decrement (editor-cursor ed))
              text))
          ed))))

(define editor-backspace-delete-line
  'impl
    (lambda (ed)
      (editor-modify-line-count
        decrement
        (editor-modify-text
          (lambda (text)
            (text-combine-line-with-next
              (decrement (grid-posn-row (editor-cursor ed)))
              text))
          (cursor-to-end-of-line
            (editor-modify-cursor
              (lambda (cursor) (grid-posn-modify-row decrement cursor))
              ed))))))

(define editor-delete
  'impl
    (lambda (ed)
      (cond
        ((>= (current-line-length ed) (grid-posn-column (editor-cursor ed)))
          (editor-delete-char ed))

        ('t
          ed))))

(define editor-delete-char
  'impl
    (lambda (ed)
      (editor-modify-text
        (lambda (text) (text-delete-at-location (editor-cursor ed) text))
        ed)))

(define arrow-up
  'impl
    (lambda (ed)
      (cursor-up 1 ed)))

(define arrow-down
  'impl
    (lambda (ed)
      (cursor-down 1 ed)))

(define arrow-left
  'impl
    (lambda (ed)
      (editor-modify-cursor
        (lambda (cursor)
          (grid-posn-modify-column
            (lambda (column) (max 1 (decrement column)))
            cursor))
        ed)))

(define arrow-right
  'impl
    (lambda (ed)
      (let
        ((current-line-length (string-length (editor-current-line ed)))
         (cursor-col (grid-posn-column (editor-cursor ed))))
        (if
          (> cursor-col current-line-length)
          ed
          (editor-modify-cursor
            (lambda (cursor)
              (grid-posn-modify-column
                (lambda (column) (increment column)) cursor))
            ed)))))

(define page-up
  'impl
    (lambda (ed)
      (cursor-up (- display-size-rows 2) ed)))

(define page-down
  'impl
    (lambda (ed)
      (cursor-down (- display-size-rows 2) ed)))

(define cursor-up
  'impl
    (lambda (n ed)
      (maybe-raise-viewport
        (adjust-row-and-shadow-cursor
          (grid-posn-column (editor-cursor ed))
          (editor-modify-cursor
            (lambda (cursor)
              (grid-posn-modify-row (lambda (row) (max 1 (- row n))) cursor))
            ed)))))

(define maybe-raise-viewport
  'impl
    (lambda (editor)
      (let
        ((cursor-row (grid-posn-row (editor-cursor editor)))
         (viewport-row (editor-viewport-row editor)))
        (cond
          ; cursor offscreen high
          ((< cursor-row viewport-row)
            (editor-set-viewport-row cursor-row editor))

          ('t editor)))))

(define cursor-down
  'impl
    (lambda (n ed)
      (maybe-lower-viewport
        (adjust-row-and-shadow-cursor
          (grid-posn-column (editor-cursor ed))
          (editor-modify-cursor
            (lambda (cursor)
              ; TODO: don't allow below the end of the text
              (grid-posn-modify-row (lambda (row) (+ row n)) cursor))
            ed)))))

; For example given the following rows:
;
; 1. long-row-with-the-cursor-at-the-end_
; 2. short-row
; 3. long-row-number-two-aaaaaaaaaaaaaaaaaaaaaaa
;
; ...with the cursor at the end of the first row.
;
; If we press the down arrow, we want the cursor to jump
; to the end of row 2.
;
; If we then press it again we want it to jump to row 3,
; but to the same column it started at originally from row 1.
(define adjust-row-and-shadow-cursor
  'impl
    (lambda (column-before-vertical-move ed)
      (let
        ((desired-column (option-default
                           (editor-shadow-cursor-column ed)
                           column-before-vertical-move))

         (new-line-length (string-length (editor-current-line ed))))

        (if
          (> desired-column (+ 1 new-line-length))
          (editor-modify-cursor
            (lambda (cursor)
              (grid-posn-set-column (+ 1 new-line-length) cursor))
            (editor-set-shadow-cursor-column (some desired-column) ed))
          (editor-modify-cursor
            (lambda (cursor)
              (grid-posn-set-column desired-column cursor))
            (editor-set-shadow-cursor-column none ed))))))

(define maybe-lower-viewport
  'impl
    (lambda (editor)
      (let
        ((cursor-row (grid-posn-row (editor-cursor editor)))
         (viewport-row (editor-viewport-row editor))

         ; If the cursor is on the last line of the viewport or below
         ; then we move the viewport down.
         ;
         ; So for example if the viewport is on line 1 and is 10 rows high,
         ; the lowest the cursor can get before we move the viewport is row 9.
         (last-cursor-in-view (- (+ viewport-row display-size-rows) 2)))
        (cond
          ; cursor offscreen low
          ((>= cursor-row last-cursor-in-view)
            (editor-set-viewport-row (- (+ cursor-row 2) display-size-rows) editor))

          ('t editor)))))

(define cursor-to-start-of-line
  'impl
    (lambda (ed)
      (editor-modify-cursor
        (lambda (cursor) (grid-posn-set-column 1 cursor))
        ed)))

(define cursor-to-end-of-line
  'impl
    (lambda (ed)
      (let
        ((current-line-length (string-length (editor-current-line ed))))
        (editor-modify-cursor
          (lambda (cursor)
            (grid-posn-set-column
              (+ 1 current-line-length)
              cursor))
          ed))))

; ------------------------------------------------------------------------------
; editor helpers

(define current-line-length
  'impl
    (lambda (ed)
      (string-length (editor-current-line ed))))

(define editor-current-line
  'impl
    (lambda (ed)
      (list-get-by-index
        (grid-posn-row (editor-cursor ed))
        (editor-text ed))))

; ------------------------------------------------------------------------------
; editor
;
; (struct editor (viewport-row cursor shadow-cursor-column text line-count))

(define string->editor
  'impl
    (lambda (str)
      (editor-new
        1
        (column-and-row-to-grid-posn 1 1)
        none
        (string->text str))))

(define editor-new
  'impl
    (lambda (viewport-row cursor shadow-cursor-column text)
      (type-tag-add
        editor-tag
        (dict-insert 'viewport-row viewport-row
          (dict-insert 'cursor cursor
            (dict-insert 'shadow-cursor-column shadow-cursor-column
              (dict-insert 'text text
                (dict-singleton 'line-count (list-length text)))))))))

(define editor-viewport-row
  'impl
    (lambda (editor)
      (dict-lookup-or-crash 'viewport-row (type-tag-get editor-tag editor))))

(define editor-cursor
  'impl
    (lambda (editor)
      (dict-lookup-or-crash 'cursor (type-tag-get editor-tag editor))))

(define editor-shadow-cursor-column
  'impl
    (lambda (editor)
      (dict-lookup-or-crash
        'shadow-cursor-column
        (type-tag-get editor-tag editor))))

(define editor-text
  'impl
    (lambda (editor)
      (dict-lookup-or-crash 'text (type-tag-get editor-tag editor))))

(define editor-line-count
  'impl
    (lambda (editor)
      (dict-lookup-or-crash 'line-count (type-tag-get editor-tag editor))))

(define editor-set-viewport-row
  'impl
    (lambda (viewport-row editor)
      (editor-modify-viewport-row (lambda (_) viewport-row) editor)))

(define editor-set-cursor
  'impl
    (lambda (cursor editor)
      (editor-modify-cursor (lambda (_) cursor) editor)))

(define editor-set-shadow-cursor-column
  'impl
    (lambda (shadow-cursor-column editor)
      (editor-modify-shadow-cursor-column
        (lambda (_) shadow-cursor-column)
        editor)))

(define editor-set-text
  'impl
    (lambda (text editor)
      (editor-modify-text (lambda (_) text) editor)))

(define editor-set-line-count
  'impl
    (lambda (count editor)
      (editor-modify-line-count (lambda (_) count) editor)))

(define editor-modify-viewport-row
  'impl
    (lambda (f editor)
      (type-tag-add
        editor-tag
        (dict-update 'viewport-row f (type-tag-get editor-tag editor)))))

(define editor-modify-cursor
  'impl
    (lambda (f editor)
      (type-tag-add
        editor-tag
        (dict-update 'cursor f (type-tag-get editor-tag editor)))))

(define editor-modify-shadow-cursor-column
  'impl
    (lambda (f editor)
      (type-tag-add
        editor-tag
        (dict-update
          'shadow-cursor-column
          f
          (type-tag-get editor-tag editor)))))

(define editor-modify-text
  'impl
    (lambda (f editor)
      (type-tag-add
        editor-tag
        (dict-update 'text f (type-tag-get editor-tag editor)))))

(define editor-modify-line-count
  'impl
    (lambda (f editor)
      (type-tag-add
        editor-tag
        (dict-update 'line-count f (type-tag-get editor-tag editor)))))

(define editor-tag
  'impl
    'editor-tag)

; ------------------------------------------------------------------------------
; text

; a list of strings
; where none of those strings contains newlines

(define string->text
  'examples
    (
      (string->text "abc")
        ((string-tag (97 98 99)))

      (string->text "abc\n")
        ((string-tag (97 98 99)) (string-tag ()))

      (string->text "ab\nc")
        ((string-tag (97 98)) (string-tag (99)))

      (string->text (list->string (list 97 98 codepoint-newline 99))) ; ab\nc
        ((string-tag (97 98)) (string-tag (99)))
    )
  'impl
    (lambda (str)
      (string-split-on (lambda (c) (= c codepoint-newline)) str)))

(define text->string
  'examples
    (
      (text->string (list "abc"))
        (string-tag (97 98 99))

      (text->string (list "abc" ""))
        (string-tag (97 98 99 10))

      (text->string (list "ab" "c"))
        (string-tag (97 98 10 99))
    )
  'impl
    (lambda (strs)
      (string-concat
        (list-intersperse (codepoint->string codepoint-newline) strs))))

(define text-insert-at-location
  'impl
    (lambda (grid-posn c strs)
      (list-map-by-index
        (grid-posn-row grid-posn)
        (lambda (str)
          (string-insert-at-offset
            (nat-decrement (grid-posn-column grid-posn))
            c
            str))
        strs)))

(define text-delete-at-location
  'impl
    (lambda (grid-posn strs)
      (list-map-by-index
        (grid-posn-row grid-posn)
        (lambda (str)
          (type-tag-map
            string-tag
            (lambda (xs)
              (list-drop-at-offset
                (nat-decrement (grid-posn-column grid-posn))
                xs))
            str))
        strs)))

(define text-combine-line-with-next
  'examples
    (
      (text-combine-line-with-next 1 (list "a" "b"))
        ((string-tag (97 98)))
    )
  'impl
    (lambda (n text)
      (let
        ((before-and-after (list-split-at-offset (nat-decrement n) text))
         (all-before (car before-and-after))
         (targets-and-after (cadr before-and-after))
         (first-target (car targets-and-after))
         (second-target (cadr targets-and-after))
         (rest (cddr targets-and-after)))
        (list-append
          all-before
          (cons
            (string-append first-target second-target)
            rest)))))

(define text-insert-newline
  'examples
    (
      (text-insert-newline (column-and-row-to-grid-posn 1 1) (list "a"))
        ((string-tag ()) (string-tag (97)))
    )
  'impl
    (lambda (grid-posn strs)
      (let
        ((lines-before-split-and-rest
          (list-split-at-offset
            (nat-decrement (grid-posn-row grid-posn))
            strs))
         (lines-before-split (car lines-before-split-and-rest))
         (split-line-and-after (cadr lines-before-split-and-rest)))
        (if
          (list-empty? split-line-and-after)
          (list-append lines-before-split (list ""))
          (let
            ((str-split-in-two
              (string-split-at-offset
                (nat-decrement (grid-posn-column grid-posn))
                (car split-line-and-after))))
            (list-concat
              (list
                lines-before-split
                str-split-in-two
                (cdr split-line-and-after))))))))

; ------------------------------------------------------------------------------
; grid-posn
;
; (struct grid-posn (column row))

(define column-and-row-to-grid-posn
  'impl
    (lambda (col row)
      (type-tag-add
        grid-posn-tag
        (list col row))))

(define grid-posn-column
  'impl
    (lambda (grid-posn)
      (car (type-tag-get grid-posn-tag grid-posn))))

(define grid-posn-row
  'impl
    (lambda (grid-posn)
      (cadr (type-tag-get grid-posn-tag grid-posn))))

(define grid-posn-set-column
  'impl
    (lambda (col grid-posn)
      (grid-posn-modify-column (lambda (_) col) grid-posn)))

(define grid-posn-set-row
  'impl
    (lambda (row grid-posn)
      (grid-posn-modify-row (lambda (_) row) grid-posn)))

(define grid-posn-modify-column
  'impl
    (lambda (f grid-posn)
      (let
        ((col-and-row (type-tag-get grid-posn-tag grid-posn)))
        (type-tag-add grid-posn-tag (list-map-first f col-and-row)))))

(define grid-posn-modify-row
  'impl
    (lambda (f grid-posn)
      (let
        ((col-and-row (type-tag-get grid-posn-tag grid-posn)))
        (type-tag-add grid-posn-tag (list-map-second f col-and-row)))))

(define grid-posn-tag
  'impl
    'grid-posn-tag)

; ------------------------------------------------------------------------------
; midnight parser
;
; This is the "faster" parser which combines lexing and parsing.
;
; There's also a reference implementation in the next section
; that keeps them separate.

(define eval-string
  'examples
    (
      (eval-string "'a") a
      (eval-string "((lambda (x) x) 'a)") a
    )
  'impl
    (lambda (str)
      (case (sexp-parse str)
        ('error e)
          (crash e)

        ('ok sexp)
          (eval sexp))))

(define sexp-parse
  'examples
    (
      (sexp-parse "()") (ok ())
      (sexp-parse "'()") (ok '())
      (sexp-parse "(())") (ok (()))
      (sexp-parse "(() ())") (ok (() ()))
      (sexp-parse "abc") (ok abc)
      (sexp-parse "car") (ok car)
      (sexp-parse "'abc") (ok (quote abc))
      (sexp-parse "1") (ok 1)
      (sexp-parse "-1") (ok -1)
      (sexp-parse "\"a\"") (ok '(string-tag (97)))
      (sexp-parse "\"abc\"") (ok '(string-tag (97 98 99)))
      ; \n
      (sexp-parse '(string-tag (34 92 110 34))) (ok '(string-tag (10)))
      (sexp-parse "a ;") (ok a)
      (sexp-parse "a ; abc") (ok a)
      (sexp-parse "a ; abc\n\n") (ok a)
    )
  'impl
    (lambda (str)
      (case (sexp-tokenize-and-parse (type-tag-get string-tag str))
        ('error e)
          (error e)

        ('ok sexp-and-remaining)
          (if
            (list-empty? (cadr sexp-and-remaining))
            (ok (car sexp-and-remaining))
            (error
              'sexp-parse
              'tokens-remain
              (cadr sexp-and-remaining))))))

(define sexp-tokenize-and-parse
  'impl
    (lambda (codepoints)
      (if
        (list-empty? codepoints)
        (error 'sexp-tokenize-and-parse 'end-of-input)
        (let
          ((next (car codepoints)))
          (cond
            ((= next 32) ; ` `, codepoint-space
              (sexp-tokenize-and-parse (cdr codepoints)))

            ((= next 40) ; `(`, codepoint-begin-paren
              (sexp-tokenize-and-parse-list (cdr codepoints)))

            ((= next 10) ; `\n`, codepoint-newline
              (sexp-tokenize-and-parse (cdr codepoints)))

            ((= next 39) ; `'`, codepoint-single-quote
              (sexp-tokenize-and-parse-quoted (cdr codepoints)))

            ((= next 59) ; `;`, codepoint-colon
              (sexp-tokenize-and-parse (chomp-comment (cdr codepoints))))

            ((= next 34) ; `"`, codepoint-double-quote
              (let
                ((token-and-remaining (sexp-token-string (cdr codepoints)))
                 (token (car token-and-remaining)))
                (ok
                  (list
                    (cadr token)
                    (chomp-comment-and-space (cadr token-and-remaining))))))

            ((= next 41) ; `)`, codepoint-end-paren
              (error 'sexp-tokenize-and-parse 'unexpected-end-paren))

            ('t
              (let
                ; Include the value of `next` in the arg,
                ; unlike all of the above which pass `(cdr codepoints)`:
                ((token-and-remaining (sexp-symbol-or-int codepoints))
                 (token (car token-and-remaining)))
                (ok
                  (list
                    (cadr token)
                    (chomp-comment-and-space
                      (cadr token-and-remaining)))))))))))

; In the separate lexer/parser impl, chomping whitespace after tokens
; is handled by the lexer.
;
; Here we have to call this after sucessfully parsing any sub-expression,
; either lists or atoms.
;
; If we don't then (eg) `sexp-tokenize-and-parse-list` will fail
; when there's a space before an end paren.
; It won't realize it's hit the end
; and will send the codepoints starting with ` )` to `sexp-tokenize-and-parse`.
(define chomp-comment-and-space
  'impl
    (lambda (codepoints)
      (if
        (list-empty? codepoints)
        '()
        (let
          ((next (car codepoints)))
          (cond
            ((= next 32) ; ` `, codepoint-space
              (chomp-comment-and-space (cdr codepoints)))

            ((= next 10) ; `\n`, codepoint-newline
              (chomp-comment-and-space (cdr codepoints)))

            ((= next 59) ; `;`, codepoint-colon
              (chomp-comment-and-space (chomp-comment (cdr codepoints))))

            ('t
              codepoints))))))

; Begin paren has already been taken off the argument.
(define sexp-tokenize-and-parse-list
  'impl
    (lambda (codepoints)
      (let
        ((loop
          (lambda (acc codepoints)
            (if
              (list-empty? codepoints)
              (error 'sexp-tokenize-and-parse-list 'no-closing-paren)
              (if
                (= (car codepoints) 41) ; `)`, codepoint-end-paren
                (ok
                  (list
                    (list-reverse acc)
                    (chomp-comment-and-space (cdr codepoints))))
                (case (sexp-tokenize-and-parse codepoints)
                  ('error e)
                    (error e)

                  ('ok sexp-and-remaining)
                    (loop
                      (cons (car sexp-and-remaining) acc)
                      (cadr sexp-and-remaining))))))))
        (loop '() codepoints))))

(define sexp-tokenize-and-parse-quoted
  'impl
    (lambda (codepoints)
      (case (sexp-tokenize-and-parse codepoints)
        ('error e)
          (error e)

        ('ok sexp-and-remaining)
          (ok
            (list
              (list 'quote (car sexp-and-remaining))
              (cadr sexp-and-remaining))))))

; ------------------------------------------------------------------------------
; midnight reference lexer and parser
;
; Some of these functions are used by the faster parser above.

(define sexp-reference-parse
  'examples
    (
      (sexp-reference-parse "()") (ok ())
      (sexp-reference-parse "(())") (ok (()))
      (sexp-reference-parse "(() ())") (ok (() ()))
      (sexp-reference-parse "abc") (ok abc)
      (sexp-reference-parse "1") (ok 1)
      (sexp-reference-parse "\"a\"") (ok '(string-tag (97)))
      (sexp-reference-parse "'abc") (ok (quote abc))
    )
  'impl
    (lambda (str)
      (case (sexp-tokenize str)
        ('error e)
          (error e)

        ('ok tokens)
          (case (sexp-parse-from-tokens tokens)
            ('error e)
              (error e)

            ('ok sexp-and-remaining)
              (if
                (list-empty? (cadr sexp-and-remaining))
                (ok (car sexp-and-remaining))
                (error
                  'sexp-reference-parse
                  'tokens-remain
                  (cadr sexp-and-remaining)))))))

(define sexp-parse-from-tokens
  'impl
    (lambda (tokens)
      (if
        (list-empty? tokens)
        (error 'sexp-parse-from-tokens 'empty-list)
        (case (car tokens)
          ('token-begin-paren)
            (sexp-parse-list (cdr tokens))

          ('token-quote)
            (sexp-quoted (cdr tokens))

          ('token-symbol sym)
            (ok (list sym (cdr tokens)))

          ('token-int n)
            (ok (list n (cdr tokens)))

          ('token-string s)
            (ok (list s (cdr tokens)))))))

(define sexp-quoted
  'impl
    (lambda (tokens)
      (case (sexp-parse-from-tokens tokens)
        ('error e)
          (error e)

        ('ok sexp-and-remaining)
          (ok
            (list
              (list 'quote (car sexp-and-remaining))
              (cadr sexp-and-remaining))))))

; Begin paren has already been taken off the argument.
(define sexp-parse-list
  'impl
    (lambda (tokens)
      (let
        ((loop
          (lambda (acc tokens)
            (if
              (list-empty? tokens)
              (error 'sexp-parse-list 'no-closing-paren)
              (if
                (eq? (car tokens) '(token-end-paren))
                (ok (list (list-reverse acc) (cdr tokens)))
                (case (sexp-parse-from-tokens tokens)
                  ('error e)
                    (error e)

                  ('ok sexp-and-remaining)
                    (loop
                      (cons (car sexp-and-remaining) acc)
                      (cadr sexp-and-remaining))))))))
        (loop '() tokens))))

(define sexp-tokenize
  'examples
    (
      (sexp-tokenize "(") (ok ((token-begin-paren)))
      (sexp-tokenize ")") (ok ((token-end-paren)))
      (sexp-tokenize "()")
        (ok ((token-begin-paren) (token-end-paren)))
      (sexp-tokenize "'") (ok ((token-quote)))
      (sexp-tokenize " ") (ok ())
      (sexp-tokenize "\n") (ok ())
      (sexp-tokenize "a") (ok ((token-symbol a)))
      (sexp-tokenize "abc") (ok ((token-symbol abc)))
      (sexp-tokenize "car") (ok ((token-symbol car)))
      (sexp-tokenize "1") (ok ((token-int 1)))
      (sexp-tokenize "-1") (ok ((token-int -1)))
      (sexp-tokenize "\"a\"")
        (ok ((token-string '(string-tag (97)))))
      (sexp-tokenize "\"abc\"")
        (ok ((token-string '(string-tag (97 98 99)))))
      ; \n
      (sexp-tokenize '(string-tag (34 92 110 34)))
        (ok ((token-string '(string-tag (10)))))
      (sexp-tokenize ";") (ok ())
      (sexp-tokenize "; abc") (ok ())
      (sexp-tokenize "; abc\n\n") (ok ())
    )
  'impl
    (lambda (str)
      (let
        ((codepoints (type-tag-get string-tag str))
         (loop (lambda (acc xs)
           (if
             (list-empty? xs)
             (ok (list-reverse acc))
             (let
               ((x (car xs))
                (rest (cdr xs)))
               (cond
                 ((eq? x (string-to-codepoint " "))
                   (loop acc rest))

                 ((eq? x (string-to-codepoint "("))
                   (loop (cons '(token-begin-paren) acc) rest))

                 ((eq? x (string-to-codepoint ")"))
                   (loop (cons '(token-end-paren) acc) rest))

                 ((eq? x codepoint-newline)
                   (loop acc rest))

                 ((eq? x (string-to-codepoint "'"))
                   (loop (cons '(token-quote) acc) rest))

                 ((eq? x (string-to-codepoint ";"))
                   (loop acc (chomp-comment rest)))

                 ((eq? x (string-to-codepoint "\""))
                   (let
                     ((string-and-remaining (sexp-token-string rest)))
                       (loop
                         (cons (car string-and-remaining) acc)
                         (cadr string-and-remaining))))

                 ('t
                   (let
                     ((atom-and-remaining (sexp-symbol-or-int xs)))
                     (loop
                       (cons (car atom-and-remaining) acc)
                       (cadr atom-and-remaining))))))))))
        (loop '() codepoints))))

(define chomp-comment
  'impl
    (lambda (xs)
      (list-drop-while
        (lambda (x) (/= x 10)) ; `\n`, codepoint-newline
        xs)))

; TODO: should this check for "?
(define sexp-codepoint-used-for-syntax
  'examples
    (
      (sexp-codepoint-used-for-syntax codepoint-newline) t
      (sexp-codepoint-used-for-syntax 97) f
    )
  'impl
    (lambda (c)
      (cond
        ((= c 32) 't) ; ` `, codepoint-space
        ((= c 40) 't) ; `(`, codepoint-begin-paren
        ((= c 41) 't) ; `)`, codepoint-end-paren
        ((= c 39) 't) ; `'`, codepoint-single-quote
        ((= c 10) 't) ; `\n`, codepoint-newline
        ((= c 59) 't) ; `;`, codepoint-colon
        ('t 'f))))

; Assumes a nonempty input whose first token is atom-valid.
(define sexp-symbol-or-int
  'examples
    (
      (sexp-symbol-or-int '(97 98 99)) ((token-symbol abc) ())
      (sexp-symbol-or-int '(49))       ((token-int 1) ())
    )
  'impl
    (lambda (codepoints)
      (let
        ((taken-and-remaining
          (list-split-once-on sexp-codepoint-used-for-syntax codepoints))
         (taken (car taken-and-remaining))
         (remaining (cadr taken-and-remaining)))
        (list
          (if
            (sexp-int-sequence? taken)
            (list
              'token-int
              (string-to-int-or-crash (type-tag-add string-tag taken)))
            (list 'token-symbol (codepoints->symbol taken)))
          remaining))))

; Assumes a nonempty argument.
(define sexp-int-sequence?
  'impl
    (lambda (xs)
      (if
        (= (car xs) 45) ; `-`, codepoint-hyphen
        (if
          (list-empty? (cdr xs))
          'f
          (list-all sexp-int-codepoint? (cdr xs)))
        (list-all sexp-int-codepoint? xs))))

(define sexp-int-codepoint?
  'examples
    (
      (sexp-int-codepoint? (string-to-codepoint "0")) t
      (sexp-int-codepoint? (string-to-codepoint "1")) t
      (sexp-int-codepoint? (string-to-codepoint "a")) f
    )
  'impl
    (lambda (c)
      (and (>= c 48) (<= c 57))))

; The first `"` has already been taken off the argument.
(define sexp-token-string
  'examples
    (
      (sexp-token-string '(97 34)) ((token-string '(string-tag (97))) ())
    )
  'impl
    (lambda (xs)
      (let
        ((loop-escape-mode-off
          (lambda (acc xs)
            (cond
              ((list-empty? xs)
                (crash 'sexp-token-string-no-ending-double-quote))

              ((= (car xs) codepoint-double-quote)
                (list
                  (list 'quote (list->string (list-reverse acc)))
                  (cdr xs)))

              ((= (car xs) codepoint-backslash)
                (loop-escape-mode-on acc (cdr xs)))

              ('t ; TODO: make sure not required to be escaped
                (loop-escape-mode-off (cons (car xs) acc) (cdr xs))))))

         (loop-escape-mode-on
           (lambda (acc xs)
             ; TODO: make sure list nonempty and car can be escaped
             (cond
               ((= (car xs) 110)
                 (loop-escape-mode-off (cons 10 acc) (cdr xs)))

               ('t
                 (loop-escape-mode-off (cons (car xs) acc) (cdr xs))))))

         (str-and-remaining (loop-escape-mode-off '() xs)))
        (list
          (list 'token-string (car str-and-remaining))
          (cadr str-and-remaining)))))

; ------------------------------------------------------------------------------
; string

(define abc
  'examples
    (
      abc   (string-tag (97 98 99))
      "abc" (string-tag (97 98 99))

      ; This doesn't work because the right side is quoted:
      ; "abc" "abc"
    )
  'impl
    (list->string '(97 98 99)))

(define string-pad-center
  'examples
    (
      (string-pad-center 5 "a" "z") (string-tag (97 32 32 32 122))
    )
  'impl
    (lambda (n left right)
      (let 
        ((spaces-to-add
            (- (- n (string-length left)) (string-length right))))
        (string-concat
          (list
            left
            (string-repeat spaces-to-add " ")
            right)))))

(define string-repeat
  'examples
    (
      (string-repeat 2 "ab") (string-tag (97 98 97 98))
    )
  'impl
    (lambda (n str)
      (string-concat (list-replicate n str))))

; AKA codepoints->string
(define list->string
  'impl
    (lambda (chars)
      (type-tag-add string-tag chars)))

; AKA string->codepoints
(define string->list
  'impl
    (lambda (str)
      (type-tag-get string-tag str)))

(define string-length
  'examples
    (
      (string-length "abc") 3
    )
  'impl
    (lambda (str)
      (list-length (string->list str))))

(define string-append
  'examples
    (
      (string-append abc abc) (string-tag (97 98 99 97 98 99))
    )
  'impl
    (lambda (xs ys)
      (type-tag-add
        string-tag
        (list-append
          (type-tag-get string-tag xs)
          (type-tag-get string-tag ys)))))

(define string-concat
  'impl
    (lambda (strs)
      (list->string (list-concat (list-map string->list strs)))))

(define string-insert-at-offset
  'examples
    (
      ; NOTE: Don't switch the int args!
      (string-insert-at-offset 1 100 abc) (string-tag (97 100 98 99))
    )
  'impl
    (lambda (offset c str)
      (type-tag-map
        string-tag
        (lambda (xs) (list-insert-at-offset offset c xs))
        str)))

(define string-split-at-offset
  'examples
    (
      (string-split-at-offset 1 abc) ((string-tag (97)) (string-tag (98 99)))
    )
  'impl
    (lambda (offset str)
      (let
        ((first-and-second
          (list-split-at-offset offset (type-tag-get string-tag str))))
        (list
          (list->string (car first-and-second))
          (list->string (cadr first-and-second))))))

(define string-split-on
  'impl
    (lambda (pred? str)
      (let
        ((charlists (list-split-on pred? (type-tag-get string-tag str))))
        (list-map list->string charlists))))

(define string-to-codepoint
  'examples
    (
      (string-to-codepoint "a") 97
    )
  'impl
    (lambda (str)
      (let
        ((xs (type-tag-get string-tag str)))
        (if
          (list-empty? xs)
          (crash 'string-to-codepoint-empty)
          (if
            (list-empty? (cdr xs))
            (car xs)
            (crash 'string-to-codepoint-too-long))))))

(define string-to-int-or-crash
  'examples
    (
      (string-to-int-or-crash "1") 1
      (string-to-int-or-crash "-1") -1
    ) ; TODO: test with just "-"
  'impl
    (lambda (str)
      (let
        ((codepoints (type-tag-get string-tag str)))
        (if
          (list-empty? codepoints)
          (crash 'string-to-int-or-crash-empty-string)
          (if
            (= (car codepoints) (string-to-codepoint "-"))
            (negate
              (string-to-nat-or-crash
                (type-tag-add string-tag (cdr codepoints))))
            (string-to-nat-or-crash
              (type-tag-add string-tag codepoints)))))))

(define string-to-nat-or-crash
  'examples
    (
      (string-to-nat-or-crash "123") 123
    )
  'impl
    (lambda (str)
      (let
        ((codepoints (type-tag-get string-tag str)))
        (if
          (list-empty? codepoints)
          (crash 'string-to-nat-or-crash-empty-string)
          (list-foldl
            (lambda (n x)
              (+ (* 10 n) (codepoint-to-nat-or-crash x)))
            0
            codepoints)))))

(define string-empty
  'examples
    (
      string-empty (string-tag ())
    )
  'impl
    (type-tag-add string-tag '()))

(define string-tag
  'impl
    'string-tag)

; ------------------------------------------------------------------------------
; codepoint

(define codepoint-newline
  'impl
    10)

(define codepoint-space
  'impl
    32)

(define codepoint-backslash
  'impl
    92)

(define codepoint-double-quote
  'impl
    34)

(define codepoint-underscore
  'impl
    95)

(define codepoint-to-nat-or-crash
  'examples
    (
      (codepoint-to-nat-or-crash (string-to-codepoint "1")) 1
    )
  'impl
    (lambda (c)
      (- c 48)) ; TODO: doesn't actually crash, need to rename
  'reference-impl
    (lambda (c)
      (cond
        ((eq? c (string-to-codepoint "0")) 0)
        ((eq? c (string-to-codepoint "1")) 1)
        ((eq? c (string-to-codepoint "2")) 2)
        ((eq? c (string-to-codepoint "3")) 3)
        ((eq? c (string-to-codepoint "4")) 4)
        ((eq? c (string-to-codepoint "5")) 5)
        ((eq? c (string-to-codepoint "6")) 6)
        ((eq? c (string-to-codepoint "7")) 7)
        ((eq? c (string-to-codepoint "8")) 8)
        ((eq? c (string-to-codepoint "9")) 9))))

(define codepoint->string
  'impl
    (lambda (c)
      (list->string (list c))))

; ------------------------------------------------------------------------------
; example tests

; Takes as an argument a list of `define`s.
;
; Extract the `'examples` from them, and returns an expression that,
; if evaluated in the context of all the other `define`s, runs those examples.
(define defines->example-tests
  'examples
    (
      (defines->example-tests '((define one 'impl 1)))
        (begin-order-unspecified)

      (defines->example-tests '((define one 'examples (one 1) 'impl 1)))
        (begin-order-unspecified (assert-eq one (quote 1)))
    )
  'impl
    (lambda (defines)
      (cons 'begin-order-unspecified
        (list-map
          example-to-runnable
          (list-flatmap flat-alist-to-examples defines)))))

(define flat-alist-to-examples
  'examples
    (
      (flat-alist-to-examples '(1 2 'examples () 3 4))
        ()

      (flat-alist-to-examples '(1 2 'examples ((identity 'a) a) 3 4))
        (((identity 'a) a))
    )
  'impl
    (lambda (defines)
      (case (flat-alist-to-dict defines)
        ('error _)
          (crash (list 'flat-alist-to-examples 'define-list-not-even))

        ('ok dict)
          (case (dict-lookup ''examples dict)
            ('error e)
              '()

            ('ok flat-examples)
              (case (flat-alist-to-dict flat-examples)
                ('error e)
                  (crash (list 'flat-alist-to-examples 'example-list-not-even))

                ('ok examples)
                  (dict->alist examples))))))

(define example-to-runnable
  'examples
    (
      (example-to-runnable '((identity 'a) a)) (assert-eq (identity 'a) 'a)
    )
  'impl
    (lambda (expression-and-expected)
      (let
        ((expr (car expression-and-expected))
         ; quote!!! eg `a` becomes `'a`:
         (quoted-expected (list 'quote (cadr expression-and-expected))))
        (list 'assert-eq expr quoted-expected))))

; ------------------------------------------------------------------------------
; test macros

; If you put this test before `list` is defined you get a pretty scary error:
; `#<undef> is not a function`.
(define case-test-capture-multiple
  'examples
    (
      case-test-capture-multiple (foo bar)
    )
  'impl
    (case '(b foo bar)
      ('a x)   no
      ('b x y) (list x y)
      ('c x)   nope))

(define case-test-capture
  'examples
    (
      case-test-capture 12
    )
  'impl
    (case '(b 10)
      ('a x) (+ x 1)
      ('b x) (+ x 2)
      ('c x) (+ x 3)))

(define case-test-basic
  'examples
    (
      case-test-basic y
    )
  'impl
    (case '(b)
      ('a) x
      ('b) 'y
      ('c) z))

(define test-cond-macro
  'examples
    (
      test-cond-macro b
    )
  'impl
    (cond
      ('f a)
      ('t 'b)
      ('f c)))

(define test-or-macro
  'examples
    (
      (test-or-macro 't 't) t
      (test-or-macro 't 'f) t
      (test-or-macro 'f 't) t
      (test-or-macro 'f 'f) f
    )
  'impl
    (lambda (a b)
      (or a b)))

; (define test-and-macro
;   'examples
;     (
;       (and 't 't) t
;       (and 't 'f) f
;       (and 'f 't) f
;       (and 'f 'f) f
;     )
;   'impl
;     'stub)

(define test-and-macro
  'examples
    (
      (test-and-macro 't 't) t
      (test-and-macro 't 'f) f
      (test-and-macro 'f 't) f
      (test-and-macro 'f 'f) f
    )
  'impl
    (lambda (a b)
      (and a b)))

; ------------------------------------------------------------------------------
; test builtins

(define test-builtin-/
  'examples
    (
      (/ 5 2) 2
      (/ 5 -2) -2
      (/ 0 2) 0
      ; (/ 5 0) 2
      ; (/ 0 0) 0
    )
  'impl
    'stub)

; ------------------------------------------------------------------------------
; dict

(define dict-singleton
  'examples
    ( (dict-singleton 'a 1) (alist-tag ((a 1)))
    )
  'impl
    (lambda (k v)
      (dict-insert k v dict-empty)))

(define dict-insert
  'examples
    ( (dict-insert 'a 1 dict-empty) (alist-tag ((a 1)))
    )
  'impl
    (lambda (k v alist)
      (list-map-second
        (lambda (xs)
          (cons
            (list k v)
            (list-drop-where (lambda (x) (eq? (car x) k)) xs)))
        alist)))

(define dict-lookup-or-crash
  'impl
    (lambda (k alist)
      (case (dict-lookup k alist)
        ('error e) (crash 'dict-lookup-or-crash)
        ('ok a)    a)))

(define dict-lookup
  'examples
    (
      (dict-lookup 'a (dict-singleton 'a 1)) (ok 1)
      (dict-lookup 'a dict-empty)            (error (dict-lookup a))
    )
  'impl
    (lambda (k alist)
      (let
        ((loop
          (lambda (xs)
            (if
              (list-empty? xs)
              (error 'dict-lookup k)
              (let
                ((pair (car xs)))
                (if
                  (eq? k (car pair))
                  (ok (cadr pair))
                  (loop (cdr xs))))))))
        (loop (type-tag-get dict-tag alist)))))

(define dict-update
  'examples
    (
      (dict-update 'a increment (dict-singleton 'a 1)) (alist-tag ((a 2)))
      (dict-update 'a increment (dict-singleton 'b 1)) (alist-tag ((b 1)))
    )
  'impl
    (lambda (k f alist)
      (let
        ((res (dict-lookup k alist)))
        (if
          (eq? (car res) 'error)
          alist
          (dict-insert k (f (cadr res)) alist)))))

(define flat-alist-to-dict
  'examples
    (
      ; Hmm, order matters here:
      (flat-alist-to-dict '(a x b y)) (ok (alist-tag ((b y) (a x))))
      (flat-alist-to-dict '())        (ok (alist-tag ()))
      (flat-alist-to-dict '(a))       (error (flat-alist-to-dict unpaired a))
    )
  'impl
    (lambda (xs)
      (let
        ((loop
          (lambda (dict xs)
            (if
              (list-empty? xs)
              (ok dict)
              (if
                (list-empty? (cdr xs))
                (error 'flat-alist-to-dict 'unpaired (car xs))
                (loop (dict-insert (car xs) (cadr xs) dict) (cdr (cdr xs))))))))
        (loop dict-empty xs))))

(define dict->alist
  'impl
    (lambda (dict)
      (type-tag-get dict-tag dict)))

(define dict-empty
  'examples
    ( dict-empty (alist-tag ())
    )
  'impl
    (type-tag-add dict-tag '()))

(define dict-tag
  'impl
    'alist-tag)

; ------------------------------------------------------------------------------
; type tag

(define type-tag-add
  'examples
    (
      (type-tag-add 'set '(a b)) (set (a b))
    )
  'impl
    (lambda (tag a)
      (list tag a)))

(define type-tag-get
  'examples
    (
      (type-tag-get 'a '(a 1)) 1
    )
  'impl
    (lambda (tag tagged)
      (if
        (symbol-eq? tag (car tagged))
        (cadr tagged)
        (crash (list 'type-tag-get tag tagged)))))

(define type-tag-map
  'examples
    (
      (type-tag-map 'positive-tag increment '(positive-tag 1)) (positive-tag 2)
    )
  'impl
    (lambda (tag f tagged)
      (if
        (symbol-eq? tag (car tagged))
        (list-map-second f tagged)
        (crash 'type-tag-map))))

; ------------------------------------------------------------------------------
; error

(define error
  'impl
    (lambda xs
      (list 'error xs)))

(define ok
  'impl
    (lambda (a)
      (list 'ok a)))

; ------------------------------------------------------------------------------
; option

(define some
  'impl
    (lambda (a)
      (list 'some a)))

(define none
  'impl
    '(none))

(define option-default
  'examples
    (
      (option-default (some 'a) 'b) a
      (option-default none      'b) b
    )
  'impl
    (lambda (option def)
      (case option
        ('some a)
          a

        ('none)
          def)))

; ------------------------------------------------------------------------------
; list

(define cadr
  'impl
    (lambda (xs)
      (car (cdr xs))))

(define caddr
  'impl
    (lambda (xs)
      (car (cdr (cdr xs)))))

(define cadddr
  'impl
    (lambda (xs)
      (car (cdr (cdr (cdr xs))))))

(define cddr
  'impl
    (lambda (xs)
      (cdr (cdr xs))))

(define list
  'examples
    (
      (list 'a 'b 'c) (a b c)
      (list)          ()
    )
  'impl
    (lambda xs
      xs))

(define list-replicate
  'examples
    (
      (list-replicate 2 'a) (a a)
    )
  'impl
    (lambda (n x)
      (list-map
        (lambda (_) x)
        (list-of-n-elements n))))

(define list-of-n-elements
  'examples
    (
      (list-of-n-elements 2) (0 1)
    )
  'impl
    (lambda (n)
      (list-reverse
        (int-foldl (lambda (acc next) (cons next acc)) '() n))))

(define list-all
  'examples
    (
      (list-all nat? '(1 2))  t
      (list-all nat? '(1 -2)) f
    )
  'impl
    (lambda (pred? xs)
      (list-foldl
        (lambda (acc x)
          (and acc (pred? x)))
        't
        xs)))

(define list-length
  'examples
    (
      (list-length '())    0
      (list-length '(a))   1
      (list-length '(a b)) 2
    )
  'impl
    (lambda (xs)
      (list-foldl
        (lambda (acc x) (increment acc))
        0
        xs)))

(define list-map
  'examples
    (
      (list-map increment '(1 2)) (2 3)
    )
  'impl
    (lambda (f xs)
      (list-reverse
        (list-foldl (lambda (acc x) (cons (f x) acc)) '() xs))))

; Should this crash if there isn't a first element?
(define list-map-first
  'examples
    (
      (list-map-first increment '(1 2 3)) (2 2 3)
      (list-map-first increment '())      ()
    )
  'impl
    (lambda (f xs)
      (if
        (pair? xs)
        (cons (f (car xs)) (cdr xs))
        '())))

(define list-map-second
  'examples
    (
      (list-map-second increment '(1 2 3)) (1 3 3)
      (list-map-second increment '())      ()
    )
  'impl
    (lambda (f xs)
      (if
        (pair? xs)
        (cons (car xs) (list-map-first f (cdr xs)))
        '())))

(define list-last
  'examples
    (
      (list-last '(a b c)) c
    )
  'impl
    (lambda (xs)
      (car (list-reverse xs))))

(define list-reverse
  'examples
    (
      (list-reverse '(a b)) (b a)
    )
  'impl
    (lambda (xs)
      (list-foldl (lambda (acc x) (cons x acc)) '() xs)))

(define snoc
  'impl
    (lambda (xs x)
      (list-reverse (cons x (list-reverse xs)))))

(define list-intersperse
  'examples
    (
      (list-intersperse 'x '(a b c)) (a x b x c)
      (list-intersperse 'x '())      ()
    )
  'impl
    (lambda (sep xs)
      (if
        (list-empty? xs)
        '()
        (let
          ((rev (list-reverse xs)))
          (list-foldl
            (lambda (acc x)
              (cons x (cons sep acc)))
            (list (car rev))
            (cdr rev))))))

(define list-flatmap
  'examples
    (
      (list-flatmap (lambda (x) (list x x)) '(a b)) (a a b b)
    )
  'impl
    (lambda (f xs)
      (list-concat (list-map f xs))))

(define list-concat
  'examples
    (
      (list-concat '((a b) (c d) (e f))) (a b c d e f)
      (list-concat '((a b) () (c d)))    (a b c d)
      (list-concat '(() (a) () (b) ()))  (a b)
      (list-concat '())                  ()
    )
  'impl
    (lambda (xs)
      (list-reverse
        (list-foldl
          (lambda (acc ys)
            (list-foldl (lambda (accInner y) (cons y accInner)) acc ys))
          '()
          xs))))

(define list-append
  'examples
    (
      (list-append '(a b) '(c d)) (a b c d)
      (list-append '(a b) '())    (a b)
      (list-append '() '(a b))    (a b)
      (list-append '() '())       ()
    )
  'impl
    (lambda (xs ys)
      (list-foldl
        (lambda (acc x) (cons x acc))
        ys
        (list-reverse xs))))

(define list-get-by-index
  'examples
    (
      (list-get-by-index 2 '(a b c)) b
    )
  'impl
    (lambda (n xs)
      (car (list-drop (nat-decrement n) xs))))

(define list-take
  'examples
    (
      (list-take -1 '(a b c)) ()
      (list-take 0  '(a b c)) ()
      (list-take 1  '(a b c)) (a)
      (list-take 2  '(a b c)) (a b)
      (list-take 3  '(a b c)) (a b c)
      (list-take 4  '(a b c)) (a b c)
    )
  'impl
    (lambda (n xs)
      (car (list-split-at-offset n xs))))

(define list-take-while
  'examples
    (
      (list-take-while nat? '(1 2 3 -4 5 6)) (1 2 3)
    )
  'impl
    (lambda (pred? xs)
      (car (list-split-once-on (lambda (x) (not (pred? x))) xs))))

(define list-drop
  'examples
    (
      (list-drop -1 '(a b c)) (a b c)
      (list-drop 0  '(a b c)) (a b c)
      (list-drop 1  '(a b c)) (b c)
      (list-drop 2  '(a b c)) (c)
      (list-drop 3  '(a b c)) ()
      (list-drop 4  '(a b c)) ()
    )
  'impl
    (lambda (n xs)
      (cadr (list-split-at-offset n xs))))

(define list-drop-while
  'examples
    (
      (list-drop-while nat? '(1 2 3 -4 5 6)) (-4 5 6)
    )
  'impl
    (lambda (pred? xs)
      (cadr (list-split-once-on (lambda (x) (not (pred? x))) xs))))

(define list-drop-where
  'examples
    (
      (list-drop-where nat? '(-1 2 -3 4)) (-1 -3)
    )
  'impl
    (lambda (pred? xs)
      (list-reverse
        (list-foldl
          (lambda (acc x)
            (if
              (pred? x)
              acc
              (cons x acc)))
          '()
          xs))))

(define list-member?
  'examples
    (
      (list-member? 'a '(a b c)) t
      (list-member? 'x '(a b c)) f
      (list-member? 'a '())      f
    )
  'impl
    (lambda (x xs)
      (if
        (list-empty? xs)
        'f
        (if
          (eq? x (car xs))
          't
          (list-member? x (cdr xs))))))

(define list-map-with-index
  'examples
    (
      (list-map-with-index (lambda (n x) (list n x)) '(a b)) ((1 a) (2 b))
      (list-map-with-index (lambda (n x) (list n x)) '())    ()
    )
  'impl
    (lambda (f xs)
      (list-map-with-offset
        (lambda (n x) (f (increment n) x))
        xs)))

(define list-map-with-offset
  'examples
    (
      (list-map-with-offset (lambda (n x) (list n x)) '(a b)) ((0 a) (1 b))
      (list-map-with-offset (lambda (n x) (list n x)) '())    ()
    )
  'impl
    (lambda (f xs)
      (list-reverse
        (cadr
          (list-foldl
            (lambda (offsetAndAcc x)
              (list
                (increment (car offsetAndAcc))
                (cons
                  (f (car offsetAndAcc) x)
                  (cadr offsetAndAcc))))
            '(0 ())
            xs)))))

(define list-map-by-index
  'examples
    (
      (list-map-by-index 0 increment '(1 1 1)) (2 1 1)
      (list-map-by-index 1 increment '(1 1 1)) (2 1 1)
      (list-map-by-index 2 increment '(1 1 1)) (1 2 1)
    )
  'impl
    (lambda (n f xs)
      (list-map-by-offset (nat-decrement n) f xs)))

(define list-map-by-offset
  'examples
    (
      (list-map-by-offset -1 increment '(1 1 1)) (2 1 1)
      (list-map-by-offset 0  increment '(1 1 1)) (2 1 1)
      (list-map-by-offset 1  increment '(1 1 1)) (1 2 1)
      (list-map-by-offset 2  increment '(1 1 1)) (1 1 2)
      (list-map-by-offset 3  increment '(1 1 1)) (1 1 1) ; TODO: inconsistent
      (list-map-by-offset 4  increment '(1 1 1)) (1 1 1)
      ; (list-map-by-offset 2  increment '())      ()
    )
  'impl
    (lambda (offset f xs)
      (let
        ((first-and-second (list-split-at-offset offset xs))
         (second (cadr first-and-second)))
        (if
          (list-empty? second)
          xs
          (list-append
            (car first-and-second)
            (cons (f (car second)) (cdr second)))))))

(define list-drop-at-offset
  'examples
    (
      (list-drop-at-offset -1 '(a b c)) (a b c)
      (list-drop-at-offset 0  '(a b c)) (b c)
      (list-drop-at-offset 1  '(a b c)) (a c)
      (list-drop-at-offset 2  '(a b c)) (a b)
      (list-drop-at-offset 3  '(a b c)) (a b c)
      (list-drop-at-offset 4  '(a b c)) (a b c)
      (list-drop-at-offset 2  '())      ()
    )
  'impl
    (lambda (n xs)
      (if
        (< n 0)
        xs
        (let
          ((res (list-split-at-offset n xs)))
          (list-append (car res) (list-drop 1 (cadr res)))))))

(define list-insert-at-offset
 'examples
   (
     (list-insert-at-offset -1 'x '(a b)) (x a b)
     (list-insert-at-offset 0  'x '(a b)) (x a b)
     (list-insert-at-offset 1  'x '(a b)) (a x b)
     (list-insert-at-offset 2  'x '(a b)) (a b x)
     (list-insert-at-offset 3  'x '(a b)) (a b x)
   )
 'impl
   (lambda (offset a xs)
     (let
       ((res (list-split-at-offset offset xs)))
       (list-append
         (snoc (car res) a)
         (cadr res)))))

(define list-split-at-offset
  'examples
    (
      (list-split-at-offset -1 '(a b c)) (() (a b c))
      (list-split-at-offset 0  '(a b c)) (() (a b c))
      (list-split-at-offset 1  '(a b c)) ((a) (b c))
      (list-split-at-offset 2  '(a b c)) ((a b) (c))
      (list-split-at-offset 3  '(a b c)) ((a b c) ())
      (list-split-at-offset 4  '(a b c)) ((a b c) ())
      (list-split-at-offset 1  '())      (() ())
    )
  'impl
    (lambda (offset target)
      (let
        ((loop
          (lambda (n xs ys)
            (if
              (or (<= n 0) (list-empty? ys))
              (list xs ys)
              (loop (decrement n) (cons (car ys) xs) (cdr ys)))))) ; PERF
        (list-map-first list-reverse (loop offset '() target)))))

(define list-split-on
  'examples
    (
      (list-split-on nat? '(a 1 b)) ((a) (b))
      (list-split-on nat? '()) (())
      (list-split-on nat? '(a)) ((a))
      (list-split-on nat? '(1 a 1 b c 1 1)) (() (a) (b c) () ())
    )
  'impl
    (lambda (pred? xs)
      (let
        ((loop (lambda (acc temp-acc xs)
          (if
            (list-empty? xs)
            (cons (list-reverse temp-acc) acc)
            (if
              (pred? (car xs))
              (loop (cons (list-reverse temp-acc) acc) '() (cdr xs))
              (loop acc (cons (car xs) temp-acc) (cdr xs)))))))
        (list-reverse (loop '() '() xs)))))

(define list-split-once-on
  'examples
    (
      (list-split-once-on nat? '(a 1 b))   ((a) (1 b))
      (list-split-once-on nat? '())        (() ())
      (list-split-once-on nat? '(a))       ((a) ())
      (list-split-once-on nat? '(1))       (() (1))
      (list-split-once-on nat? '(1 a 1 b)) (() (1 a 1 b))
    )
  'impl
    (lambda (pred? xs)
      (let
        ((loop (lambda (acc xs)
          (if
            (list-empty? xs)
            (list acc '())
            (if
              (pred? (car xs))
              (list acc xs)
              (loop (cons (car xs) acc) (cdr xs)))))))
        (list-map-first list-reverse (loop '() xs)))))

(define list-foldl
  'examples
    (
      (list-foldl (lambda (acc a) (cons a acc)) '() '(a b c))      (c b a)
      (list-foldl (lambda (acc a) (cons a acc)) '(x y z) '(a b c)) (c b a x y z)
      (list-foldl (lambda (acc a) (cons a acc)) '() '())           ()
    )
  'impl
    (lambda (f start xs)
      (let
        ((loop (lambda (acc xs)
          (if
            (list-empty? xs)
            acc
            (loop (f acc (car xs)) (cdr xs))))))
        (loop start xs))))

; ------------------------------------------------------------------------------
; nat

(define nat?
  'examples
    (
      (nat? 0)  t
      (nat? -1) f
      (nat? 'a) f
    )
  'impl
    (lambda (n)
      (if
        (int? n)
        (>= n 0)
        'f)))

(define nat-decrement
  'examples
    (
      (nat-decrement 0) 0
      (nat-decrement 1) 0
      (nat-decrement 2) 1
    )
  'impl
    (lambda (n)
      (max 0 (decrement n))))

; ------------------------------------------------------------------------------
; int

(define int-foldl
  'examples
    (
      (int-foldl + 100 2) 101
      (int-foldl + 100 3) 103
      (int-foldl (lambda (acc n) (cons n acc)) '() 3) (2 1 0)
      (int-foldl + 100 0) 100
    )
  'impl
    (lambda (f start n)
      (let
        ((loop
          (lambda (acc next)
            (if
              (= next n)
              acc
              (loop (f acc next) (increment next))))))
        (loop start 0))))

(define int->string
  'examples
    (
      (int->string 0) (string-tag (48))
      (int->string 12) (string-tag (49 50))
      (int->string -12) (string-tag (45 49 50))
    )
  'impl
    (lambda (n)
      (let
        ((digits (list-map digit->codepoint (int->list-digit n))))
        (list->string
          (if
            (< n 0)
            (cons (string-to-codepoint "-") digits)
            digits)))))

; Won't include the '-' sign for negative numbers.
(define int->list-digit
  'impl
    (lambda (n)
      (let
        ((go
          (lambda (n acc)
            (if
              (< n 10)
              (cons n acc)
              (go (/ n 10) (cons (% n 10) acc))))))
        (go (abs n) '()))))

(define digit->codepoint
  'examples
    (
      (digit->codepoint 0) 48
      (digit->codepoint 9) 57
    )
  'impl
    (lambda (d)
      (cond
        ((= d 0) 48)
        ((= d 1) 49)
        ((= d 2) 50)
        ((= d 3) 51)
        ((= d 4) 52)
        ((= d 5) 53)
        ((= d 6) 54)
        ((= d 7) 55)
        ((= d 8) 56)
        ((= d 9) 57)
        ('t (crash 'digit->codepoint-not-digit)))))

(define <=
  'examples
    ( (<= 1 2) t
      (<= 1 1) t
      (<= 2 1) f
    )
  'impl
    (lambda (a b)
      (or (< a b) (= a b))))

(define >=
  'examples
    ( (>= 1 2) f
      (>= 1 1) t
      (>= 2 1) t
    )
  'impl
    (lambda (a b)
      (or (> a b) (= a b))))

(define /=
  'examples
    ( (/= 1 2) t
      (/= 1 1) f
      (/= 2 1) t
    )
  'impl
    (lambda (a b)
      (not (= a b))))

(define min
  'examples
    (
      (min 1 2) 1
    )
  'impl
    (lambda (x y)
      (if
        (< x y)
        x
        y)))

(define max
  'examples
    (
      (max 1 2) 2
    )
  'impl
    (lambda (x y)
      (if
        (> x y)
        x
        y)))

; absolute value
(define abs
  'impl
    (lambda (n)
      (if
        (< n 0)
        (* n -1)
        n)))

(define negate
  'impl
    (lambda (n)
      (* n -1)))

(define increment
  'examples
    (
      (increment 1) 2
    )
  'impl
    (lambda (n)
      (+ n 1)))

(define decrement
  'examples
    (
      (decrement 0) -1
      (decrement 1) 0
      (decrement 2) 1
    )
  'impl
    (lambda (n)
      (- n 1)))

; ------------------------------------------------------------------------------
; basic

(define identity
  'examples
    (
      (identity 'a) a
      (identity 1)  1
    )
  'impl
    (lambda (a)
      a))

(define not
  'examples
    (
      (not 't) f
      (not 'f) t
    )
  'impl
    (lambda (b)
      (if
        b
        'f
        't)))

(define begin-order-unspecified
  'impl
    (lambda xs
      'begin-void))

(define assert-eq
  'impl
    (lambda (a b)
      (if
        (eq? a b)
        't
        (crash (list 'assert-eq a b)))))

(define crash
  'impl
    (lambda (a)
      (let
        ; TODO: need to allow variables to repeat again?
        ; lost during the JS compiler transition
        ((_ (trace 'crash-------------------- '-))
         (__ (trace a '-))
         (___ (trace '------------------------- '-)))
        crash-now)))

(define eq?
  'examples
    (
      (eq? 'a 'a)     t
      (eq? 'a 'b)     f
      (eq? 'a 1)      f
      (eq? 1 1)       t
      (eq? 1 2)       f
      (eq? '() '())   t
      (eq? '() '(a))  f
      (eq? '(a) '(a)) t
      (eq? '(a) '(b)) f
    )
  'impl
    (lambda (a b)
      (if
        (and (symbol? a) (symbol? b))
        (symbol-eq? a b)
        (if
          (and (int? a) (int? b))
          (= a b)
          (if
            ; NOTE: errors like this kind of misplaced paren
            ; can be really confusing:
            ;
            ; > (and (list-empty? a)) (list-empty? b)
            ;
            (and (list-empty? a) (list-empty? b))
            't
            (if
              (and (pair? a) (pair? b))
              (if
                (eq? (car a) (car b))
                (eq? (cdr a) (cdr b))
                'f)
              'f))))))

; ------------------------------------------------------------------------------
; resume plain midnight
; ------------------------------------------------------------------------------
))

  ; ----------------------------------------
  ; misc

  ; No macros down here, this evaluates both arguments.
  (strict-and (lambda (a b)
    (if
      a
      b
      'f)))

  (eq?
    (lambda (a b)
      (if
        (strict-and (symbol? a) (symbol? b))
        (symbol-eq? a b)
        (if
          (strict-and (int? a) (int? b))
          (= a b)
          (if
            (strict-and (list-empty? a) (list-empty? b))
            't
            (if
              (strict-and (pair? a) (pair? b))
              (if
                (eq? (car a) (car b))
                (eq? (cdr a) (cdr b))
                'f)
              'f))))))

  (crash
    (lambda (a)
      (let
        ; TODO: need to allow variables to repeat again?
        ((_ (trace 'crash-plain-midnight----- '-))
         (__ (trace a '-))
         (___ (trace '------------------------- '-)))
        crash-now)))

  (increment (lambda (n)
    (+ n 1)))

  (decrement (lambda (n)
    (- n 1)))

  (repeat (lambda (n f target)
    (if
      (= n 0)
      target
      (repeat (decrement n) f (f target)))))

  ; ----------------------------------------
  ; list

  (cadr (lambda (xs)
    (car (cdr xs))))

  (list (lambda xs
    xs))

  (list-foldl (lambda (f start xs)
    (let
      ((loop (lambda (acc xs)
        (if
          (list-empty? xs)
          acc
          (loop (f acc (car xs)) (cdr xs))))))
      (loop start xs))))

  (list-reverse (lambda (xs)
    (list-foldl (lambda (acc x) (cons x acc)) '() xs)))

  (list-map (lambda (f xs)
    (list-reverse
      (list-foldl (lambda (acc x) (cons (f x) acc)) '() xs))))

  (list-append (lambda (xs ys)
    (list-foldl
      (lambda (acc x) (cons x acc))
      ys
      (list-reverse xs))))

  (list-map-with-offset (lambda (f xs)
    (list-reverse
      (cadr
        (list-foldl
          (lambda (offsetAndAcc x)
            (list
              (increment (car offsetAndAcc))
              (cons
                (f (car offsetAndAcc) x)
                (cadr offsetAndAcc))))
          '(0 ())
          xs)))))

  ; ----------------------------------------
  ; alist

  (alist-key? (lambda (k alist)
    (if
      (pair? alist)
      (let
        ((pair (car alist)))
        (if
          (eq? k (car pair))
          't
          (alist-key? k (cdr alist))))
      'f)))

  (alist-get-or-crash (lambda (k alist)
    (if
      (pair? alist)
      (let
        ((pair (car alist)))
        (if
          (eq? k (car pair))
          (cadr pair)
          (alist-get-or-crash k (cdr alist))))
      (crash (list 'alist-get-or-crash k)))))

  ; ----------------------------------------
  ; flat alist

  (flat-alist-get-predicate (lambda (pred? xs)
    (if
      (list-empty? xs)
      (crash 'flat-alist-get-predicate)
      (if
        (pred? (car xs))
        (cadr xs)
        (flat-alist-get-predicate pred? (cdr xs))))))

  ; ----------------------------------------
  ; create a let

  (to-let (lambda (varlist body)
    (list 'let varlist body)))

  ; ----------------------------------------
  ; macro helpers

  (two-args (lambda (sexps)
    (if
      (pair? sexps)
      (if
        (pair? (cdr sexps))
        (list (car sexps) (cadr sexps))
        (crash 'two-args-second-not-present))
      (crash 'two-args-first-not-present))))

  ; ----------------------------------------
  ; macros

  (identity-macro (lambda (sexps)
    (car sexps)))

  (and-macro (lambda (sexps)
    (let
      ((args (two-args sexps)))
      (list
        'if
        (car args)
        (cadr args)
        ''f))))

  (or-macro (lambda (sexps)
    (let
      ((args (two-args sexps)))
      (list
        'if
        (car args)
        ''t
        (cadr args)))))

  (cond-macro (lambda (sexps)
    (if
      (pair? sexps)
      (let
        ((pair (car sexps)))
        (list
          'if
           (car pair)
           (cadr pair)
           (cond-macro (cdr sexps))))
      (list
        'crash
        ''cond-no-match))))

  ; ----------------------------------------
  ; case macro

  ; case expressions are made up of a scrutinee
  ; followed by "arms",
  ; where each arm is a pair of a pattern and an alternative.
  ;
  ; Patterns consist of a constructor and pattern (or binding) variables.
  ;
  ; The parts of the scrutinee that bind to variables are called matched
  ; (or captured) values.
  ;
  ;
  ;
  ; Some notes, currently:
  ;
  ; - scrutinee must evaluate to a list, where the first element is a symbol
  ; - no way to match int literals atm
  (case-macro (lambda (sexps)
    (list
      'let
      (list
        (list
          'case-macro-scrutinee ; stealing this from available variables
          (car sexps)))
      (arms->ifs (cdr sexps)))))

  (arms->ifs (lambda (arms)
    (if
      (list-empty? arms)
      (list
        'crash
        ''case-no-match)
      (let
        ((pattern (car arms))
         (rest (cdr arms)))
        (if
          (list-empty? rest)
          (crash 'case-macro-no-alternative-for-pattern)
          (let
            ((alternative (car rest))
             (remainingArms (cdr rest)))
            (if
              (list-empty? pattern)
              (crash 'case-macro-pattern-empty)
              (let
                ((pattern-constructor (car pattern))
                 (bindingVars (cdr pattern)))
                (list
                  'if
                  (list
                    'eq?
                    pattern-constructor
                    (list
                      'car ; TODO: Assumes the scrutinee evaluates to a list
                      'case-macro-scrutinee))
                  (binding-vars-and-alternative->result bindingVars alternative)
                  (arms->ifs remainingArms))))))))))

  ; Will output an expression that uses the `case-macro-scrutinee` variable.
  (binding-vars-and-alternative->result (lambda (bindingVars alternative)
    ; Note that it's invalid to generate a `let` with `(())` as the assignments
    (if
      (list-empty? bindingVars)
      alternative
      (list
        'let
        (list-map-with-offset case-macro-binding bindingVars)
        alternative))))

  (case-macro-binding (lambda (offset bindingVar)
    (let
      ((generate-cdr (lambda (x)
        (list
          'cdr
          x))))
      (list
        bindingVar
        (list
          'car
          (repeat (+ offset 1) generate-cdr 'case-macro-scrutinee))))))

  ; ----------------------------------------
  ; macroexpand

  (macrotable
    (list
      (list 'identity-macro identity-macro)
      (list 'and and-macro)
      (list 'or or-macro)
      (list 'cond cond-macro)
      (list 'case case-macro)))

  (macroexpand (lambda (sexp)
    (if
      (pair? sexp)
      (let
        ((first (car sexp)))
        (if
          (alist-key? first macrotable)
          (macroexpand
            ((alist-get-or-crash first macrotable) (cdr sexp)))
          (list-map macroexpand sexp)))
      sexp)))

  ; ----------------------------------------
  ; expand a sexp of defines+impls to a varlist

  (defines-to-varlist (lambda (sexps)
    (list-map to-define sexps)))

  (to-define (lambda (sexp)
    (if
      (define? sexp)
      (let
        ((less-define (cdr sexp)))
        ; NOTE: Can't use `(flat-alist-get 'impl` here,
        ; because the target impl is still quoted:
        (list
          (car less-define)
          (macroexpand
            (flat-alist-get-predicate quoted-impl? (cdr less-define)))))
      (crash 'to-define))))

  (define? (lambda (sexp)
    (if
      (pair? sexp)
      (let
        ((first (car sexp)))
        (if
          (symbol? first)
          (symbol-eq? first 'define)
          'f))
      'f)))

  (quoted-impl? (lambda (sexp)
    (eq? sexp ''impl)))

  ; ----------------------------------------
  ; do the expansion

  (midnight-plus-macros-varlist
    (list-reverse ; reverse the order of the defines
      (defines-to-varlist midnight-plus-macros)))

  ; ----------------------------------------
  ; run example tests

  (test-result
    (let
      (
       ; An expression representing passing the defines to the test creator
       ;
       ; Note that `midnight-plus-macros` is the original source exactly,
       ; no macroexpansion or anything like that.
       (test-generating-expr
         (list 'defines->example-tests (list 'quote midnight-plus-macros)))

       ; Eval the above expression to get the tests
       (tests
         (eval (to-let midnight-plus-macros-varlist test-generating-expr))))

      ; Run the tests in the context of the original code
      (eval
        (to-let midnight-plus-macros-varlist tests))))

  ; ----------------------------------------
  ; run

  (midnight-let
    (to-let
      midnight-plus-macros-varlist
      'main))
  )

(eval midnight-let))
"""