;; lexical-binding: t; -*-

(require 'ert)

(ert-deftest test-strip-line ()

  ;; with gtabularize, should not strip anything from a length=1 line which
  ;; means that it was not split on a delimiter
  (should (equal
           '("  nonmatching  ")
           (tabular--strip-line '("  nonmatching  ") t)))

  ;; length 2
  (should (equal
           '("  a" "=")
           (tabular--strip-line '("  a" "=") t)))

  (should (equal
           '("   a" "   =   " "b")
           (tabular--strip-line '("   a  " "   =   " "   b   ")))))

(ert-deftest test-parse-formatstr ()
  (should (equal (tabular--parse-formatstr "r1c1l0") '((r 1) (c 1) (l 0))))
  (should (equal (tabular--parse-formatstr "l0") '((l 0))))
  (should (equal (tabular--parse-formatstr "l10") '((l 10))))
  (should (equal (tabular--parse-formatstr "r2") '((r 2))))
  (should (equal (tabular--parse-formatstr "c1") '((c 1)))))

(ert-deftest test-elempat ()
  (should (equal nil (tabular--validate-formatstr "l-1")))
  (should (equal nil (tabular--validate-formatstr "la")))
  (should (equal 0   (tabular--validate-formatstr "r1c1l0")))
  (should (equal 0   (tabular--validate-formatstr "l0")))
  (should (equal 0   (tabular--validate-formatstr "r0")))
  (should (equal 0   (tabular--validate-formatstr "c0")))
  (should (equal 0   (tabular--validate-formatstr "l1")))
  (should (equal 0   (tabular--validate-formatstr "r1")))
  (should (equal 0   (tabular--validate-formatstr "c1")))
  (should (equal 0   (tabular--validate-formatstr "l5")))
  (should (equal 0   (tabular--validate-formatstr "r5")))
  (should (equal 0   (tabular--validate-formatstr "c5"))))

(ert-deftest test-alignment ()
  (should (equal (tabular--left-align   "pizza" 5) "pizza"))       ; padding should keep current length
  (should (equal (tabular--left-align   "pizza" 1) "pizza"))       ; padding should not trim a string
  (should (equal (tabular--left-align      "a" 10) "a         "))  ; left pad a string
  (should (equal (tabular--left-align       "" 10) "          "))  ; left pad an empty string
  (should (equal (tabular--right-align  "pizza" 5) "pizza"))       ; padding should keep current length
  (should (equal (tabular--right-align  "pizza" 1) "pizza"))       ; padding should not trim a string
  (should (equal (tabular--right-align     "a" 10) "         a"))  ; right pad a string
  (should (equal (tabular--right-align      "" 10) "          "))  ; right pad an empty string
  (should (equal (tabular--center-align "pizza" 5) "pizza"))       ; padding should keep current length
  (should (equal (tabular--center-align "pizza" 1) "pizza"))       ; padding should not trim a string
  (should (equal (tabular--center-align    "a" 10) "     a    "))  ; center pad a string
  (should (equal (tabular--center-align     "" 10) "          "))) ; center pad an empty string

(ert-deftest test-split-delim-two-spaces ()
  (let ((delimiter  "  "))
    (should (equal (tabular--split-delim "a  b  c"  delimiter)  '("a"  "  "  "b"  "  "  "c")))
    (should (equal (tabular--split-delim "a b   c"  delimiter)  '("a b"  "  "  " c")))
    (should (equal (tabular--split-delim "ab     c"  delimiter)  '("ab"  "  "  "  "  " c")))
    (should (equal (tabular--split-delim "ab    c"  delimiter)  '("ab"  "  "  "  "  "c")))))

(ert-deftest test-split-delim-trailing-comments ()
  (let ((delimiter  (rx "//")))
    (should (equal (tabular--split-delim "x//foo"   delimiter)  '("x"  "//"  "foo"))))
  (let ((delimiter (rx (or "/*" "*/"))))
    (should (equal (tabular--split-delim "x/*foo*/"   delimiter)  '("x"  "/*"  "foo"  "*/")))))

(ert-deftest test-split-delim-cpp-io ()
  (let ((delimiter (rx (or "<<" ">>"))))
    (should (equal (tabular--split-delim "a<<b<<c"  delimiter)  '("a"  "<<"  "b"  "<<"  "c")))))

(ert-deftest test-split-delim-assignment ()
  (let ((delimiter  "[|&+*/%<>=!~-]\@<!\([<>!=]=\|=\~\)\@![|&+*/%<>=!~-]*="))
    (should (equal (tabular--split-delim "a+=b"     "+=") '("a" "+=" "b")))
    (should (equal (tabular--split-delim "a-=b"     "-=")  '("a"  "-="  "b"))) ;
    ;; (should (equal (tabular--split-delim "a!=b"     "!=")  '("a!=b")))
    ;; (should (equal (tabular--split-delim "a==b"     delimiter)  '("a==b")))
    (should (equal (tabular--split-delim "a&=b"     "&=")  '("a"  "&="  "b")))
    (should (equal (tabular--split-delim "a|=b"     "|=")  '("a"  "|="  "b")))
    (should (equal (tabular--split-delim "a=b=c"    "=")  '("a"  "="  "b"  "="  "c")))))

(ert-deftest test-split-delim-vhdl-assignment ()
  (let ((delimiter  "<="))
    (should (equal (tabular--split-delim "a<=b"  delimiter)  '("a"  "<="  "b"))))
  (let ((delimiter  ":="))
    (should (equal (tabular--split-delim "a:=b"  delimiter)  '("a"  ":="  "b")))))

(ert-deftest test-split-delim-pascal-assignment ()
  (let ((delimiter  ":="))
    (should (equal (tabular--split-delim "a:=b=c" delimiter)  '("a"  ":="  "b=c")))))

(ert-deftest test-split-delim-hash()
  (should (equal (tabular--split-delim "#ab#cd#ef"  "#")  '("#"  "ab"  "#"  "cd"  "#"  "ef")))
  ;; (should (equal (tabular--split-delim "#ab#cd#ef"  "\\[^#\\]*")  '("#"  "ab"  "#"  "cd"  "#"  "ef"  "")))
  ;; (should (equal (tabular--split-delim "#ab#cd#ef"  "#\zs")   '("#"  ""  "ab#"  ""  "cd#"  ""  "ef")))
  )

(ert-deftest test-split-delim-ternary ()
  (let ((delimiter  "?\\|:"))
    (should (equal (tabular--split-delim "a?b:c"    delimiter)  '("a"  "?"  "b"  ":"  "c")))))
