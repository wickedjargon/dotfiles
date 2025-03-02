;; ocen-mode.el -- A major mode for the Ocen programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Freddie Firouzi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; ocen-mode.el --- Major mode for editing Ocen files

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))
(require 'newcomment)
(require 'imenu)
(require 'json)
(require 'prog-mode)
(require 'treesit)
(require 'c-ts-common) ; For comment indent and filling.

(eval-when-compile
  (require 'cl-lib)
  (require 'ido)
  (require 'rx))

(defvar ido-cur-list)
(defvar electric-layout-rules)
(declare-function ido-mode "ido" (&optional arg))
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-search-subtree "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-query-compile "treesit.c")
(declare-function treesit-query-capture "treesit.c")

;;; Constants

(defconst ocen--name-start-re "[[:alpha:]_$]"
  "Regexp matching the start of a Ocen identifier, without grouping.")

(defconst ocen--stmt-delim-chars "^;{}?:")

(defconst ocen--name-re (concat ocen--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a Ocen identifier, without grouping.")

(defconst ocen--objfield-re (concat ocen--name-re ":")
  "Regexp matching the start of a Ocen object field.")

(defconst ocen--dotted-name-re
  (concat ocen--name-re "\\(?:\\." ocen--name-re "\\)*")
  "Regexp matching a dot-separated sequence of Ocen names.")

(defconst ocen--cpp-name-re ocen--name-re
  "Regexp matching a C preprocessor name.")

(defconst ocen--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst ocen--plain-method-re
  (concat "^\\s-*?\\(" ocen--dotted-name-re "\\)\\.prototype"
          "\\.\\(" ocen--name-re "\\)\\s-*?=\\s-*?\\(\\(?:async[ \t\n]+\\)function\\)\\_>")
  "Regexp matching an explicit Ocen prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the `function' keyword.")

(defconst ocen--plain-class-re
  (concat "^\\s-*\\(" ocen--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching a Ocen explicit prototype \"class\" declaration.
An example of this is \"Class.prototype = { method1: ...}\".")

;; var NewClass = BaseClass.extend(
(defconst ocen--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" ocen--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" ocen--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$"))

;; var NewClass = Class.create()
(defconst ocen--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" ocen--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()"))

(defconst ocen--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" ocen--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst ocen--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" ocen--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*("))

;; var NewClass = Class.create({
(defconst ocen--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" ocen--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" ocen--dotted-name-re "\\)\\s-*,\\s-*\\)?{?"))

;; Parent class name(s) (yes, multiple inheritance in Ocen) are
;; matched with dedicated font-lock matchers
(defconst ocen--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" ocen--dotted-name-re "\\)"))

(defconst ocen--extocen-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" ocen--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" ocen--dotted-name-re "\\)")
  "Regexp matching class declaration (style 1).")

(defconst ocen--extocen-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" ocen--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" ocen--dotted-name-re "\\)")
  "Regexp matching class declaration (style 2).")

(defconst ocen--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" ocen--dotted-name-re "\\)")
  "Regexp matching a MochiKit class declaration.")

(defconst ocen--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst ocen--class-styles
  `((:name            "Plain"
     :class-decl      ,ocen--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       ocen)

    (:name            "MochiKit"
     :class-decl      ,ocen--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,ocen--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,ocen--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,ocen--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,ocen--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,ocen--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtJS (style 1)"
     :class-decl      ,ocen--extocen-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "ExtJS (style 2)"
     :class-decl      ,ocen--extocen-class-decl-re-2
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "Merrill Press"
     :class-decl      ,ocen--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "List of Ocen class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class.  Its first group must match the name of its class.  If there
is a parent class, the second group should match, and it should be
the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name.  Otherwise, multiple definitions
will create multiple top-level entries.  Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains.")

(defconst ocen--available-frameworks
  (cl-loop for style in ocen--class-styles
           for framework = (plist-get style :framework)
           unless (memq framework available-frameworks)
           collect framework into available-frameworks
           finally return available-frameworks)
  "List of available Ocen frameworks symbols.")

(defconst ocen--function-heading-1-re
  (concat
   "^\\s-*function\\(?:\\s-\\|\\*\\)+\\(" ocen--name-re "\\)")
  "Regexp matching the start of a Ocen function header.
Match group 1 is the name of the function.")

(defconst ocen--function-heading-2-re
  (concat
   "^\\s-*\\(" ocen--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst ocen--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" ocen--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the Ocen form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst ocen--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" ocen--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun ocen--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst ocen--keyword-re
  (ocen--regexp-opt-symbol
   '("abstract" "async" "await" "break" "case" "catch" "class" "const"
     "continue" "debugger" "default" "delete" "do" "else"
     "enum" "export" "extends" "final" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "native" "new" "of" "package"
     "private" "protected" "public" "return" "static"
     "super" "switch" "synchronized" "throw"
     "throws" "transient" "try" "typeof" "var" "void" "let"
     "yield" "volatile" "while" "with"))
  "Regexp matching any Ocen keyword.")

(defconst ocen--basic-type-re
  (ocen--regexp-opt-symbol
   '("boolean" "byte" "char" "double" "float" "int" "long"
     "short" "void"))
  "Regular expression matching any predefined type in Ocen.")

(defconst ocen--constant-re
  (ocen--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in Ocen.")


(defconst ocen--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list ocen--function-heading-1-re 1 font-lock-function-name-face)
   (list ocen--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `ocen-mode'.")

(defconst ocen--font-lock-keywords-2
  (append ocen--font-lock-keywords-1
          (list (list ocen--keyword-re 1 font-lock-keyword-face)
                (cons ocen--basic-type-re font-lock-type-face)
                (cons ocen--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `ocen-mode'.")

;; ocen--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; ocen--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; ocen--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, ocen--pstate, is actually a list
;; of all ocen--pitem instances open after the marked character.
;;
;; The text property for b-end, ocen--pend, is simply the
;; ocen--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an ocen--pstate text property. Since no other
;; ocen--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; ocen--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at
;; subsequence parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(cl-defstruct (ocen--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `ocen--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `ocen--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst ocen--initial-pitem
  (make-ocen--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;;; User Customization

(defgroup js nil
  "Customization variables for Ocen mode."
  :tag "Ocen"
  :group 'languages)

(defcustom ocen-indent-level 4
  "Number of spaces for each indentation step in `ocen-mode'."
  :type 'integer
  :safe 'integerp)

(defcustom ocen-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `ocen-indent-level'."
  :type 'integer
  :safe 'integerp)

(defcustom ocen-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `ocen-indent-level'."
  :type 'integer
  :safe 'integerp
  :version "24.1")

(defcustom ocen-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `ocen-indent-level'."
  :type 'integer
  :safe 'integerp
  :version "24.1")

(defcustom ocen-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `ocen-indent-level'."
  :type 'integer
  :safe 'integerp
  :version "24.1")

(defcustom ocen-switch-indent-offset 0
  "Number of additional spaces for indenting the contents of a switch block.
The value must not be negative."
  :type 'integer
  :safe 'integerp
  :version "24.4")

(defcustom ocen-flat-functions nil
  "Treat nested functions as top-level functions in `ocen-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean)

(defcustom ocen-indent-align-list-continuation t
  "Align continuation of non-empty ([{ lines in `ocen-mode'."
  :version "26.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom ocen-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `ocen-mode'."
  :type 'function)

(defcustom ocen-enabled-frameworks ocen--available-frameworks
  "Frameworks recognized by `ocen-mode'.
To improve performance, you may turn off some frameworks you
seldom use, either globally or on a per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           ocen--available-frameworks)))

(defvar ocen-ocen-switch-tabs (and (memq system-type '(darwin)) t)
  "Whether `ocen-mode' should display tabs while selecting them.
This is useful only if the windowing system has a good mechanism
for preventing Firefox from stealing the keyboard focus.")
(make-obsolete-variable 'ocen-ocen-switch-tabs "MozRepl no longer exists" "28.1")

(defvar ocen-ocen-tmpdir (locate-user-emacs-file "js/js")
  "Temporary directory used by `ocen-mode' to communicate with Mozilla.
This directory must be readable and writable by both Mozilla and Emacs.")
(make-obsolete-variable 'ocen-ocen-tmpdir "MozRepl no longer exists" "28.1")

(defvar ocen-ocen-timeout 5
  "Reply timeout for executing commands in Mozilla via `ocen-mode'.
The value is given in seconds.  Increase this value if you are
getting timeout messages.")
(make-obsolete-variable 'ocen-ocen-timeout "MozRepl no longer exists" "28.1")

(defcustom ocen-indent-first-init nil
  "Non-nil means specially indent the first variable declaration's initializer.
Normally, the first declaration's initializer is unindented, and
subsequent declarations have their identifiers aligned with it:

  var o = {
      foo: 3
  };

  var o = {
      foo: 3
  },
      bar = 2;

If this option has the value t, indent the first declaration's
initializer by an additional level:

  var o = {
          foo: 3
      };

  var o = {
          foo: 3
      },
      bar = 2;

If this option has the value `dynamic', if there is only one declaration,
don't indent the first one's initializer; otherwise, indent it.

  var o = {
      foo: 3
  };

  var o = {
          foo: 3
      },
      bar = 2;"
  :version "25.1"
  :type '(choice (const nil) (const t) (const dynamic))
  :safe 'symbolp)

(defcustom ocen-chain-indent nil
  "Use \"chained\" indentation.
Chained indentation applies when the current line starts with \".\".
If the previous expression also contains a \".\" at the same level,
then the \".\"s will be lined up:

  let x = svg.mumble()
             .chained;"
  :version "26.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom ocen-detect-syntax t
  "When non-nil, automatically detect whether Ocen uses JSX.
`ocen-syntax' (which see) may be made buffer-local and set to
t.  The detection strategy can be customized by adding elements
to `ocen-regexps', which see."
  :version "27.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom ocen-syntax nil
  "When non-nil, parse Ocen with consideration for JSX syntax.

This enables proper font-locking and indentation of code using
Facebook’s “JSX” syntax extension for Ocen, for use with
Facebook’s “React” library.  Font-locking is like `sgml-mode'.
Indentation is also like `sgml-mode', although some indentation
behavior may differ slightly to align more closely with the
conventions of the React developer community.

When `ocen-mode' is already enabled, you should call
`ocen-enable' to set this variable.

It is set to be buffer-local (and t) when in `ocen-mode'."
  :version "27.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom ocen-align->-with-< t
  "When non-nil, “>” will be indented to the opening “<” in JSX.

When this is enabled, JSX indentation looks like this:

  <element
    attr=\"\"
  >
  </element>
  <input
  />

When this is disabled, JSX indentation looks like this:

  <element
    attr=\"\"
    >
  </element>
  <input
    />"
  :version "27.1"
  :type 'boolean
  :safe 'booleanp)

(defcustom ocen-indent-level nil
  "When non-nil, indent JSX by this value, instead of like JS.

Let `ocen-indent-level' be 4.  When this variable is also set to
nil, JSX indentation looks like this (consistent):

  return (
      <element>
          <element>
              Hello World!
          </element>
      </element>
  )

Alternatively, when this variable is also set to 2, JSX
indentation looks like this (different):

  return (
      <element>
        <element>
          Hello World!
        </element>
      </element>
  )"
  :version "27.1"
  :type '(choice integer
                 (const :tag "Not Set" nil))
  :safe (lambda (x) (or (null x) (integerp x))))
;; This is how indentation behaved out-of-the-box until Emacs 27.  JSX
;; indentation was controlled with `sgml-basic-offset', which defaults
;; to 2, whereas `ocen-indent-level' defaults to 4.  Users who had the
;; same values configured for both their HTML and JS indentation would
;; luckily get consistent JSX indentation; most others were probably
;; unhappy.  I’d be surprised if anyone actually wants different
;; indentation levels, but just in case, here’s a way back to that.

(defcustom ocen-attribute-offset 0
  "Specifies a delta for JSXAttribute indentation.

Let `ocen-indent-level' be 2.  When this variable is also set to 0,
JSXAttribute indentation looks like this:

  <element
    attribute=\"value\">
  </element>

Alternatively, when this variable is also set to 2, JSXAttribute
indentation looks like this:

  <element
      attribute=\"value\">
  </element>

This variable is like `sgml-attribute-offset'."
  :version "27.1"
  :type 'integer
  :safe 'integerp)

;;; Keymap

(defvar-keymap ocen-mode-map
  :doc "Keymap for `ocen-mode'."
  "M-." #'ocen-find-symbol)

(defvar ocen-ts-mode-map (copy-keymap ocen-mode-map)
  "Keymap used in `ocen-ts-mode'.")

;;; Syntax table and parsing

(defvar ocen-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `ocen-mode' and `ocen-ts-mode'.")

(defvar-local ocen--quick-match-re nil
  "Autogenerated regexp used by `ocen-mode' to match buffer constructs.")

(defvar-local ocen--quick-match-re-func nil
  "Autogenerated regexp used by `ocen-mode' to match constructs and functions.")

(defvar-local ocen--cache-end 1
  "Last valid buffer position for the `ocen-mode' function cache.")

(defvar-local ocen--last-parse-pos nil
  "Latest parse position reached by `ocen--ensure-cache'.")

(defvar-local ocen--state-at-last-parse-pos nil
  "Parse state at `ocen--last-parse-pos'.")

(defun ocen--maybe-join (prefix separator suffix &rest list)
  "Helper function for `ocen--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (flatten-tree list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun ocen--update-quick-match-re ()
  "Internal function used by `ocen-mode' for caching buffer constructs.
This updates `ocen--quick-match-re', based on the current set of
enabled frameworks."
  (setq ocen--quick-match-re
        (ocen--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'extjs ocen-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype ocen-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (ocen--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*("

          (when (memq 'prototype ocen-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'extjs ocen-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress ocen-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo ocen-enabled-frameworks)
           "dojo\\.declare[ \t]*(")

         (when (memq 'mochikit ocen-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*(")

         ;; mumble.prototypeTHING
         (ocen--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'ocen ocen-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq ocen--quick-match-re-func
        (concat "function\\|" ocen--quick-match-re)))

(defun ocen--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun ocen--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst ocen--forward-pstate ()
  (ocen--forward-text-property 'ocen--pstate))

(defsubst ocen--backward-pstate ()
  (ocen--backward-text-property 'ocen--pstate))

(defun ocen--pitem-goto-h-end (pitem)
  (goto-char (ocen--pitem-h-begin pitem))
  (ocen--forward-pstate))

(defun ocen--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `ocen--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (ocen--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (line-end-position) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (ocen--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun ocen--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'ocen--re-search-backward-inner)
               ((> count 0) #'ocen--re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun ocen--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `ocen--re-search-backward'."
  (let ((parse)
        (orig-macro-start
         (save-excursion
           (and (ocen--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((nth 8 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (ocen--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun ocen--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (ocen--re-search-forward regexp bound noerror (if count (- count) -1)))

(defun ocen--forward-expression ()
  "Move forward over a whole Ocen expression.
This function doesn't move over expressions continued across
lines."
  (cl-loop
   ;; non-continued case; simplistic, but good enough?
   do (cl-loop until (or (eolp)
                         (progn
                           (forward-comment most-positive-fixnum)
                           (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
               do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (ocen--continued-expression-p)))))

(defun ocen--forward-function-decl ()
  "Move forward over a Ocen function declaration.
This puts point at the `function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (unless (looking-at "\\(\\_<async\\_>[ \t\n]+\\)?\\_<function\\_>")
    (error "Invalid position"))
  (let ((name t))
    (goto-char (match-end 0))
    (forward-comment most-positive-fixnum)
    (when (eq (char-after) ?*)
      (forward-char)
      (forward-comment most-positive-fixnum))
    (when (looking-at ocen--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun ocen--function-prologue-beginning (&optional pos)
  "Return the start of the Ocen function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at ocen--function-heading-2-re)
                  (looking-at ocen--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (let ((start nil))
        (and (or (looking-at "\\_<function\\_>")
                 (ocen--re-search-backward "\\_<function\\_>" nil t))
             (progn
               (setq start (match-beginning 0))
               (goto-char start)
               (when (looking-back "\\_<async\\_>[ \t\n]+" (- (point) 30))
                 (setq start (match-beginning 0)))
               (ocen--forward-function-decl))
             (<= pos (point))
             (or prologue-begin start))))))

(defun ocen--beginning-of-defun-raw ()
  "Helper function for `ocen-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (ocen--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (ocen--backward-pstate))
                (not (eq 'function (ocen--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun ocen--pstate-is-toplevel-defun (pstate)
  "Helper function for `ocen--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (cl-loop for pitem in pstate
           with func-depth = 0
           with func-pitem
           if (eq 'function (ocen--pitem-type pitem))
           do (cl-incf func-depth)
           and do (setq func-pitem pitem)
           finally return (if (eq func-depth 1) func-pitem)))

(defun ocen--beginning-of-defun-nested ()
  "Helper function for `ocen--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (cl-loop for pitem in (ocen--parse-state-at-point)
            if (and (eq 'function (ocen--pitem-type pitem))
                    (ocen--inside-pitem-p pitem))
            do (goto-char (ocen--pitem-h-begin pitem))
            and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (cl-loop for pstate = (ocen--backward-pstate)
            while pstate
            if (ocen--pstate-is-toplevel-defun pstate)
            do (goto-char (ocen--pitem-h-begin it))
            and return it)))

(defun ocen--beginning-of-defun-flat ()
  "Helper function for `ocen-beginning-of-defun'."
  (let ((pstate (ocen--beginning-of-defun-raw)))
    (when pstate
      (goto-char (ocen--pitem-h-begin (car pstate)))
      t)))

(defun ocen-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `ocen-mode'."
  (setq arg (or arg 1))
  (let ((found))
    (while (and (not (eobp)) (< arg 0))
      (cl-incf arg)
      (when (and (not ocen-flat-functions)
                 (or (eq (ocen-syntactic-context) 'function)
                     (ocen--function-prologue-beginning)))
        (ocen-end-of-defun))

      (if (ocen--re-search-forward
           "\\_<function\\_>" nil t)
          (progn (goto-char (ocen--function-prologue-beginning))
                 (setq found t))
        (goto-char (point-max))
        (setq found nil)))

    (while (> arg 0)
      (cl-decf arg)
      ;; If we're just past the end of a function, the user probably wants
      ;; to go to the beginning of *that* function
      (when (eq (char-before) ?})
        (backward-char))

      (let ((prologue-begin (ocen--function-prologue-beginning)))
        (cond ((and prologue-begin (< prologue-begin (point)))
               (goto-char prologue-begin)
               (setq found t))

              (ocen-flat-functions
               (setq found (ocen--beginning-of-defun-flat)))
              (t
               (when (ocen--beginning-of-defun-nested)
                 (setq found t))))))
    found))

(defun ocen--flush-caches (&optional beg _ignored)
  "Flush the `ocen-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq ocen--cache-end (min ocen--cache-end beg)))

(defmacro ocen--debug (&rest _arguments)
  ;; `(message ,@arguments)
  )

(defun ocen--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (ocen--pitem-paren-depth top-item))
      (cl-assert (not (get-text-property (1- (point)) 'ocen-pend)))
      (put-text-property (1- (point)) (point) 'ocen--pend top-item)
      (setf (ocen--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (ocen--pitem-add-child (cl-second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro ocen--ensure-cache--update-parse ()
  "Helper function for `ocen--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `ocen--ensure-cache'."
  '(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (ocen--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (cl-assert (> (nth 0 parse)
                            (ocen--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (ocen--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (ocen--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (ocen--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun ocen--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'ocen--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun ocen--split-name (string)
  "Split a Ocen name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar ocen--guess-function-name-start nil)

(defun ocen--guess-function-name (position)
  "Guess the name of the Ocen function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `ocen--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq ocen--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at ocen--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq ocen--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at ocen--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq ocen--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun ocen--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (ocen--forward-text-property
                             'ocen--pend))
        (setf (ocen--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(ocen--pstate t ocen--pend t)))

(defun ocen--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< ocen--cache-end limit)

    (c-save-buffer-state
        (open-items
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         goal-point)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (cl-loop for style in ocen--class-styles
                     if (memq (plist-get style :framework)
                              ocen-enabled-frameworks)
                     collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char ocen--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'ocen--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'ocen--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'ocen--pstate))
                (cl-assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list ocen--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (ocen--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (cl-loop while (re-search-forward ocen--quick-match-re-func nil t)
                   for orig-match-start = (goto-char (match-beginning 0))
                   for orig-match-end = (match-end 0)
                   do (ocen--ensure-cache--update-parse)
                   for orig-depth = (nth 0 parse)

                   ;; Each of these conditions should return non-nil if
                   ;; we should add a new item and leave point at the end
                   ;; of the new item's header (h-end in the
                   ;; ocen--pitem diagram). This point is the one
                   ;; after the last character we need to unambiguously
                   ;; detect this construct. If one of these evaluates to
                   ;; nil, the location of the point is ignored.
                   if (cond
                       ;; In comment or string
                       ((nth 8 parse) nil)

                       ;; Regular function declaration
                       ((and (looking-at "\\_<function\\_>")
                             (setq name (ocen--forward-function-decl)))
                        (when (eq name t)
                          (setq name (ocen--guess-function-name orig-match-end))
                          (if name
                              (when ocen--guess-function-name-start
                                (setq orig-match-start
                                      ocen--guess-function-name-start))

                            (setq name t)))

                        (cl-assert (eq (char-after) ?{))
                        (forward-char)
                        (save-excursion
                          (goto-char orig-match-start)
                          (when (looking-back "\\_<async\\_>[ \t\n]+"
                                              (- (point) 30))
                            (setq orig-match-start (match-beginning 0))))
                        (make-ocen--pitem
                         :paren-depth orig-depth
                         :h-begin orig-match-start
                         :type 'function
                         :name (if (eq name t)
                                   name
                                 (ocen--split-name name))))

                       ;; Macro
                       ((looking-at ocen--macro-decl-re)

                        ;; Macros often contain unbalanced parentheses.
                        ;; Make sure that h-end is at the textual end of
                        ;; the macro no matter what the parenthesis say.
                        (c-end-of-macro)
                        (ocen--ensure-cache--update-parse)

                        (make-ocen--pitem
                         :paren-depth (nth 0 parse)
                         :h-begin orig-match-start
                         :type 'macro
                         :name (list (match-string-no-properties 1))))

                       ;; "Prototype function" declaration
                       ((looking-at ocen--plain-method-re)
                        (goto-char (match-beginning 3))
                        (when (save-match-data
                                (ocen--forward-function-decl))
                          (forward-char)
                          (make-ocen--pitem
                           :paren-depth orig-depth
                           :h-begin orig-match-start
                           :type 'function
                           :name (nconc (ocen--split-name
                                         (match-string-no-properties 1))
                                        (list (match-string-no-properties 2))))))

                       ;; Class definition
                       ((cl-loop
                         with syntactic-context =
                         (ocen--syntactic-context-from-pstate open-items)
                         for class-style in filtered-class-styles
                         if (and (memq syntactic-context
                                       (plist-get class-style :contexts))
                                 (looking-at (plist-get class-style
                                                        :class-decl)))
                         do (goto-char (match-end 0))
                         and return
                         (make-ocen--pitem
                          :paren-depth orig-depth
                          :h-begin orig-match-start
                          :type class-style
                          :name (ocen--split-name
                                 (match-string-no-properties 1))))))

                   do (ocen--ensure-cache--update-parse)
                   and do (push it open-items)
                   and do (put-text-property
                           (1- (point)) (point) 'ocen--pstate open-items)
                   else do (goto-char orig-match-end))

          (goto-char limit)
          (ocen--ensure-cache--update-parse)
          (setq ocen--cache-end limit)
          (setq ocen--last-parse-pos limit)
          (setq ocen--state-at-last-parse-pos open-items)
          )))))

(defun ocen--end-of-defun-flat ()
  "Helper function for `ocen-end-of-defun'."
  (cl-loop while (ocen--re-search-forward "}" nil t)
           do (ocen--ensure-cache)
           if (get-text-property (1- (point)) 'ocen--pend)
           if (eq 'function (ocen--pitem-type it))
           return t
           finally do (goto-char (point-max))))

(defun ocen--end-of-defun-nested ()
  "Helper function for `ocen-end-of-defun'."
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (ocen--beginning-of-defun-nested))
                          (ocen--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (ocen--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (ocen--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun ocen-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `ocen-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (cl-incf arg)
    (ocen-beginning-of-defun)
    (ocen-beginning-of-defun)
    (unless (bobp)
      (ocen-end-of-defun)))

  (while (> arg 0)
    (cl-decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if ocen-flat-functions
        (ocen--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call ocen--end-of-defun-nested to do the real work
      (let ((prologue-begin (ocen--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (ocen--forward-function-decl)
               (forward-list))

              (t (ocen--end-of-defun-nested)))))))

(defun ocen--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at ocen--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun ocen--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `ocen-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (ocen--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (ocen--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun ocen--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `ocen-mode'."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;; Like (up-list -1), but only considers lists that end nearby"
(defun ocen--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our computation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun ocen--inside-param-list-p ()
  "Return non-nil if point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (ocen--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun ocen--inside-dojo-class-list-p ()
  "Return non-nil if point is in a Dojo multiple-inheritance class block."
  (ignore-errors
    (save-excursion
      (ocen--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at ocen--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

;;; Font Lock
(defun ocen--make-framework-matcher (framework &rest regexps)
  "Helper function for building `ocen--font-lock-keywords'.
Create a byte-compiled function for matching a concatenation of
REGEXPS, but only if FRAMEWORK is in `ocen-enabled-frameworks'."
  (let ((regexp (apply #'concat regexps)))
    (lambda (limit)
      (when (memq framework ocen-enabled-frameworks)
        (re-search-forward regexp limit t)))))

(defvar-local ocen--tmp-location nil)

(defun ocen--forward-destructuring-spec (&optional func)
  "Move forward over a Ocen destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true if this was actually a
spec.  FUNC must preserve the match data."
  (pcase (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (ocen--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at ocen--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at ocen--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (ocen--forward-destructuring-spec func))
                      ((looking-at ocen--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun ocen--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at ocen--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (ocen--forward-destructuring-spec))

                          (ocen--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (ocen--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

;; It wouldn’t be sufficient to font-lock JSX with mere regexps, since
;; a JSXElement may be nested inside a JS expression within the
;; boundaries of a parent JSXOpeningElement, and such a hierarchy
;; ought to be fontified like JSX, JS, and JSX respectively:
;;
;;   <div attr={void(<div></div>) && void(0)}></div>
;;
;;   <div attr={           ← JSX
;;          void(          ← JS
;;            <div></div>  ← JSX
;;          ) && void(0)   ← JS
;;        }></div>         ← JSX
;;
;; `ocen-syntax-propertize' unambiguously identifies JSX syntax,
;; including when it’s nested.
;;
;; Using a matcher function for each relevant part, retrieve match
;; data recorded as syntax properties for fontification.

(defconst ocen--font-lock-keywords
  `((ocen--match-tag-name 0 font-lock-function-name-face t)
    (ocen--match-attribute-name 0 font-lock-variable-name-face t)
    (ocen--match-text 0 'default t) ; “Undo” keyword fontification.
    (ocen--match-tag-beg)
    (ocen--match-tag-end)
    (ocen--match-expr))
  "JSX font lock faces and multiline text properties.")

(defun ocen--match-tag-name (limit)
  "Match JSXBoundaryElement names, until LIMIT."
  (when ocen-syntax
    (let ((pos (next-single-char-property-change (point) 'ocen-tag-name nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'ocen-tag-name))
                 (progn (set-match-data value) t))
            (ocen--match-tag-name limit))))))

(defun ocen--match-attribute-name (limit)
  "Match JSXAttribute names, until LIMIT."
  (when ocen-syntax
    (let ((pos (next-single-char-property-change (point) 'ocen-attribute-name nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'ocen-attribute-name))
                 (progn (set-match-data value) t))
            (ocen--match-attribute-name limit))))))

(defun ocen--match-text (limit)
  "Match JSXText, until LIMIT."
  (when ocen-syntax
    (let ((pos (next-single-char-property-change (point) 'ocen-text nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'ocen-text))
                 (progn (set-match-data value)
                        (put-text-property (car value) (cadr value) 'font-lock-multiline t)
                        t))
            (ocen--match-text limit))))))

(defun ocen--match-tag-beg (limit)
  "Match JSXBoundaryElements from start, until LIMIT."
  (when ocen-syntax
    (let ((pos (next-single-char-property-change (point) 'ocen-tag-beg nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'ocen-tag-beg))
                 (progn (put-text-property pos (cdr value) 'font-lock-multiline t) t))
            (ocen--match-tag-beg limit))))))

(defun ocen--match-tag-end (limit)
  "Match JSXBoundaryElements from end, until LIMIT."
  (when ocen-syntax
    (let ((pos (next-single-char-property-change (point) 'ocen-tag-end nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'ocen-tag-end))
                 (progn (put-text-property value pos 'font-lock-multiline t) t))
            (ocen--match-tag-end limit))))))

(defun ocen--match-expr (limit)
  "Match JSXExpressionContainers, until LIMIT."
  (when ocen-syntax
    (let ((pos (next-single-char-property-change (point) 'ocen-expr nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'ocen-expr))
                 (progn (put-text-property pos value 'font-lock-multiline t) t))
            (ocen--match-expr limit))))))

(defconst ocen--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@ocen--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (ocen--class-decl-matcher
     ,(concat "\\(" ocen--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (ocen--class-decl-matcher
     ,(concat "\\(" ocen--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq ocen--tmp-location (match-end 2))
           (goto-char ocen--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq ocen--tmp-location nil)
       (goto-char (line-end-position)))
     (when ocen--tmp-location
       (save-excursion
         (goto-char ocen--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (ocen--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(ocen--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" ocen--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" ocen--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(ocen--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" ocen--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" ocen--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(ocen--make-framework-matcher
       'dojo
       "^\\s-*" ocen--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" ocen--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (ocen--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" ocen--basic-type-re)
      (list #'ocen--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" ocen--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" ocen--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" ocen--name-re "\\)?\\s-*(\\s-*"
       ocen--name-start-re)
      (list (concat "\\(" ocen--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" ocen--name-re "\\s-*[,)]")
      (list ocen--name-re
            '(if (save-excursion (backward-char)
                                 (ocen--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face)))

    ;; jsx (when enabled)
    ,@ocen--font-lock-keywords)
  "Level three font lock for `ocen-mode'.")

(defun ocen--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (ocen--ensure-cache)
  (cl-assert (ocen--pitem-h-begin pitem))
  (cl-assert (ocen--pitem-paren-depth pitem))

  (and (> (point) (ocen--pitem-h-begin pitem))
       (or (null (ocen--pitem-b-end pitem))
           (> (ocen--pitem-b-end pitem) (point)))))

(defun ocen--parse-state-at-point ()
  "Parse the Ocen program state at point.
Return a list of `ocen--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (ocen--ensure-cache)
      (let ((pstate (or (save-excursion
                          (ocen--backward-pstate))
                        (list ocen--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (cl-loop for pitem = (car pstate)
                 until (or (eq (ocen--pitem-type pitem)
                               'toplevel)
                           (ocen--inside-pitem-p pitem))
                 do (pop pstate))

        pstate))))

(defun ocen--syntactic-context-from-pstate (pstate)
  "Return the Ocen syntactic context corresponding to PSTATE."
  (let ((type (ocen--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun ocen-syntactic-context ()
  "Return the Ocen syntactic context at point.
When called interactively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (ocen--syntactic-context-from-pstate
                             (ocen--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun ocen--class-decl-matcher (limit)
  "Font lock function used by `ocen-mode'.
This performs fontification according to `ocen--class-styles'."
  (when ocen-enabled-frameworks
    (cl-loop initially (ocen--ensure-cache limit)
             while (re-search-forward ocen--quick-match-re limit t)
             for orig-end = (match-end 0)
             do (goto-char (match-beginning 0))
             if (cl-loop for style in ocen--class-styles
                         for decl-re = (plist-get style :class-decl)
                         if (and (memq (plist-get style :framework)
                                       ocen-enabled-frameworks)
                                 (memq (ocen-syntactic-context)
                                       (plist-get style :contexts))
                                 decl-re
                                 (looking-at decl-re))
                         do (goto-char (match-end 0))
                         and return t)
             return t
             else do (goto-char orig-end))))

(defconst ocen--font-lock-keywords
  '(ocen--font-lock-keywords-3 ocen--font-lock-keywords-1
                                   ocen--font-lock-keywords-2
                                   ocen--font-lock-keywords-3)
  "Font lock keywords for `ocen-mode'.  See `font-lock-keywords'.")

(defun ocen-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      font-lock-string-face
    (if (save-excursion
          (goto-char (nth 8 state))
          (looking-at "/\\*\\*"))
        font-lock-doc-face
      font-lock-comment-face)))

(defconst ocen--syntax-propertize-regexp-regexp
  (rx
   ;; Start of regexp.
   "/"
   (0+ (or
        ;; Match characters outside of a character class.
        (not (any ?\[ ?/ ?\\))
        ;; Match backslash quoted characters.
        (and "\\" not-newline)
        ;; Match character class.
        (and
         "["
         (0+ (or
              (not (any ?\] ?\\))
              (and "\\" not-newline)))
         "]")))
   (group (zero-or-one "/")))
  "Regular expression matching a Ocen regexp literal.")

(defun ocen-syntax-propertize-regexp (end)
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      ;; A /.../ regexp.
      (goto-char (nth 8 ppss))
      (when (looking-at ocen--syntax-propertize-regexp-regexp)
        ;; Don't touch text after END.
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defconst ocen--unary-keyword-re
  (ocen--regexp-opt-symbol '("await" "delete" "typeof" "void" "yield"))
  "Regexp matching unary operator keywords.")

(defun ocen--unary-keyword-p (string)
  "Check if STRING is a unary operator keyword in Ocen."
  (string-match-p ocen--unary-keyword-re string))

;; Adding `syntax-multiline' text properties to JSX isn’t sufficient
;; to identify multiline JSX when first typing it.  For instance, if
;; the user is typing a JSXOpeningElement for the first time…
;;
;;   <div
;;       ^ (point)
;;
;; …and the user inserts a line break after the tag name (before the
;; JSXOpeningElement starting on that line has been unambiguously
;; identified as such), then the `syntax-propertize' region won’t be
;; extended backwards to the start of the JSXOpeningElement:
;;
;;   <div         ← This line wasn't JSX when last edited.
;;     attr="">   ← Despite completing the JSX, the next
;;             ^    `syntax-propertize' region wouldn’t magically
;;                  extend back a few lines.
;;
;; Therefore, to try and recover from this scenario, parse backward
;; from “>” to try and find the start of JSXBoundaryElements, and
;; extend the `syntax-propertize' region there.

(defun ocen--syntax-propertize-extend-region (start end)
  "Extend the START-END region for propertization, if necessary.
For use by `syntax-propertize-extend-region-functions'."
  (if ocen-syntax (ocen--syntax-propertize-extend-region start end)))

(defun ocen--syntax-propertize-extend-region (start end)
  "Extend the START-END region for propertization, if necessary.
If any “>” in the region appears to be the end of a tag starting
before the start of the region, extend region backwards to the
start of that tag so parsing may proceed from that point.
For use by `syntax-propertize-extend-region-functions'."
  (let (new-start
        forward-sexp-function ; Use the Lisp version.
        parse-sexp-lookup-properties) ; Fix backward-sexp error here.
    (catch 'stop
      (goto-char start)
      (while (re-search-forward ">" end t)
        (catch 'continue
          ;; Check if this is really a right shift bitwise operator
          ;; (“>>” or “>>>”).
          (unless (or (eq (char-before (1- (point))) ?>)
                      (eq (char-after) ?>))
            (save-excursion
              (backward-char)
              (while (progn (if (= (point) (point-min)) (throw 'continue nil))
                            (/= (char-before) ?<))
                (skip-chars-backward " \t\n")
                (if (= (point) (point-min)) (throw 'continue nil))
                (cond
                 ((memq (char-before) '(?\" ?\' ?\` ?\}))
                  (condition-case nil
                      (backward-sexp)
                    (scan-error (throw 'continue nil))))
                 ((memq (char-before) '(?\/ ?\=)) (backward-char))
                 ((looking-back ocen--dotted-name-re (line-beginning-position) t)
                  (goto-char (match-beginning 0)))
                 (t (throw 'continue nil))))
              (when (< (point) start)
                (setq new-start (1- (point)))
                (throw 'stop nil)))))))
    (if new-start (cons new-start end))))

;; When applying syntax properties, since `ocen-syntax-propertize' uses
;; `syntax-propertize-rules' to parse JSXBoundaryElements iteratively
;; and statelessly, whenever we exit such an element, we need to
;; determine the JSX depth.  If >0, then we know to apply syntax
;; properties to JSXText up until the next JSXBoundaryElement occurs.
;; But if the JSX depth is 0, then—importantly—we know to NOT parse
;; the following code as JSXText, rather propertize it as regular JS
;; as long as warranted.
;;
;; Also, when indenting code, we need to know if the code we’re trying
;; to indent is on the 2nd or later line of multiline JSX, in which
;; case the code is indented according to XML-like JSX conventions.
;;
;; For the aforementioned reasons, we find ourselves needing to
;; determine whether point is enclosed in JSX or not; and, if so,
;; where the JSX is.  The following functions provide that knowledge.

(defconst ocen--tag-start-re
  (concat "\\(" ocen--dotted-name-re "\\)\\(?:"
          ;; Whitespace is only necessary if an attribute implies JSX.
          "\\(?:\\s-\\|\n\\)*[{/>]"
          "\\|"
          "\\(?:\\s-\\|\n\\)+" ocen--name-start-re
          "\\)")
  "Regexp unambiguously matching a JSXOpeningElement.")

(defun ocen--matched-tag-type ()
  "Determine if the last “<” was a JSXBoundaryElement and its type.
Return `close' for a JSXClosingElement/JSXClosingFragment match,
return `self-closing' for some self-closing JSXOpeningElements,
else return `other'."
  (cond
   ((= (char-after) ?/) (forward-char) 'close) ; JSXClosingElement/JSXClosingFragment
   ((= (char-after) ?>) (forward-char) 'other) ; JSXOpeningFragment
   ((and (looking-at ocen--tag-start-re) ; JSXOpeningElement
         (not (ocen--unary-keyword-p (match-string 1))))
    (goto-char (match-end 0))
    (if (= (char-before) ?/) 'self-closing 'other))))

(defconst ocen--self-closing-re "/\\s-*>"
  "Regexp matching the end of a self-closing JSXOpeningElement.")

(defun ocen--matching-close-tag-pos ()
  "Return position of the closer of the opener before point.
Assuming a JSXOpeningElement or a JSXOpeningFragment is
immediately before point, find a matching JSXClosingElement or
JSXClosingFragment, skipping over any nested JSXElements to find
the match.  Return nil if a match can’t be found."
  (let ((tag-stack 1) tag-pos type last-pos pos)
    (catch 'stop
      (while (and (re-search-forward "<\\s-*" nil t) (not (eobp)))
        ;; Not inside a comment or string.
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (when (setq tag-pos (match-beginning 0)
                      type (ocen--matched-tag-type))
            (when last-pos
              (setq pos (point))
              (goto-char last-pos)
              (while (re-search-forward ocen--self-closing-re pos 'move)
                (setq tag-stack (1- tag-stack))))
            (if (eq type 'close)
                (progn
                  (setq tag-stack (1- tag-stack))
                  (when (= tag-stack 0)
                    (throw 'stop tag-pos)))
              ;; JSXOpeningElements that we know are self-closing
              ;; aren’t added to the stack at all (because point is
              ;; already past that syntax).
              (unless (eq type 'self-closing)
                (setq tag-stack (1+ tag-stack))))
            (setq last-pos (point))))))))

(defun ocen--enclosing-tag-pos ()
  "Return beginning and end of a JSXElement about point.
Look backward for a JSXElement that both starts before point and
also ends at/after point.  That may be either a self-closing
JSXElement or a JSXOpeningElement/JSXClosingElement pair."
  (let ((start (point)) tag-beg tag-beg-pos tag-end-pos close-tag-pos)
    (while
        (and
         (setq tag-beg (ocen--backward-text-property 'ocen-tag-beg))
         (progn
           (setq tag-beg-pos (point)
                 tag-end-pos (cdr tag-beg))
           (not
            (or
             (and (eq (car tag-beg) 'self-closing)
                  (< start tag-end-pos))
             (and (eq (car tag-beg) 'open)
                  (or (< start tag-end-pos)
                      (progn
                        (unless
                            ;; Try to read a cached close position,
                            ;; but it might not be available yet.
                            (setq close-tag-pos
                                  (get-text-property (point) 'ocen-close-tag-pos))
                          (save-excursion
                            (goto-char tag-end-pos)
                            (setq close-tag-pos (ocen--matching-close-tag-pos)))
                          (when close-tag-pos
                            ;; Cache the close position to make future
                            ;; searches faster.
                            (put-text-property
                             (point) (1+ (point))
                             'ocen-close-tag-pos close-tag-pos)))
                        ;; The JSXOpeningElement may be unclosed, else
                        ;; the closure must occur at/after the start
                        ;; point (otherwise, a miscellaneous previous
                        ;; JSXOpeningElement has been found, so keep
                        ;; looking backwards for an enclosing one).
                        (or (not close-tag-pos) (<= start close-tag-pos)))))))))
      ;; Don't return the last tag pos, as it wasn't enclosing.
      (setq tag-beg nil close-tag-pos nil))
    (and tag-beg (list tag-beg-pos tag-end-pos close-tag-pos))))

(defun ocen--at-enclosing-tag-child-p ()
  "Return t if point is at an enclosing tag’s child."
  (let ((pos (save-excursion (ocen--enclosing-tag-pos))))
    (and pos (>= (point) (nth 1 pos)))))

;; We implement `syntax-propertize-function' logic fully parsing JSX
;; in order to provide very accurate JSX indentation, even in the most
;; complex cases (e.g. to indent JSX within a JS expression within a
;; JSXAttribute…), as over the years users have requested this.  Since
;; we find so much information during this parse, we later use some of
;; the useful bits for font-locking, too.
;;
;; Some extra effort is devoted to ensuring that no code which could
;; possibly be valid JS is ever misinterpreted as partial JSX, since
;; that would be regressive.
;;
;; We first parse trying to find the minimum number of components
;; necessary to unambiguously identify a JSXBoundaryElement, even if
;; it is a partial one.  If a complete one is parsed, we move on to
;; parse any JSXText.  When that’s terminated, we unwind back to the
;; `syntax-propertize-rules' loop so the next JSXBoundaryElement can
;; be parsed, if any, be it an opening or closing one.

(defun ocen--text-range (beg end)
  "Identify JSXText within a “>/{/}/<” pair."
  (when (> (- end beg) 0)
    (save-excursion
      (goto-char beg)
      (while (and (skip-chars-forward " \t\n" end) (< (point) end))
        ;; Comments and string quotes don’t serve their usual
        ;; syntactic roles in JSXText; make them plain punctuation to
        ;; negate those roles.
        (when (or (= (char-after) ?/) ; comment
                  (= (syntax-class (syntax-after (point))) 7)) ; string quote
          (put-text-property (point) (1+ (point)) 'syntax-table '(1)))
        (forward-char)))
    ;; Mark JSXText so it can be font-locked as non-keywords.
    (put-text-property beg (1+ beg) 'ocen-text (list beg end (current-buffer)))
    ;; Ensure future propertization beginning from within the
    ;; JSXText determines JSXText context from earlier lines.
    (put-text-property beg end 'syntax-multiline t)))

;; In order to respect the end boundary `syntax-propertize-function'
;; sets, care is taken in the following functions to abort parsing
;; whenever that boundary is reached.

(defun ocen--syntax-propertize-tag-text (end)
  "Determine if JSXText is before END and propertize it.
Text within an open/close tag pair may be JSXText.  Temporarily
interrupt JSXText by JSXExpressionContainers, and terminate
JSXText when another JSXBoundaryElement is encountered.  Despite
terminations, all JSXText will be identified once all the
JSXBoundaryElements within an outermost JSXElement’s tree have
been propertized."
  (let ((text-beg (point))
        forward-sexp-function) ; Use Lisp version.
    (catch 'stop
      (while (re-search-forward "[{<]" end t)
        (ocen--text-range text-beg (1- (point)))
        (cond
         ((= (char-before) ?{)
          (let (expr-beg expr-end)
            (condition-case nil
                (save-excursion
                  (backward-char)
                  (setq expr-beg (point))
                  (forward-sexp)
                  (setq expr-end (point)))
              (scan-error nil))
            ;; Recursively propertize the JSXExpressionContainer’s
            ;; (possibly-incomplete) expression.
            (ocen-syntax-propertize (1+ expr-beg) (if expr-end (min (1- expr-end) end) end))
            ;; Ensure future propertization beginning from within the
            ;; (possibly-incomplete) expression can determine JSXText
            ;; context from earlier lines.
            (put-text-property expr-beg (1+ expr-beg) 'ocen-expr (or expr-end end)) ; font-lock
            (put-text-property expr-beg (if expr-end (min expr-end end) end) 'syntax-multiline t) ; syntax-propertize
            ;; Exit the JSXExpressionContainer if that’s possible,
            ;; else move to the end of the propertized area.
            (goto-char (if expr-end (min expr-end end) end))))
         ((= (char-before) ?<)
          (backward-char) ; Ensure the next tag can be propertized.
          (throw 'stop nil)))
        (setq text-beg (point))))))

(defconst ocen--attribute-name-re (concat ocen--name-start-re
                                            "\\(?:\\s_\\|\\sw\\|-\\)*")
  "Like `ocen--name-re', but matches “-” as well.")

(defun ocen--syntax-propertize-tag (end)
  "Determine if a JSXBoundaryElement is before END and propertize it.
Disambiguate JSX from inequality operators and arrow functions by
testing for syntax only valid as JSX."
  (let ((tag-beg (1- (point))) tag-end (type 'open)
        name-beg name-match-data expr-attribute-beg unambiguous
        forward-sexp-function) ; Use Lisp version.
    (catch 'stop
      (while (and (< (point) end)
                  (progn (skip-chars-forward " \t\n" end)
                         (< (point) end)))
        (cond
         ((= (char-after) ?>)
          ;; Make the closing “>” a close parenthesis.
          (put-text-property (point) (1+ (point)) 'syntax-table
                             (eval-when-compile (string-to-syntax ")<")))
          (forward-char)
          (setq unambiguous t)
          (throw 'stop nil))
         ;; Handle a JSXSpreadChild (“<Foo {...bar}”) or a
         ;; JSXExpressionContainer as a JSXAttribute value
         ;; (“<Foo bar={…}”).  Check this early in case continuing a
         ;; JSXAttribute parse.
         ((or (and name-beg (= (char-after) ?{))
              (setq expr-attribute-beg nil))
          (setq unambiguous t) ; JSXExpressionContainer post tag name ⇒ JSX
          (when expr-attribute-beg
            ;; Remember that this JSXExpressionContainer is part of a
            ;; JSXAttribute, as that can affect its expression’s
            ;; indentation.
            (put-text-property
             (point) (1+ (point)) 'ocen-expr-attribute expr-attribute-beg)
            (setq expr-attribute-beg nil))
          (let (expr-end)
            (condition-case nil
                (save-excursion
                  (forward-sexp)
                  (setq expr-end (point)))
              (scan-error nil))
            (forward-char)
            (if (>= (point) end) (throw 'stop nil))
            (skip-chars-forward " \t\n" end)
            (if (>= (point) end) (throw 'stop nil))
            (if (= (char-after) ?}) (forward-char) ; Shortcut to bail.
              ;; Recursively propertize the JSXExpressionContainer’s
              ;; expression.
              (ocen-syntax-propertize (point) (if expr-end (min (1- expr-end) end) end))
              ;; Exit the JSXExpressionContainer if that’s possible,
              ;; else move to the end of the propertized area.
              (goto-char (if expr-end (min expr-end end) end)))))
         ((= (char-after) ?/)
          ;; Assume a tag is an open tag until a slash is found, then
          ;; figure out what type it actually is.
          (if (eq type 'open) (setq type (if name-beg 'self-closing 'close)))
          (forward-char))
         ((and (not name-beg) (looking-at ocen--dotted-name-re))
          ;; Don’t match code like “if (i < await foo)”
          (if (ocen--unary-keyword-p (match-string 0)) (throw 'stop nil))
          ;; Save boundaries for later fontification after
          ;; unambiguously determining the code is JSX.
          (setq name-beg (match-beginning 0)
                name-match-data (match-data))
          (goto-char (match-end 0)))
         ((and name-beg (looking-at ocen--attribute-name-re))
          (setq unambiguous t) ; Non-unary name followed by 2nd name ⇒ JSX
          ;; Save JSXAttribute’s name’s match data for font-locking later.
          (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                             'ocen-attribute-name (match-data))
          (goto-char (match-end 0))
          (if (>= (point) end) (throw 'stop nil))
          (skip-chars-forward " \t\n" end)
          (if (>= (point) end) (throw 'stop nil))
          ;; “=” is optional for null-valued JSXAttributes.
          (when (= (char-after) ?=)
            (forward-char)
            (if (>= (point) end) (throw 'stop nil))
            (skip-chars-forward " \t\n" end)
            (if (>= (point) end) (throw 'stop nil))
            ;; Skip over strings (if possible).  Any
            ;; JSXExpressionContainer here will be parsed in the
            ;; next iteration of the loop.
            (if (memq (char-after) '(?\" ?\' ?\`))
                (progn
                  ;; Record the string’s position so derived modes
                  ;; applying syntactic fontification atypically
                  ;; (e.g. js2-mode) can recognize it as part of JSX.
                  (put-text-property (point) (1+ (point)) 'ocen-string t)
                  (condition-case nil
                      (forward-sexp)
                    (scan-error (throw 'stop nil))))
              ;; Save JSXAttribute’s beginning in case we find a
              ;; JSXExpressionContainer as the JSXAttribute’s value which
              ;; we should associate with the JSXAttribute.
              (setq expr-attribute-beg (match-beginning 0)))))
         ;; There is nothing more to check; this either isn’t JSX, or
         ;; the tag is incomplete.
         (t (throw 'stop nil)))))
    (when unambiguous
      ;; Save JSXBoundaryElement’s name’s match data for font-locking.
      (if name-beg (put-text-property name-beg (1+ name-beg) 'ocen-tag-name name-match-data))
      ;; Make the opening “<” an open parenthesis.
      (put-text-property tag-beg (1+ tag-beg) 'syntax-table
                         (eval-when-compile (string-to-syntax "(>")))
      ;; Prevent “out of range” errors when typing at the end of a buffer.
      (setq tag-end (if (eobp) (1- (point)) (point)))
      ;; Mark beginning and end of tag for font-locking.
      (put-text-property tag-beg (1+ tag-beg) 'ocen-tag-beg (cons type tag-end))
      (put-text-property tag-end (1+ tag-end) 'ocen-tag-end tag-beg)
      ;; Use text properties to extend the syntax-propertize region
      ;; backward to the beginning of the JSXBoundaryElement in the
      ;; future.  Typically the closing angle bracket could suggest
      ;; extending backward, but that would also involve more rigorous
      ;; parsing, and the closing angle bracket may not even exist yet
      ;; if the JSXBoundaryElement is still being typed.
      (put-text-property tag-beg (1+ tag-end) 'syntax-multiline t))
    (if (ocen--at-enclosing-tag-child-p) (ocen--syntax-propertize-tag-text end))))

(defconst ocen--text-properties
  (list
   'ocen-tag-beg nil 'ocen-tag-end nil 'ocen-close-tag-pos nil
   'ocen-tag-name nil 'ocen-attribute-name nil 'ocen-string nil
   'ocen-text nil 'ocen-expr nil 'ocen-expr-attribute nil)
  "Plist of text properties added by `ocen-syntax-propertize'.")

(defun ocen-syntax-propertize (start end)
  ;; Ocen allows immediate regular expression objects, written /.../.
  (goto-char start)
  (if ocen-syntax (remove-text-properties start end ocen--text-properties))
  (ocen-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ;; Distinguish /-division from /-regexp chars (and from /-comment-starter).
    ;; FIXME: Allow regexps after infix ops like + ...
    ;; https://developer.mozilla.org/en/Ocen/Reference/Operators
    ;; We can probably just add +, -, <, >, %, ^, ~, ?, : at which
    ;; point I think only * and / would be missing which could also be added,
    ;; but need care to avoid affecting the // and */ comment markers.
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
	 (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   ;; If the / is at the beginning of line, we have to check
                   ;; the end of the previous text.
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (ocen-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b"))
    ("<" (0 (ignore
             (when ocen-syntax
               ;; Not inside a comment or string.
               (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
                 (ocen--syntax-propertize-tag end)))))))
   (point) end))

(defconst ocen--prettify-symbols-alist
  '(("=>" . ?⇒)
    (">=" . ?≥)
    ("<=" . ?≤))
  "Alist of symbol prettifications for Ocen.")

;;; Indentation

(defconst ocen--possibly-braceless-keyword-re
  (ocen--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst ocen--declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
  "Regular expression matching variable declaration keywords.")

(defconst ocen--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\|"
          (ocen--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")

(defun ocen--looking-at-start-tag-p ()
  "Non-nil if a JSXOpeningElement immediately follows point."
  (let ((tag-beg (get-text-property (point) 'ocen-tag-beg)))
    (and tag-beg (memq (car tag-beg) '(open self-closing)))))

(defun ocen--looking-at-operator-p ()
  "Return non-nil if point is on a Ocen operator, other than a comma."
  (save-match-data
    (and (looking-at ocen--indent-operator-re)
         (or (not (eq (char-after) ?:))
             (save-excursion
               (ocen--backward-syntactic-ws)
               (when (= (char-before) ?\)) (backward-list))
               (and (ocen--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (eq (char-after) ??))))
         (not (and
               (eq (char-after) ?/)
               (save-excursion
                 (eq (nth 3 (syntax-ppss)) ?/))))
         (not (and
               (eq (char-after) ?*)
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" ocen--name-re " *(\\)"))
               (save-excursion
                 (ocen--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{)))))
         ;; “<” isn’t necessarily an operator in JSX.
         (not (and ocen-syntax (ocen--looking-at-start-tag-p))))))

(defun ocen--find-newline-backward ()
  "Move backward to the nearest newline that is not in a block comment."
  (let ((continue t)
        (result t))
    (while continue
      (setq continue nil)
      (if (search-backward "\n" nil t)
          (let ((parse (syntax-ppss)))
            ;; We match the end of a // comment but not a newline in a
            ;; block comment.
            (when (nth 4 parse)
              (goto-char (nth 8 parse))
              ;; If we saw a block comment, keep trying.
              (unless (nth 7 parse)
                (setq continue t))))
        (setq result nil)))
    result))

(defun ocen--looking-back-at-end-tag-p ()
  "Non-nil if a JSXClosingElement immediately precedes point."
  (get-text-property (point) 'ocen-tag-end))

(defun ocen--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (ocen--looking-at-operator-p)
        (if (eq (char-after) ?/)
            (prog1
                (not (nth 3 (syntax-ppss (1+ (point)))))
              (forward-char -1))
          (or
           (not (memq (char-after) '(?- ?+)))
           (progn
             (forward-comment (- (point)))
             (not (memq (char-before) '(?, ?\[ ?\())))))
      (and (ocen--find-newline-backward)
           (progn
             (skip-chars-backward " \t")
             (and
              ;; The “>” at the end of any JSXBoundaryElement isn’t
              ;; part of a continued expression.
              (not (and ocen-syntax (ocen--looking-back-at-end-tag-p)))
              (progn
                (or (bobp) (backward-char))
                (and (> (point) (point-min))
                     (save-excursion
                       (backward-char)
                       (not (looking-at "[/*]/\\|=>")))
                     (ocen--looking-at-operator-p)
                     (and (progn (backward-char)
                                 (not (looking-at "\\+\\+\\|--\\|/[/*]"))))))))))))

(defun ocen--skip-term-backward ()
  "Skip a term before point; return t if a term was skipped."
  (let ((term-skipped nil))
    ;; Skip backward over balanced parens.
    (let ((progress t))
      (while progress
        (setq progress nil)
        ;; First skip whitespace.
        (skip-syntax-backward " ")
        ;; Now if we're looking at closing paren, skip to the opener.
        ;; This doesn't strictly follow JS syntax, in that we might
        ;; skip something nonsensical like "()[]{}", but it is enough
        ;; if it works ok for valid input.
        (when (memq (char-before) '(?\] ?\) ?\}))
          (setq progress t term-skipped t)
          (backward-list))))
    ;; Maybe skip over a symbol.
    (let ((save-point (point)))
      (if (and (< (skip-syntax-backward "w_") 0)
                 (looking-at ocen--name-re))
          ;; Skipped.
          (progn
            (setq term-skipped t)
            (skip-syntax-backward " "))
        ;; Did not skip, so restore point.
        (goto-char save-point)))
    (when (and term-skipped (> (point) (point-min)))
      (backward-char)
      (eq (char-after) ?.))))

(defun ocen--skip-terms-backward ()
  "Skip any number of terms backward.
Move point to the earliest \".\" without changing paren levels.
Returns t if successful, nil if no term was found."
  (when (ocen--skip-term-backward)
    ;; Found at least one.
    (let ((last-point (point)))
      (while (ocen--skip-term-backward)
        (setq last-point (point)))
      (goto-char last-point)
      t)))

(defun ocen--chained-expression-p ()
  "A helper for ocen--proper-indentation that handles chained expressions.
A chained expression is when the current line starts with '.' and the
previous line also has a '.' expression.
This function returns the indentation for the current line if it is
a chained expression line; otherwise nil.
This should only be called while point is at the start of the line's content,
as determined by `back-to-indentation'."
  (when ocen-chain-indent
    (save-excursion
      (when (and (eq (char-after) ?.)
                 (ocen--continued-expression-p)
                 (ocen--find-newline-backward)
                 (ocen--skip-terms-backward))
        (current-column)))))

(defun ocen--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward " \t\n}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
          (ocen--re-search-backward "\\_<do\\_>" (line-beginning-position) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (ocen--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (ocen--re-search-forward
                           "\\_<while\\_>" (line-end-position) t))
		     (= (current-indentation) saved-indent)))))))))


(defun ocen--ctrl-statement-indentation ()
  "Helper function for `ocen--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (line-beginning-position) (point-min)))
                 (not (looking-at "[{]"))
                 (ocen--re-search-backward "[[:graph:]]" nil t)
                 (progn
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at ocen--possibly-braceless-keyword-re))
                 (memq (char-before) '(?\s ?\t ?\n ?\}))
                 (not (ocen--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) ocen-indent-level)))))

(defun ocen--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c ocen-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun ocen--same-line (pos)
  (and (>= pos (line-beginning-position))
       (<= pos (line-end-position))))

(defun ocen--multi-line-declaration-indentation ()
  "Helper function for `ocen--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (forward-sexp-function ; Use Lisp version.
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at ocen--declaration-keyword-re))
        (let ((pt (point)))
          (when (looking-at ocen--indent-operator-re)
            (goto-char (match-end 0)))
          ;; The "operator" is probably a regexp literal opener.
          (when (nth 3 (syntax-ppss))
            (goto-char pt)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (ocen--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at ocen--indent-operator-re)
                                   (ocen--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (ocen--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at ocen--declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun ocen--indent-in-array-comp (bracket)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (save-excursion
      (goto-char bracket)
      (when (looking-at "\\[")
        (forward-char 1)
        (ocen--forward-syntactic-ws)
        (if (looking-at "[[{]")
            (let (forward-sexp-function) ; Use Lisp version.
              (condition-case nil
                  (progn
                    (forward-sexp)       ; Skip destructuring form.
                    (ocen--forward-syntactic-ws)
                    (if (and (/= (char-after) ?,) ; Regular array.
                             (looking-at "for"))
                        (match-beginning 0)))
                (scan-error
                 ;; Nothing to do here.
                 nil)))
          ;; To skip arbitrary expressions we need the parser,
          ;; so we'll just guess at it.
          (if (and (> end (point)) ; Not empty literal.
                   (re-search-forward "[^,]]* \\(for\\_>\\)" end t)
                   ;; Not inside comment or string literal.
                   (let ((status (parse-partial-sexp bracket (point))))
                     (and (= 1 (car status))
                          (not (nth 8 status)))))
              (match-beginning 1)))))))

(defun ocen--array-comp-indentation (bracket for-kwd)
  (if (ocen--same-line for-kwd)
      ;; First continuation line.
      (save-excursion
        (goto-char bracket)
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun ocen--maybe-goto-declaration-keyword-end (parse-status)
  "Helper function for `ocen--proper-indentation'.
Depending on the value of `ocen-indent-first-init', move
point to the end of a variable declaration keyword so that
indentation is aligned to that column."
  (cond
   ((eq ocen-indent-first-init t)
    (when (looking-at ocen--declaration-keyword-re)
      (goto-char (1+ (match-end 0)))))
   ((eq ocen-indent-first-init 'dynamic)
    (let ((bracket (nth 1 parse-status))
          declaration-keyword-end
          at-closing-bracket-p
          forward-sexp-function ; Use Lisp version.
          comma-p)
      (when (looking-at ocen--declaration-keyword-re)
        (setq declaration-keyword-end (match-end 0))
        (save-excursion
          (goto-char bracket)
          (setq at-closing-bracket-p
                (condition-case nil
                    (progn
                      (forward-sexp)
                      t)
                  (error nil)))
          (when at-closing-bracket-p
            (while (forward-comment 1))
            (setq comma-p (looking-at-p ","))))
        (when comma-p
          (goto-char (1+ declaration-keyword-end))))))))

(defconst ocen--line-terminating-arrow-re "=>\\s-*\\(/[/*]\\|$\\)"
  "Regexp matching the last \"=>\" (arrow) token on a line.
Whitespace and comments around the arrow are ignored.")

(defun ocen--broken-arrow-terminates-line-p ()
  "Helper function for `ocen--proper-indentation'.
Return non-nil if the last non-comment, non-whitespace token of the
current line is the \"=>\" token (of an arrow function)."
  (let ((from (point)))
    (end-of-line)
    (re-search-backward ocen--line-terminating-arrow-re from t)))

;; When indenting, we want to know if the line is…
;;
;;   - within a multiline JSXElement, or
;;   - within a string in a JSXBoundaryElement, or
;;   - within JSXText, or
;;   - within a JSXAttribute’s multiline JSXExpressionContainer.
;;
;; In these cases, special XML-like indentation rules for JSX apply.
;; If JS is nested within JSX, then indentation calculations may be
;; combined, such that JS indentation is “relative” to the JSX’s.
;;
;; Therefore, functions below provide such contextual information, and
;; `ocen--proper-indentation' may call itself once recursively in order
;; to finish calculating that “relative” JS+JSX indentation.

(defun ocen--context ()
  "Determine JSX context and move to enclosing JSX."
  (let ((pos (point))
        (parse-status (syntax-ppss))
        (enclosing-tag-pos (ocen--enclosing-tag-pos)))
    (when enclosing-tag-pos
      (if (< pos (nth 1 enclosing-tag-pos))
          (if (nth 3 parse-status)
              (list 'string (nth 8 parse-status))
            (list 'tag (nth 0 enclosing-tag-pos) (nth 1 enclosing-tag-pos)))
        (list 'text (nth 0 enclosing-tag-pos) (nth 2 enclosing-tag-pos))))))

(defun ocen--contextual-indentation (line context)
  "Calculate indentation column for LINE from CONTEXT.
The column calculation is based off of `sgml-calculate-indent'."
  (pcase (nth 0 context)

    ('string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (nth 1 context))
		 (zerop (forward-line -1))
		 (looking-at "[ \t]*$")))
     (if (> (point) (nth 1 context))
	 ;; Previous line is inside the string.
	 (current-indentation)
       (goto-char (nth 1 context))
       (1+ (current-column))))

    ('tag
     ;; Special JSX indentation rule: a “dangling” closing angle
     ;; bracket on its own line is indented at the same level as the
     ;; opening angle bracket of the JSXElement.  Otherwise, indent
     ;; JSXAttribute space like SGML.
     (if (and
          ocen-align->-with-<
          (progn
            (goto-char (nth 2 context))
            (and (= line (line-number-at-pos))
                 (looking-back "^\\s-*/?>" (line-beginning-position)))))
         (progn
           (goto-char (nth 1 context))
           (current-column))
       ;; Indent JSXAttribute space like SGML.
       (goto-char (nth 1 context))
       ;; Skip tag name:
       (skip-chars-forward " \t")
       (skip-chars-forward "^ \t\n")
       (skip-chars-forward " \t")
       (if (not (eolp))
	   (current-column)
         ;; This is the first attribute: indent.
         (goto-char (+ (nth 1 context) ocen-attribute-offset))
         (+ (current-column) (or ocen-indent-level ocen-indent-level)))))

    ('text
     ;; Indent to reflect nesting.
     (goto-char (nth 1 context))
     (+ (current-column)
        ;; The last line isn’t nested, but the rest are.
        (if (or (not (nth 2 context)) ; Unclosed.
                (< line (line-number-at-pos (nth 2 context))))
            (or ocen-indent-level ocen-indent-level)
          0)))

    ))

(defun ocen--enclosing-curly-pos ()
  "Return position of enclosing “{” in a “{/}” pair about point."
  (let ((parens (reverse (nth 9 (syntax-ppss)))) paren-pos curly-pos)
    (while
        (and
         (setq paren-pos (car parens))
         (not (when (= (char-after paren-pos) ?{)
                (setq curly-pos paren-pos)))
         (setq parens (cdr parens))))
    curly-pos))

(defun ocen--goto-outermost-enclosing-curly (limit)
  "Set point to enclosing “{” at or closest after LIMIT."
  (let (pos)
    (while
        (and
         (setq pos (ocen--enclosing-curly-pos))
         (if (>= pos limit) (goto-char pos))
         (> pos limit)))))

(defun ocen--expr-attribute-pos (start limit)
  "Look back from START to LIMIT for a JSXAttribute."
  (save-excursion
    (goto-char start) ; Skip the first curly.
    ;; Skip any remaining enclosing curlies until the JSXElement’s
    ;; beginning position; the last curly ought to be one of a
    ;; JSXExpressionContainer, which may refer to its JSXAttribute’s
    ;; beginning position (if it has one).
    (ocen--goto-outermost-enclosing-curly limit)
    (get-text-property (point) 'ocen-expr-attribute)))

(defvar ocen--indent-col nil
  "Baseline column for JS indentation within JSX.")

(defvar ocen--indent-attribute-line nil
  "Line relative to which indentation uses JSX as a baseline.")

(defun ocen--expr-indentation (parse-status pos col)
  "Indent using PARSE-STATUS; relative to POS, use base COL.
To indent a JSXExpressionContainer’s expression, calculate the JS
indentation, using JSX indentation as the base column when
indenting relative to the beginning line of the
JSXExpressionContainer’s JSXAttribute (if any)."
  (let* ((ocen--indent-col col)
         (ocen--indent-attribute-line
          (if pos (line-number-at-pos pos))))
    (ocen--proper-indentation parse-status)))

(defun ocen--indentation (parse-status)
  "Helper function for `ocen--proper-indentation'.
Return the proper indentation of the current line if it is part
of a JSXElement expression spanning multiple lines; otherwise,
return nil."
  (let ((current-line (line-number-at-pos))
        (curly-pos (ocen--enclosing-curly-pos))
        nth-context context expr-p beg-line col
        forward-sexp-function) ; Use the Lisp version.
    ;; Find the immediate context for indentation information, but
    ;; keep going to determine that point is at the N+1th line of
    ;; multiline JSX.
    (save-excursion
      (while
          (and
           (setq nth-context (ocen--context))
           (progn
             (unless context
               (setq context nth-context)
               (setq expr-p (and curly-pos (< (point) curly-pos))))
             (setq beg-line (line-number-at-pos))
             (and
              (= beg-line current-line)
              (or (not curly-pos) (> (point) curly-pos)))))))
    ;; When on the second or later line of JSX, indent as JSX,
    ;; possibly switching back to JS indentation within
    ;; JSXExpressionContainers, possibly using the JSX as a base
    ;; column while switching back to JS indentation.
    (when (and context (> current-line beg-line))
      (save-excursion
        (setq col (ocen--contextual-indentation current-line context)))
      (if expr-p
          (ocen--expr-indentation
           parse-status (ocen--expr-attribute-pos curly-pos (nth 1 context)) col)
        col))))

;; this is the function that was causing the problem.
;; diff with js.el version to determine the changes
(defun ocen--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (ocen--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((when (and ocen-syntax (not ocen--indent-col))
             (save-excursion (ocen--indentation parse-status))))
          ((and (eq (char-after) ?#)
                (save-excursion
                  (forward-char 1)
                  (looking-at-p cpp-font-lock-keywords-source-directives)))
           0)
          ((save-excursion (ocen--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (ocen--same-line bracket))
                  (setq beg (ocen--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (ocen--array-comp-indentation bracket beg))))
          ((ocen--chained-expression-p))
          ((ocen--ctrl-statement-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (ocen--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (or (not ocen-indent-align-list-continuation)
                     (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (save-excursion (forward-char) (ocen--broken-arrow-terminates-line-p)))
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (ocen--maybe-goto-declaration-keyword-end parse-status)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (+
                            (cond
                             ((and ocen--indent-attribute-line
                                   (eq ocen--indent-attribute-line
                                       (line-number-at-pos)))
                              ocen--indent-col)
                             (t
                              (current-column)))
                            (cond (same-indent-p 0)
                                  (continued-expr-p
                                   (+ ocen-indent-level
                                      ocen-expr-indent-offset))
                                  (t
                                   (+ ocen-indent-level
                                      (pcase (char-after (nth 1 parse-status))
                                        (?\( ocen-paren-indent-offset)
                                        (?\[ ocen-square-indent-offset)
                                        (?\{ ocen-curly-indent-offset))))))))
                     (if in-switch-p
                         (+ indent ocen-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((ocen--continued-expression-p)
           (+ ocen-indent-level ocen-expr-indent-offset))
          (t (prog-first-column)))))

(defun ocen-indent-line ()
  "Indent the current line as Ocen."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (line-beginning-position))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (ocen--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(defconst ocen-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Define operators and punctuation
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; Define comments
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Define strings and char
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Underscores are part of words
    (modify-syntax-entry ?_ "w" table)
    table))

(defconst ocen-keywords
  '("let" "const" "def" "if" "else" "match"
    "for" "while" "namespace" "import" "enum"
    "struct" "union" "yield" "as" "and" "or"
    "not" "main" "extern" "false" "true" "atomic"
    "then" "in" "return")
  "Ocen language keywords.")

(defconst ocen-builtin-types
  '("u8" "i8" "u16" "i16" "u32" "i32" "u64" "i64"
    "f32" "f64" "char" "str" "untyped_ptr" "bool")
  "Ocen built-in types.")

(defvar ocen-font-lock-keywords
  `((,(regexp-opt ocen-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt ocen-builtin-types 'symbols) . font-lock-type-face)
    ("\\<def\\s-+\\([A-Za-z0-9_:]+\\)" 1 font-lock-function-name-face)
    ;; String literals
    ("\"\\(?:\\\\.\\|[^\"]\\)*\"" . font-lock-string-face)
    ;; Backtick-based format strings
    ("`\\(?:\\\\.\\|[^`]\\)*`" . font-lock-string-face)
    ;; f-string format (Python-style)
    ("f\"\\(?:\\\\.\\|[^\"\\]\\)*\"" . font-lock-string-face)))

(defvar ocen-defun-regexp "^\\s-*\\(def\\|enum\\|struct\\|union\\).*"
  "Regular expression to match the start of a function, enum, or struct definition in Ocen.")

;;;###autoload
(define-derived-mode ocen-mode prog-mode "Ocen"
  "Major mode for editing Ocen files."
  :syntax-table ocen-mode-syntax-table
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s *")
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local buffer-file-coding-system 'utf-8-unix)
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local indent-line-function #'ocen-indent-line)
  (setq-local ocen-indent-level tab-width)
  (setq-local defun-prompt-regexp ocen-defun-regexp)

  ;; Set up syntax highlighting
  (setq-local font-lock-defaults '((ocen-font-lock-keywords)))
  ;; Set up Imenu
  (setq-local imenu-generic-expression
              '(("Enum" "^enum \\([A-Za-z0-9_]+\\)" 1)
                ("Struct" "^struct \\([A-Za-z0-9_]+\\)" 1)
                ("Union" "^union \\([A-Za-z0-9_]+\\)" 1)
                ("Function" "^def \\([ A-Za-z0-9_:]+\\)" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.oc\\'" . ocen-mode))

(defun ocen-move-if-bol-and-whitespace-only ()
  "Move cursor to the end of the line if it's at the beginning and
the line has only whitespace."
  (when (and (derived-mode-p 'ocen-mode) ; Ensure we're in `ocen-mode`
             (looking-at-p "^[ \t]*$"))  ; Line has only whitespace
    (end-of-line)))

(defun setup-ocen-mode-whitespace-cursor-movement ()
  "Set up cursor movement for whitespace-only lines in Ocen mode."
  (add-hook 'post-command-hook 'ocen-move-if-bol-and-whitespace-only nil t))

(add-hook 'ocen-mode-hook 'setup-ocen-mode-whitespace-cursor-movement)

(provide 'ocen-mode)

;;; ocen-mode.el ends here
