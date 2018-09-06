;; Penrose syntax highlighting defined using generic-mode

;; NOTE: you will need to manually update the keywords and syntax here if the language grammar changes
;; The domain keywords (for LA, RA, etc.) need to be present in both the Substance and Style modes

;; To use: 
;; paste into your .emacs file (usually located at ~/.emacs)
;; run `M-x eval-buffer` in your .emacs buffer
;; then run one of the following commands, depending on the kind of file:
;; `M-x penrose-dsl-mode`
;; `M-x penrose-substance-mode`
;; `M-x penrose-style-mode`

;; Resources:
;; https://www.emacswiki.org/emacs/GenericMode
;; https://stackoverflow.com/questions/3887372/simplest-emacs-syntax-highlighting-tutorial
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

(require 'generic-x)

;; Penrose DSL mode
(define-generic-mode
    'penrose-dsl-mode

  ;; Comment syntax
  '(("--")
    ("/*" . "*/"))

  ;; Keywords
  '(
    "tconstructor" "type" "operator" "predicate" "vconstructor" "value" "notation" "display"
    )

  ;; Operators, regexes
  '(
    ("<:" . 'font-lock-variable-name-face)
    ("(.*)" . 'font-lock-variable-name-face)
    (":" . 'font-lock-variable-name-face)
    )

  ;; File extensions that trigger the mode
  '("\\.dsl$")

  ;; Any other functions to call
  nil

  ;; Docstring
  "Penrose DSL book mode"
)

;; Penrose Substance mode
(define-generic-mode
    'penrose-substance-mode

  ;; Comment syntax
  '(("--")
    ("/*" . "*/"))

  ;; Keywords
  '(
    ;;;;; Domain reserved words
    ;; LA reserved words
    "VectorSpace" "Scalar" "Vector" "LinearMap" "Neg" "Scale" "AddV" "AddS" "Norm" "InnerProd" "Determinant" "Apply" "In" "From" "Not"

    ;; RA reserved words
    "Set" "Reals" "Real" "Point" "Function" "Interval" "OpenInterval" "ClosedInterval" "LeftClopenInterval" "RightClopenInterval" "CreateInterval" "CreateOpenInterval" "CreateClosedInterval" "CreateLeftClopenInterval" "CreateRightClopenInterval" "CreateFunction" "Pt" "union" "intersection" "derivativeAtP" "derivativeOverD" "integral" "apply" "compose" "Bounded" "LeftBounded" "RightBounded" "Unbounded" "Compact" "Disconnected" "Degenerate" "Empty" "Continuous" "Discontinuous" "Differentiable" "Integrable" "Invertible" "Monotonic" "Decreasing" "Increasing" "In" "In2" "Subset" "LessThan" "ClosedEnd" "OpenEnd" 

    ;; Reserved over all domains
    "AutoLabel" "All" "Label" "NoLabel"
    )

  ;; Operators, regexes
  '(
    (":=" . 'font-lock-variable-name-face)
    ("=" . 'font-lock-variable-name-face)
    ("$.*$" . 'font-lock-variable-name-face)
    ("(" . 'font-lock-variable-name-face)
    (")" . 'font-lock-variable-name-face)
    ;; ("(.*)" . 'font-lock-variable-name-face)
    )

  ;; File extensions that trigger the mode
  '("\\.sub$")

  ;; Any other functions to call
  nil

  ;; Docstring
  "Penrose Substance mode (for all domains)"
)

;; Penrose Style mode
;; TODO: highlight left side of "_ = _"
;; TODO: distinguish between DSL and Style keywords
;; TODO: auto-generate mode from grammar
;; TODO: color objective function names and property names?

(define-generic-mode
    'penrose-style-mode

  ;; Comment syntax
  '(("--")
    ("/*" . "*/"))

  ;; Keywords
  '(
    ;; commonly used selector names
    ;; "const" "Colors" "global"

    ;;;;; Domain reserved words
    ;; LA reserved words
    "VectorSpace" "Scalar" "Vector" "LinearMap" "Neg" "Scale" "AddV" "AddS" "Norm" "InnerProd" "Determinant" "Apply" "In" "From" "Not"

    ;; RA reserved words
    "Set" "Reals" "Real" "Point" "Function" "Interval" "OpenInterval" "ClosedInterval" "LeftClopenInterval" "RightClopenInterval" "CreateInterval" "CreateOpenInterval" "CreateClosedInterval" "CreateLeftClopenInterval" "CreateRightClopenInterval" "CreateFunction" "Pt" "union" "intersection" "derivativeAtP" "derivativeOverD" "integral" "apply" "compose" "Bounded" "LeftBounded" "RightBounded" "Unbounded" "Compact" "Disconnected" "Degenerate" "Empty" "Continuous" "Discontinuous" "Differentiable" "Integrable" "Invertible" "Monotonic" "Decreasing" "Increasing" "In" "In2" "Subset" "LessThan" "ClosedEnd" "OpenEnd" 

    ;; Style reserved words
    "with" "where" "LOCAL" "ensure" "encourage" "override" "delete" "OPTIMIZED"

    ;; GPIs
    "Circle" "Square" "Brace" "Parallelogram" "Arrow" "AnchorPoint" "Ellipse" "Curve" "Line" "Rectangle" "Arc" "Image" "Text"

    ;; property names
    ;; TODO
    )
  
  ;; Operators, regexes
  '(
    ("=" . 'font-lock-function-name-face)
    ("(" . 'font-lock-type-face)
    (")" . 'font-lock-type-face)
    ("{" . 'font-lock-type-face)
    ("}" . 'font-lock-type-face)
    ;; ("." . 'font-lock-operator)
    ("[0-9]+" . 'font-lock-variable-name-face)
    ;; Not sure how to combine these regexes
    ;; Also their relative orders seem to matter
    ("[A-Za-z_]+\\.[A-Za-z_]+\\.[A-Za-z_]+" . 'font-lock-constant-face)
    ("[A-Za-z_]+\\.[A-Za-z_]+" . 'font-lock-constant-face)
    )

  ;; File extensions that trigger the mode
  '("\\.sty$")

  ;; Any other functions to call
  nil

  ;; Docstring
  "Penrose Style mode (for all domains)"
)
