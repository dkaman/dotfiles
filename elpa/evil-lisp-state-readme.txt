Adds a new Evil state called --LISP-- (<L>) with mnemonics key bindings
to navigate Lisp code and edit the sexp tree.

Philosophy
----------

`evil-lisp-state` goal is to replace the `normal state` in lisp buffers so
_you should not have the need_ to switch back and forth  between `normal state`
and `lisp state`. In the case you do, please fill an issue.

_Note that some mechanism will be provided in order to  have `insert state`
to optionally go back to `lisp state` when pressing `ESC`. Stay tuned._

To achieve this goal, this mode tries to keep the useful commands from the
`normal state` and add new commands (often with `shift` modifier) for
manipulating the data structure.

Intuitive navigation model
--------------------------

`hjkl` behaves like in the default `normal state`.

**Next sexp on the same level (sibling)**
- `L` next sexp
- `H` previous sexp

**Change level (parent/children)**
- `J` go to next sexp one level down
- `K` go to previous one level up

Example Configuration:
----------------------

override the `L` key bindings of evil `motion state`:

(require 'evil-lisp-state)
(define-key evil-normal-state-map "L" 'evil-lisp-state)

More information in the readme of the repository:
https://github.com/syl20bnr/evil-lisp-state
