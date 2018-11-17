
;;;;instruction set should be:
;;;; -orthogonal
;;;; -support self-modification
;;;; -smaller is better all else being equal
;;;; -support multithreading
;;;; -support garbage collection
;;;; -stackless
;;;; -have no preference for "nil"

;;;;on trying to only allow "move" to dereference cars and set cars:
;;;;the evaluator must dereference the function code itself and
;;;;store the result somewhere. So why not store the result in the
;;;;code itself? this fits the theme of self-modification. and allow instructions
;;;;to dereference cars since the evaluator was going to do so anyway.

;;;;in normal lisp:
;;;(a b c d) -> (a . (b . (c . (d . nil))))
;;;;in this thing:
;;;;(a b c d) -> (a . (b . (c . (d . ???))))
;;;;or????: lists are circular
;;;;(a b c d) -> #0=(a . (b . (c . (d . #0#))))

;;;;"???" and "..." mean "don't care"

;;;;on cons cells:
;;;;references to cons cells have a "direction".
;;;;the reference is flipped in place when "flip" executes.
;;;;you cannot tell whether or not a reference is flipped or not, it is relative.
;;;;syntax? <relative-front relative-back} {relative-back relative-front>

;;;;there are no symbols.

;;;;is "cons" really necessary? what if there was an [infinite?]
;;;;list of cons cells that
;;;;already existed, and you just use that instead?
;;;;(cons (se1) (se2) next)

;;;;if arg1 and arg2 are the same reference and not flipped relative to each
;;;;other, execute next-same. otherwise execute next-different.
(test (arg1) (arg2) next-same next-different)

;;;;replace destination with value
(move (destination) (value) next)

(move (...) (foo) ...) -> (move (foo) (foo) ...)

;;;;flip value in-place.
(flip (value) next)

(flip (<... ...}) ...) -> (flip ({... ...>) ...)

;;;;flip toggles the direction of the cell on each invocation.

;;;;on the necessity of "fork":
;;;;each instruction could potentially trigger more
;;;;than one "next". but that would make instructions unorthogonal?
;;;;should variable length arguments be supported? but those can be implemented
;;;;with the others?
(fork next1 next2)

;;;;on the necessity of "halt":
;;;;an evaluator could just jump to a loop that does nothing forever
;;;;relative to the other evaluators.
;;;;(halt)

;;;;could this lisp run in a limited environment? like dna?

;;;;test -> branches
;;;;move -> state
;;;;flip -> two sides of cons cell, also state?
;;;;fork -> concurrency

;;;;

;;;;moveflip operator is the only one that can
;;;;change state.
(moveflip ((destination) . (source))
	  next)

(moveflip ((...) . (<foo})) ...) -> (moveflip (({oof>) . (<foo})) ...)

;;;;if arg1 and arg2 are:
;;;; -flipped versions of the same reference execute next-same and next-different
;;;;  concurrently.
;;;; -identical execute next-same
;;;; -not identical and not flipped versions execute next-different.
(forktest ((arg1) . (arg2))
	  (next-same . next-different))

;;;;forktest -> concurrency
;;;;moveflip -> state

;;;below (a b) == (a . b) in regular lisp

(moveflip ;;instruction name
 (((destination ?)
   (source ?))
  (next
   ?)))

(forktest
 (((arg1 ?)
   (arg2 ?))
  (next-same
   next-different)))

;;;;- Tuples are the only data type.
;;;;- Tuples holds references to two other tuples.
;;;;- The reference to a tuple can be in either of two states.
;;;;  The "back" of one reference might be the "front" of another.

;;;;notation:
;;;;(a b)
;;;;"(" marks the "front" of the tuple
;;;;")" marks the "back" of the tuple
;;;;every continuous block of non-whitespace or "(" or ")"
;;;;are just references to other tuples. In this case, "a" and "b" are
;;;;just other tuples.

;;;;one mega-instruction:
;;;;- replace "to" with the flipped value of "from"
;;;;- compare arg0 and arg1:
;;;; - when identical, execute next-same
;;;; - when one is the flipped version of the other,
;;;;   execute next-same and next-different concurrently
;;;; - otherwise execute next-different
((((to ?)
   (from ?))
  ((arg0 ?)
   (arg1 ?)))
 ((next-same ?)
  (next-different ?)))
