;we dont want to translate all the rules for our system into language
;our system can understand our rules as is


;all differentiation rules are like this:

         (rules)
pattern ----------> skeleton (with placeholders to create the answer)

   |
   | (match)
   |
   .
set of expressions


pattern match
foo - matches exactly foo
(f a b) - matches a list whose first element is f, second element is a, third element is b
(? x) - matches anything that is, and calls it x
(?c x) - matches constants, calls it x
(?v x) - matches variables, calls it x


skeletons
foo - instantiates exactly itself
(f a b) - instantiates to a 3-list which are the **result of instantiating** each of f, a, b
(:x) - instantitates to the value of x as in the matched pattern (called x during pattern match)
 


 (define dsimp (simplifier deriv-rules))
 this procedure simplifier can now take as its input deriv-rules
 and simplify them -> feed it to the system written
 this procedure is generic, such that a different system, 
  for example one to evaulate algebraic expressions

with all these elements - we would have created a language to solve
  all problems in this class of programs of pattern matching and substitution
  so we can make up whatever kind of rules we like

=> (dsimp '(dd (+ x y) x))
; will evaluate to
; (+ 1 0)


 
