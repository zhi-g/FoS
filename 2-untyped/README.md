
Share
Print
Project 2: Untyped Lambda Calculus

Hand in: Wednesday, October 15 (in 2 weeks)

The goal of this exercise is to familiarize yourself with the λ-calculus; your work consists of implementing a parser and a reducer for untyped λ-terms. In this exercise, we reuse the combinator parsing library introduced in previous exercise.

The provided program template is self-contained. The packaging mechanism (from sbt) will create submission only from the files that we give you. So please do not rename/create additional files as those will not be part of the submission jar.
Assignment

We use the following syntax to express λ-terms:


     t ::= x                variable
         | "\" x "." t      abstraction
         | t t              application (left associative) 
         | "(" t ")"

REMINDER: The bodies of abstractions are taken to extend as far to the right as possible, so that, for example, λx. λy. x y x stands for the same tree as λx. (λy. ((x y) x)) (cf. TAPL, p.54).

The evaluation rules for full beta-reduction are defined as follows:

     
    t1 → t1'
    t1 t2 → t1' t2
     
    t2 → t2'
    t1 t2 → t1 t2'
     
    t1 → t1'
    λx. t1 → λx. t1'
      
     (λx. t1) t2 → [x -› t2] t1 

The last rule uses substitution, whose definition we reproduce here (presented on page 71 of the TAPL book):


     [x → s]x = s

     [x → s]y = y           if y ≠ x

     [x → s](λy. t) = 
          = λy . t          if y = x               
          = λy . [x → s]t   if y ≠ x and y ∉ FV(s) (*)  

     [x → s](t1 t2) = 
          = ([x → s]t1 [x → s]t2) 

The part marked with an (*) doesn't handle the case where y ∈ FV(s). So what shall we do then? We first use of alpha-conversion for consistently renaming a bound variable in a term - actually a lambda abstraction - and then continue to apply the substitution rules. To rename a bound variable in a lambda abstraction λx.t1, one chooses a fresh name x1 for bound variable x, and consistently renames all free occurrences of x in the body t1. We use the following rules to test if a variable is free in some term:


      FV(x)      = {x}

      FV(λx.t1)  = FV(t1) \ {x}

      FV(t1 t2)  = FV(t1) ∪ FV(t2) 

 
Evaluation strategy

TAPL book (from page 56) presents several evaluation strategies for the λ-calculus:

    Under full beta-reduction any redex may be reduced at any time. This is the strategy described by the evaluation rules listed above, but this is too unrestricted in practice: we need a deterministic way to choose a certain redex, when more than one is available.
    Under normal order strategy, the leftmost, outermost redex is always reduced first. This strategy allows to reduce inside unapplied lambda terms.
    The call-by-name strategy is yet more restrictive, allowing no reductions inside lambda abstractions. Arguments are not reduced before being substituted in the body of lambda terms when applied.
    Haskell uses an optimized version known as call-by-need that, instead of re-evaluating an argument each time it is used, overwrites all occurrences of the argument with its value the first time it is evaluated, avoiding the need for subsequent re-evaluation.

Most languages use a call-by-value strategy, in which only outermost redexes are reduced and where a redex is reduced only when its right-hand side has already been reduced to a value (a function).

The call-by-value strategy is strict, in the sense that the arguments to functions are always evaluated, whether or not they are used by the body of the function. In contrast, lazy strategies such a call-by-name and call-by-need evaluate only the arguments that are actually used.

REMINDER: Except for the full beta-reduction strategy the evaluation relation is a partial function: each term t evaluates in one step to at most one term t'.
Implementation

The following steps should help you to complete the two source files located in directory src/main/scala/fos/:

1. Complete the parser productions with the start symbol Term; you also need to define some delimiters and subclasses of class Term.

Implement a reducer that uses the normal-order strategy, which applies alpha-conversion and term substitution following the above reduction rules.

def alpha(t: Term): Term = //..
def subst(t: Term, x: String, s: Term): Term = //..
def reduceNormalOrder(t: Term): Term = //..

2. Implement another reducer which uses the call-by-value evaluation strategy. For that you need to define a new set of evaluation rules, similar to the ones given above. Since we speak about values, we need to define what a value is. We can follow the book in saying that the only values are lambda abstractions.

Does it simplify the substitution operation? What would happen if we add variables to the set of values (do not implement, these are self-check questions) ? Compare the output of the two reducers, and try to understand why it is different.

def reduceWithCallByValue(t: Term): Term = //..

The method path invoked in the main function returns a stream of terms representing the big reduction step which consists of all single steps computed by reduceNormalOrder or reduceWithCallByValue.

def path(t: Term, reduce: Term => Term): Stream[Term] = //..

Input/Output

Your program should read a string from standard input until end-of-file is encountered, which represents the input program. If the program is syntactically correct, it should print each step of the small-step reduction, starting with the input term, until it gets stuck. It should do so using both reduction strategies. The provided framework already implements this behavior. You should use it as-is.

An example of the expected output:

input: \y. ((\x.x) y)
normal order: 
(\y.(\x.x) y)
(\y.y)
call-by-value: 
(\y.(\x.x) y)

Hints

    As in the previous exercise, the project is supplied with a project/Build.sbt file for sbt, and a starting point for your project.
    Don't forget to override the method toString() in the subclasses of class Term in order to get a clean output!
    Note: omitting all brackets is a wrong approach because it can lead to wrong output. However having brackets for every term is also not good.
    Streams are a form of lazy lists, and they are a perfect match for evaluations that may not terminate. Since the tail of a stream is not evaluated unless needed, you can construct a sequence of terms almost as with a list:
    - if the term can be reduced further to t1, you use Stream.cons(t1, path(t1)) to construct a new element,
    - otherwise you return an empty stream, using Stream.empty
    The Eclipse (Scala IDE) project is already included in the archive. You can use the Import > Existing projects into Workspace to load the project into Eclipse.

