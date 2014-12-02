Project 1: The NB Language

Hand in: Wednesday, October 1 (in 2 weeks).
The cryptic acronym stands for Numbers and Booleans and comes from the course book. This simple language is defined in Chapter 3 of the the TAPL book.

    t  ::= "true"                   terms
         | "false"
         | "if" t "then" t "else" t
         | numericLiteral
         | "succ" t
         | "pred" t
         | "iszero" t

    v  ::= "true"                   values
         | "false"
         | nv

    nv ::= 0                        numeric values
         | "succ" nv

 

This language has three syntactic forms: terms, which is the most general form, and two kinds of values: numeric values, and boolean values. We have extended the syntax by allowing numeric literals. They are syntactic sugar and have to be transformed during parsing to their equivalent value succ succ .. 0. The language is completely defined by the production 't', for terms. Values are a subset of terms, and for simplicity they are defined using a BNF notation, but they need not be parsed as such.

The evaluation rules are as follows:

    Computation 	Congruence
    if true then t1 else t2 → t1 	 
    if false then t1 else t2 → t2 	 
    isZero zero → true 	 
    isZero succ NV → false 	 
    pred zero → zero 	 
    pred succ NV → NV 	 
    	
    t1 → t1'
    if t1 then t2 else t3 → if t1' then t2 else t3
    t → t'
    isZero t → isZero t'
    t → t'
    pred t → pred t'
    t → t'
    succ t → succ t'

Big Step Semantics

The other style of operational semantics commonly in use is called big step sematics. Instead of defining evaluation in terms of a single step reduction, it formulates the notion of a term that evaluates to a final value, written "t ⇓ v". Here is how the big step evaluation rules would look for this language:
 

    v ⇓ v
    	(B-VALUE)
    t1 ⇓ true     t2 ⇓ v2
    if t1 then t2 else t3 ⇓ v2
    	(B-IFTRUE)
    t1 ⇓ false     t3 ⇓ v3
    if t1 then t2 else t3 ⇓ v3
    	(B-IFFALSE)
    t1 ⇓ nv1
    succ t1 ⇓ succ nv1
    	(B-SUCC)
    t1 ⇓ 0
    pred t1 ⇓ 0
    	(B-PREDZERO)
    t1 ⇓ succ nv1
    pred t1 ⇓ nv1
    	(B-PREDSUCC)
    t1 ⇓ 0
    iszero t1 ⇓ true
    	(B-ISZEROZERO)
    t1 ⇓ succ nv1
    iszero t1 ⇓ false
    	(B-ISZEROSUCC)


What you have to do:

    Write a parser that recognizes this language, using the combinator library
    Write a reduce method which performs one step of the evaluation, according to the rules listed above
    Write an eval method which implements a big step evaluator (one which evaluates a term down to a value, or it gets stuck when no rule applies). This method should implement the big step semantics defined above, and not call reduce.

Input/Output

Your program should read a string from standard input until end-of-file is encountered, which represents the input program. If the program is syntactically correct, it should print each step of the small-step reduction, starting with the input term, until it reaches a value or gets stuck. If the reduction is stuck, it should print "Stuck term: " and the term that cannot be reduced any further. Each step should be printed on one line. Then, it should print "Big step: " and the value found by using the big-step evaluation. If the evaluation gets stuck, it should print "Stuck term: " and the guilty term. If there are syntax errors, it should not attempt any reduction, and only print the error message.

Example 1:
input: if iszero pred pred 2 then if iszero 0 then true else false else false
output:

If(IsZero(Pred(Pred(Succ(Succ(Zero))))),If(IsZero(Zero),True,False),False)
If(IsZero(Pred(Succ(Zero))),If(IsZero(Zero),True,False),False)
If(IsZero(Zero),If(IsZero(Zero),True,False),False)
If(True,If(IsZero(Zero),True,False),False)
If(IsZero(Zero),True,False)
If(True,True,False)
True
Big step: True

Example 2:
input: pred succ succ succ false
output:

Pred(Succ(Succ(Succ(False))))
Stuck term: Pred(Succ(Succ(Succ(False))))
Big step: Stuck term: Succ(False)

Implementation Hints

For this project we encourage you to use an abstract syntax tree. The standard way to do this in Scala is to define case classes for each form a term can get, and construct the tree using the ^^ operator. The provided skeleton project should give you an idea about how things should look.
 

If you repeatedly get parse errors such as the following:
          [3.2] failure: end of input expected


This is because your input file contains whitespace before the end-of-input. If you use RegexParsers you can make your parser ignore trailing whitespace by adding <~opt("""\z""".r) to your top-level parser. 
