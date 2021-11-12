Author: MMZK1526 *et ut* Yitang Chen

# 50003-Models-of-Computation
Implement the key points and algorithms from the Imperial College Course Models of Computation.  

# While Language
## Introduction
The ```While Language``` is a simple Turing-Complete language introduced in the course. I have made an interpreter in ```Haskell``` for this language.  

Here I am going to introduce the syntax of the language. If you want to know how to use the interpreter straight away, go [here](#while-intepreter-cli).

The language is originally defined as the following in the course:

```
C ::= C; C | v := E | if B then C else C | while B do C | Skip
```

where

```
B ::= true | false | not B | B or B | B and B | E < E | E = E | E > E | ...
E ::= n | v | E + E | E * E | ...
```

```v``` is a variable and ```n``` is an integer.

I have modified it so that ```E``` and ```B``` are combined, but the types still matter, *i.e.* ```false + 3``` results in a type error:  

```
C ::= C; C | v := E | if E then C else C | while E do C | Skip | return E
E ::= true | false | not E | E or E | E and E | E < E | E = E | E > E | E != E | E <= E | E >= E | n | v | E + E | E * E | E - E | E / E | E % E
```

I have introduced a number of new operators as well as extended the range of ```n``` to $\mathbb Z$ instead of $\mathbb N$. Notably, there is a ```return``` syntax. In the original ```While```, the answer configuration is always ```<Skip, s>```, in other word, the information is solely reflected by the context[^1] ```s```. Here by adding ```return```, we can have another answer configuraton ```<return E, s>```, where ```E``` is either an integer or a boolean, so that we don't have to look into the context to see a result. As you may have anticipated, the ```return``` command terminates the entire program.  

## Syntax
The syntax of my ```While``` code resembles ```python``` in the sense that no variable declarations and semicolons are required, but with several variations.  

Firstly, every instruction/expression cannot span across several lines. Therefore, it's recommended to split long expressions into several separate expressions.  

Secondly, one block of indentation must be exactly 2 spaces. I am planning to relax on this restriction.  

Thirdly, paranthesis are not allowed for ```if``` and ```while``` statements.  For example, we cannot write ```if(x > 2)```, only ```if x > 2```. We can add colons at the end (as in ```python```), but that is entirely optional.  

The following are the allowed operators in ```While```:  
| Symbol | Meaning | Note |
|--------|---------|------|
|   `+`    |  Plus   |      |
|   `-`    |  Minus  |      |
|   `*`    | Multiply |     |
|   `/`    | Divide  | rounded down|
|   `%`    |   Mod   |      |
|  `:=`    | Assignment | similar to ```=``` in most languages |
|`<`| less than |we don't allow inequalities between boolean values|
|`>`|greater than|we don't allow inequalities between boolean values|
|`<=`|less than or equal to|we don't allow inequalities between boolean values|
|`>=`|greater than or equal to|we don't allow inequalities between boolean values|
|`==`|equal to|can also be written as `=`|
|`|=`|not equal to||
|`&&`|and|can also be written as `&`|
|`||`|or|can also be written as `|`|
|`!`|not||
|`:`||optional ending of a `if`, `else` or `while` statement|

The following are the reserved words in ```While```:  
TODO

## Rules
In this section, $n$ denotes an integer while $b$ denotes a boolean value.

**Big step rules of ```While```**:
* B_NUM: $\dfrac{}{\langle n, s\rangle\Downarrow \langle n, s\rangle}$
</br >

* B_BOOL: $\dfrac{}{\langle b, s\rangle\Downarrow \langle b, s\rangle}$
</br >

* B_ADD: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1+E_2, s\rangle\Downarrow \langle n_1+n_2, s''\rangle}$
</br >

* B_SUB: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1-E_2, s\rangle\Downarrow \langle n_1-n_2, s''\rangle}$
</br >

* B_MULT: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1*E_2, s\rangle\Downarrow \langle n_1*n_2, s''\rangle}$
</br >

* B_LT: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1<E_2, s\rangle\Downarrow \langle n_1<n_2, s''\rangle}$
</br >

* B_GT: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1>E_2, s\rangle\Downarrow \langle n_1>n_2, s''\rangle}$
</br >

* B_LE: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1\leq E_2, s\rangle\Downarrow \langle n_1\leq n_2, s''\rangle}$
</br >

* B_GE: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1\geq E_2, s\rangle\Downarrow \langle n_1\geq n_2, s''\rangle}$
</br >

* B_EQ_NUM: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1=E_2, s\rangle\Downarrow \langle n_1=n_2, s''\rangle}$
</br >

* B_EQ_BOOL: $\dfrac{\langle E_1, s \rangle\Downarrow \langle b_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle b_2, s''\rangle}{\langle E_1=E_2, s\rangle\Downarrow \langle b_1=b_2, s''\rangle}$
</br >

* B_NE_NUM: $\dfrac{\langle E_1, s \rangle\Downarrow \langle n_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle n_2, s''\rangle}{\langle E_1\not=E_2, s\rangle\Downarrow \langle n_1\not=n_2, s''\rangle}$
</br >

* B_NE_BOOL: $\dfrac{\langle E_1, s \rangle\Downarrow \langle b_1,s'\rangle~~\langle E_2, s'\rangle \Downarrow \langle b_2, s''\rangle}{\langle E_1\not=E_2, s\rangle\Downarrow \langle b_1\not=b_2, s''\rangle}$
</br >

* B_AND_TRUE: $\dfrac{\langle E_1,s\rangle\Downarrow \langle{\tt true},s'\rangle~~\langle E_2, s'\rangle\Downarrow \langle b, s''\rangle}{\langle E_1\land E_2, s\rangle\Downarrow \langle b, s''\rangle}$
</br >

* B_AND_FALSE: $\dfrac{\langle E_1,s\rangle\Downarrow \langle{\tt false},s'\rangle}{\langle E_1\land E_2, s\rangle\Downarrow \langle {\tt false}, s'\rangle}$
</br >

* B_OR_TRUE: $\dfrac{\langle E_1,s\rangle\Downarrow \langle{\tt true},s'\rangle}{\langle E_1\lor E_2, s\rangle\Downarrow \langle {\tt true}, s'\rangle}$
</br >

* B_OR_FALSE: $\dfrac{\langle E_1,s\rangle\Downarrow \langle{\tt false},s'\rangle~~\langle E_2, s'\rangle\Downarrow \langle b, s''\rangle}{\langle E_1\lor E_2, s\rangle\Downarrow \langle b, s''\rangle}$
</br >

[^1]: In the course, the partial function that records the variables are called "state", but here we use the term "context" to differentiate it from the ```State``` Monad that is widely used in my implementation.  

# While Intepreter CLI
To use the CLI, we need to have ```GHC``` environment. We also need the packages ```Data.Map``` and ```Text.Parsec```.  

To install these packages using ```cabal``` (assuming you already have ```GHC```), run the following:

```
cabal install --lib containers
cabal install --lib parsec
```

Since I do not use ```stack```, I'm not sure how to install them with that. It should be similar though :)  

Here is an example of running and debugging a simple [factorial function](#Example). The full documentaton can be found [here](#Documentation).  

## Example
The most basic way of using the CLI is to first navigate to the root directory of this repo (namely the same folder as [Command.hs](Command.hs)), then run the following:  

```runghc main filename [arg1=val1 arg2=var2 ...]``` [^2]

For example, we have a [factorial example](Examples/factorial.while) which takes a parameter ```x``` and returns the factorial of this number. For how to write your own ```While`` code, see [Syntax](#Syntax).  

We can use the ```While``` program to calculate ```3!``` by running the following:

```
> runghc main Examples/factorial.while x=3
Result: 6
```

We can also pass in a debug option. The most basic option is ```-d```, which would start an interactive debugger where we can go through the evaluation step by step:  

```
> runghc main -d Examples/factorial.while x=3
Press 'x' to dump the context.
Press 's' to go to the next step.
Press 'r' to go straight to the result.
Press 'q' to quit.

Step 0:
y := x
a := 1
while y > 0
  a := a * y
  y := y - 1
a
> s
Step 1:
y := 3
a := 1
while y > 0
  a := a * y
  y := y - 1
a
> x
Context: [("x",3)]
Rules applied :[E_ASSIGN_EXP,E_VAR]
> s
Step 2:
[DO NOTHING]
a := 1
while y > 0
  a := a * y
  y := y - 1
a
> x
Context: [("x",3),("y",3)]
Rules applied :[E_ASSIGN_VAL]
> r
Program completed after 50 steps!
Result: 6
Context: [("a",6),("x",3),("y",0)]
```

Note that after entering ```r```, the debugger will dump the current context as well as the rules used in that particular step of calculation.  

We can also dump out the entire steps with the ```--debug=full``` option:   

```
> runghc main --debug=full Examples/factorial.while x=1
Step 0:
y := x
a := 1
while y > 0
  a := a * y
  y := y - 1
a

Step 1:
y := 1
a := 1
while y > 0
  a := a * y
  y := y - 1
a

Step 2:
[DO NOTHING]
a := 1
while y > 0
  a := a * y
  y := y - 1
a

Step 3:
a := 1
while y > 0
  a := a * y
  y := y - 1
a

Step 4:
[DO NOTHING]
while y > 0
  a := a * y
  y := y - 1
a

Step 5:
while y > 0
  a := a * y
  y := y - 1
a

Step 6:
if y > 0
  a := a * y
  y := y - 1
  while y > 0
    a := a * y
    y := y - 1
else
  [DO NOTHING]
a

Step 7:
if 1 > 0
  a := a * y
  y := y - 1
  while y > 0
    a := a * y
    y := y - 1
else
  [DO NOTHING]
a

Step 8:
if true
  a := a * y
  y := y - 1
  while y > 0
    a := a * y
    y := y - 1
else
  [DO NOTHING]
a

Step 9:
a := a * y
y := y - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 10:
a := 1 * y
y := y - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 11:
a := 1 * 1
y := y - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 12:
a := 1
y := y - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 13:
[DO NOTHING]
y := y - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 14:
y := y - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 15:
y := 1 - 1
while y > 0
  a := a * y
  y := y - 1
a

Step 16:
y := 0
while y > 0
  a := a * y
  y := y - 1
a

Step 17:
[DO NOTHING]
while y > 0
  a := a * y
  y := y - 1
a

Step 18:
while y > 0
  a := a * y
  y := y - 1
a

Step 19:
if y > 0
  a := a * y
  y := y - 1
  while y > 0
    a := a * y
    y := y - 1
else
  [DO NOTHING]
a

Step 20:
if 0 > 0
  a := a * y
  y := y - 1
  while y > 0
    a := a * y
    y := y - 1
else
  [DO NOTHING]
a

Step 21:
if false
  a := a * y
  y := y - 1
  while y > 0
    a := a * y
    y := y - 1
else
  [DO NOTHING]
a

Step 22:
[DO NOTHING]
a

Step 23:
a

Step 24:
1

Context: [("a",1),("x",1),("y",0)]
```

Note that this option does not dump the context after each step.  

There are more examples in the \Examples folder, feel free to try them out!  

## Documentation
TODO

[^2]: Of course, we can always compile the file Main.hs to achieve higher efficiency. In this case, just replace ```runghc main``` with the name of the executable.  
