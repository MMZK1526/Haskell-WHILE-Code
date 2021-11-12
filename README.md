Author: MMZK1526 *et ut* Yitang Chen

# 50003-Models-of-Computation
Implement the key points and algorithms from the Imperial College Course Models of Computation.  

# While Language

## Introduction
The ```While Language``` is a simple Turing-Complete language introduced in the course. I have made an interpreter in ```Haskell``` for this language.  

Here I am going to introduce the syntax of the language. If you want to know how to use the interpreter straight away, go [here](#While Interpreter CLI).

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
E ::= true | false | not E | E or E | E and E | E < E | E = E | E > E | E != E | E <= E | E >= E | n | v | E + E | E * E | E - E
```

I have introduced a number of new operators as well as extended the range of ```n``` to $\mathbb Z$ instead of $\mathbb N$. Notably, there is a ```return``` syntax. In the original ```While```, the answer configuration is always ```<Skip, s>```, in other word, the information is solely reflected by the context[^1] ```s```. Here by adding ```return```, we can have another answer configuraton ```<return E, s>```, where ```E``` is either an integer or a boolean, so that we don't have to look into the context to see a result. As you may have anticipated, the ```return``` command terminates the entire program.  

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

Here is an example of running and debugging a simple [factorial function](##Example). The full documentaton can be found [here](##Documentation).  

## Example

## Documentation
