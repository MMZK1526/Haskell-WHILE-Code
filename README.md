Author: MMZK1526 *et ut* Yitang Chen

# While Language
## Introduction
The ```While Language``` is a simple Turing-Complete language introduced in the Imperial College Course *Models of Computation*. I have made an interpreter in ```Haskell``` for this language.  

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
C ::= C; C | v := E | if E then C else C | while E do C | Skip | return [E]
E ::= true | false | not E | E or E | E and E | E < E | E = E | E > E | E != E | E <= E | E >= E | n | v | E + E | E * E | E - E | E / E | E % E
```

I have introduced a number of new operators as well as extended the range of ```n``` to $\mathbb Z$ instead of $\mathbb N$. Notably, there is a ```return``` syntax. In the original ```While```, the answer configuration is always ```<Skip, s>```, in other word, the information is solely reflected by the context[^1] ```s```. Here by adding ```return```, we can have another answer configuraton ```<return E, s>```, where ```E``` is either an integer or a boolean, so that we don't have to look into the context to see a result. As you may have anticipated, the ```return``` command terminates the entire program.  

## Syntax
The syntax of my ```While``` code resembles ```python``` in the sense that no variable declarations and semicolons are required, but with several variations.  

Firstly, every instruction/expression cannot span across several lines. Therefore, it's recommended to split long expressions into several separate expressions.  

Secondly, one block of indentation must be exactly 2 spaces. I am planning to relax on this restriction.  

Thirdly, paranthesis are not allowed for ```if``` and ```while``` statements.  For example, we cannot write ```if(x > 2)```, only ```if x > 2```. We can add colons at the end (as in ```python```), but that is entirely optional.  

In ```While```, line comments start with ```:)```. There are no block comments.  

The following are the allowed operators in ```While```:  
| Symbol | Meaning | Note |
|--------|---------|------|
|   `+`    |  Plus   |      |
|   `-`    |  Minus  |      |
|   `*`    | Multiply |     |
|   `/`    | Divide  | rounded down|
|   `%`    |   Mod   |      |
|  `:=`    | Assignment | similar to ```=``` in most languages |
|`<`| Less Than |we don't allow inequalities between boolean values|
|`>`|Greater Than|we don't allow inequalities between boolean values|
|`<=`|Less Than or Equal To|we don't allow inequalities between boolean values|
|`>=`|Greater Than or Equal To|we don't allow inequalities between boolean values|
|`==`|Equal To|can also be written as `=`|
|`!=`|Not Equal To| |
|`&&`|And|can also be written as `&`|
|`⎮⎮`[^2]|Or|can also be written as `⎮`|
|`!`|Not| |
|`:`|Optional ending of a `if`, `else` or `while` statement| |

[^2]: Note that the symbol in the table (```⎮```) is not actually the vertical bar (```|```), however, the latter confuses the markdown table and cannot be rendered properly.  

The following are the reserved words in ```While```:  
| Word | Meaning | Note |
|------|---------|------|
|`true`|  Boolean TRUE |      |
|`false`|Boolean FALSE||
|`if`|If Statement||
|`else`|The (optional) other branch of `if`||
|`elif`|Similar to `else if`|without this, nested if would be a pain|
|`while`|While Statement||
|`return`|Terminate the program and returns value|if returning a value, the keyword can be omitted|

The body of a control flow can be empty, which is denoted by an empty line (or a comment).  

[^1]: In the course, the partial function that records the variables are called "state", but here we use the term "context" to differentiate it from the ```State``` Monad that is widely used in my implementation.  

# While Intepreter CLI
To use the CLI, we need to have ```GHC``` environment. We also need the packages ```Data.Map``` and ```Text.Parsec```.  

To install these packages using ```cabal``` (assuming you already have ```GHC```), run the following:

```
cabal install --lib containers
cabal install --lib parsec
```

Since I do not use ```stack```, I'm not sure how to install them with that. It should be similar though :)  

To install the CLI, simply run ```make``` from the root directory. It will generate the CLI called "whilei".  

Here is an example of running and debugging a simple [factorial function](##Example). The full documentaton can be found [here](##Documentation).  

Note that when ran without any arguments, we would start the [While Interactive Shell](##while-interactive-shell).  

## Example
The most basic way of using the CLI is to first navigate to the root directory of this repo (namely the same folder as [Main.hs](Main.hs)), then run the following:  

```whilei filename [arg1=val1 arg2=var2 ...]```

For example, we have a [factorial example](Examples/factorial.while) which takes a parameter ```x``` and returns the factorial of this number. For how to write your own ```While`` code, see [Syntax](#Syntax).  

We can use the ```While``` program to calculate ```3!``` by running the following:

```
> whilei Examples/factorial.while x:=3
Result: 6
```

We can also pass in a debug option. The most basic option is ```-d```, which would start an interactive debugger where we can go through the evaluation step by step:  

```
> whilei -d Examples/factorial.while x:=3
Press 'x' to dump the context.
Press 's' to go to the next step.
Press Enter to go to the next line.
Press 'r' to go straight to the result.
Press 'q' to quit.

Step 0:
a := 1
while x > 0
  a := a * x
  x := x - 1
return a
> s
Step 1:
[DO NOTHING]
while x > 0
  a := a * x
  x := x - 1
return a
> x
Context: [("a",1),("x",3)]
Rules applied :[E_ASSIGN_VAL]
> s
Step 2:
while x > 0
  a := a * x
  x := x - 1
return a
> x
Context: [("a",1),("x",3)]
Rules applied :[E_SKIP]
> r
Program completed after 47 steps!
Result: 6
Context: [("a",6),("x",0)]
```

Note that after entering ```x```, the debugger will dump the current context as well as the rules used in that particular step of calculation.  

We can also dump out the entire steps with the ```--debug=full``` option:   

```
> whilei --debug=full Examples/factorial.while x:=1
Step 0:
a := 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 1:
[DO NOTHING]
while x > 0
  a := a * x
  x := x - 1
return a

Step 2:
while x > 0
  a := a * x
  x := x - 1
return a

Step 3:
if x > 0
  a := a * x
  x := x - 1
  while x > 0
    a := a * x
    x := x - 1
return a

Step 4:
if 1 > 0
  a := a * x
  x := x - 1
  while x > 0
    a := a * x
    x := x - 1
return a

Step 5:
if true
  a := a * x
  x := x - 1
  while x > 0
    a := a * x
    x := x - 1
return a

Step 6:
a := a * x
x := x - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 7:
a := 1 * x
x := x - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 8:
a := 1 * 1
x := x - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 9:
a := 1
x := x - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 10:
[DO NOTHING]
x := x - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 11:
x := x - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 12:
x := 1 - 1
while x > 0
  a := a * x
  x := x - 1
return a

Step 13:
x := 0
while x > 0
  a := a * x
  x := x - 1
return a

Step 14:
[DO NOTHING]
while x > 0
  a := a * x
  x := x - 1
return a

Step 15:
while x > 0
  a := a * x
  x := x - 1
return a

Step 16:
if x > 0
  a := a * x
  x := x - 1
  while x > 0
    a := a * x
    x := x - 1
return a

Step 17:
if 0 > 0
  a := a * x
  x := x - 1
  while x > 0
    a := a * x
    x := x - 1
return a

Step 18:
if false
  a := a * x
  x := x - 1
  while x > 0
    a := a * x
    x := x - 1
return a

Step 19:
[DO NOTHING]
return a

Step 20:
return a

Step 21:
return 1

Context: [("a",1),("x",0)]
```

Note that this option does not dump the context after each step.  

There are more examples in the \Examples folder, feel free to try them out!  

## Documentation
The most general form of command-line arguments looks like the following:  

```whilei [-h] [--debug=full|step] <while_code.txt> [<argument_name>:=<value>] [...]```

Where ```<while_code.txt>``` is the path of the ```While``` sourcecode. For the syntax of the language, see [Syntax](#Syntax).  

If the code has undefined variables, we need to pass them as command-line arguments in the form of ```[<argument_name>:=<value>]```. For example, if the code contains undefined ```x``` and ```y```, we may pass in ```x:=1 "y := 3 * 2"```. Here we can also simplify ```:=``` to ```=```, but this is **NOT** allowed in ```While``` sourcecode as ```=``` there has the same semantic as ```==```.  

We can pass an expression to the right-hand side of ```:=```, but they must not contain another variable, even if the latter is previously assigned, thus ```x:=1 y:=3*x``` is illegal.  

The argument assignments must occur after the path of the sourcecode, otherwise there are no requirement of arguments ordering. The CLI always interprets the first non-option argument as the path, and the following non-option arguments as assignments.  

The following are the available options:  

* ```--help``` or `-h`: Show the help page. If this option is present, other options are ignored.  
* ```--debug```: Debugging options. If multiple ```--debug``` options are passed in, the first one is used.  
  * ```--debug=none```: No debugging; simply prints out the result. Default config.  
  * ```--debug=step``` or `-d`: Starts an interactive debugger that can print out the next step or show current context and rules applied on user input. See [While Debugger](#while-debugger) for more information.  
  * ```--debug=all```: Prints out all intermediate steps at once, showing the answer configuration. It does not dump the context after each step.  

Apart from the options, we can also run it without any input, which would invoke the [While Interactive Shell](#while-interactive-shell).  

## While Debugger
To use the debugger, pass the `-d` option.  

In the debugger, we can track every small-step of evaluation and shows the code after each transformation. We can also look at the variable states as well as the small-step rules being applied.  

The following are the options within the debugger:  

* `x` or `dump`: Show the current context. Will dump out all variables as well as the rules applied in the latest step.  

* `s` or `step`: Conduct one small-step evaluation.   

* `l` or `line` or enter: Evaluate to the next line of code.  

* `r` or `return` or `result`: Go straight to the result.  

* `q` or `quit`: Quit the debugger.  

## While Interactive Shell
To start the Interactive Shell (REPL), run the CLI without argument:  

```whilei```

Then we can type in any `While` expressions and instructions. For example:  

```
> whilei
Welcome to the While Interactive Shell.
Type in any expression/code or press ':q' to quit.
> 3 + 5  
8
> x := 11
> 
> while x > 1:
  > x := x - 3
  > 
> x
-1
> 
> :q
```

Note that indentations are handled automatically; to outdent, simply enter an empty line. The Interactive Shell outputs results when the latest expression has a value (*i.e* not an assignment) and the current indentations level is zero.  
