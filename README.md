# Macquarie University, Department of Computing #

## COMP332 Programming Languages 2017 ##

## Hipster: a programming language for specifying cellular automata ##

### Introduction ###

A *[cellular automaton](https://en.wikipedia.org/wiki/Cellular_automaton)* consists of a regular (finite or infinite) grid of cells laid out as a multi- (often 2-) dimensional array. Each cell can contain one of a finite number of different values, such as ``off`` and ``on`` or the numbers from ``0`` to ``15`` for example. Furthermore, each cell is surrounded by a finite set of cells, called its *neighbourhood*, defined relative to (and including) that cell.

A cellular automaton *evolves* through time, which we think of being broken up into a sequence of discrete time steps or *generations* ``t = 0, 1, 2, ....``. It is initialised with a given starting state or *configuration*, specifying the initial state of the automaton at generation ``0``. It then evolves from one generation to the next according to some simple *updating rules*, which specify the contents of a cell in generation ``t+1`` in terms of the contents of its neighbourhood in generation ``t``. Usually, the rule for updating cells is the same for each one and does not change over time,

Cellular automata we originally discovered and studied by [Stanislaw Ulam](https://en.wikipedia.org/wiki/Stanislaw_Ulam) and [John von Neumann](https://en.wikipedia.org/wiki/John_von_Neumann) in the 1940s. However, they remained an academic curiosity until the 1970s when [John Conway](https://en.wikipedia.org/wiki/John_Horton_Conway) invented the [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life), a 2-dimensional automaton.

In recent years cellular automata have been used to model a wide variety of important physical and biological phenomena. Some physicists, including Nobel prize winner [Gerard 't Hooft](https://en.wikipedia.org/wiki/Gerard_%27t_Hooft), have proposed that cellular automata may provide a foundation for a unified theory of quantum mechanics and gravitation. Others have even suggested that the universe itself may be a gigantic cellular automaton. ;)

### The Game of Life ###

<img src="Neighbourhoods.jpg" alt="The Moore and von Neumann neighbourhoods" style="width: 30pc; float: right; display: inline;"/>

[Conway's game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) is by far the most well-known and well-studied cellular automaton. It is "played" on a (potentially infinite) 2-dimensional square grid, and each of its cells can be in one of two states ``alive`` or ``dead``. Whether a cell is alive in generation  ``t+1`` depends on the number of cells that are alive in its _Moore neighbourhood_ in generation ``t``.

The precise rules for updating from one generation to the next are:

1. Any live cell with fewer than two live neighbours dies (under-population).
2. Any live cell with two or three live neighbours lives on to the next generation.
3. Any live cell with more than three live neighbours dies (overpopulation).
4. Any dead cell with exactly three live neighbours becomes a live cell (reproduction).

The remarkable thing is that these very simple rules actually give rise to surprisingly complex behaviour. Part of the _game_ in the game of life is to setup a starting configuration of live cells and then see how it evolves across the generations. Many configurations die out in a couple of generations, others fall into a stable and repeating configuration of patterns, and yet others grow without bounds or grow to a large size and then die off slowly. It is this property of _emergent complexity_ from simple rules that has made cellular automata a popular pass-time and tool in sciences, like biology, where complexity abounds.

You can find many game of life simulators online, and we encourage you to explore the world of cellular automata by spending a few minutes playing with one of these. One such was used as an exercise in Java programming in COMP229 half a decade ago, and you can access the simple simulator that arose from that work at [Dom's GoL simulator](https://bitbucket.org/dominicverity/gameoflife). 

### Turing patterns ###

A nice example of a cellular automaton used to demonstrate a biological model is one designed to demonstrate the emergence of [Turing patterns](https://www.wired.com/2011/02/turing-patterns/). Here again simple rules lead to complex behaviour. In this case simple interactions between two chemicals, called _activator_ and _inhibitor_, give rise to complex patterns in the organisation of cells, colours on the skins of animals, and maybe even in the structure of galaxies. 

In the cellular automaton version of Turing's morphogenesis, each cell on a square grid represents a cell of an animal. Such cells possess two numbers which are intended to represent the concentrations of the activator and inhibitor chemicals at given point in time. The rules for updating to the next point in time are chosen to simulate the production / absorption of these chemicals by cells and the diffusion of those chemicals from areas of high to areas of low concentration.

We say that a cell is _activated_ if the concentration of activator in that cell is greater than the level of inhibitor. In the simulation you can download from [Dom's Turing Pattern Generator](https://bitbucket.org/dominicverity/turing-pattern-generator) active cells are displayed in yellow and inactive ones in black, and by selection of different rates of diffusion it gives rise to patterns very reminiscent of the shapes found in the pelts of big cats or the surfaces of corals.

### The Hipster programming language ###

The _Hipster_ programming language is a [domain specific language (DSL)](https://en.wikipedia.org/wiki/Domain-specific_language) that has been designed as a tool for specifying and exploring cellular automata. It is an [imperative](https://en.wikipedia.org/wiki/Imperative_programming) language based on a number of experimental cellular automata languages that have emerged over the years, such as [(j)Trend](http://www.complex.iastate.edu/download/Trend/) and [CAOS](https://pdfs.semanticscholar.org/023c/d739963210853d8b83599ea90c1450273a89.pdf) for example . Indeed, the name Hipster has been chosen as a pun on Trend(y).

Simulations of cellular automata may, and of course have, been written in general purpose languages such as Java and C. So it is natural to ask what the advantages of a specialised cellular automaton DSL are over simulations written in these more familiar languages. Some arguments in favour of a DSL like Hipster are:

1. It allows experimenters writing to concentrate only on specifying the cell update rules of their cellular automate. They don't need to know, or care, about details that are superfluous to them, like the precise order that cells are updated in, whether updates are made in serial or parallel (by many processor cores say), how results are rendered for display and so forth. This makes it easier for experimenters who are not programming experts to write cellular automata and it makes the code they write both simpler and easier to debug.

2. It gives the DSL compiler much more latitude to optimise the code it generates. If the cellular automaton writer is unconcerned about the precise way that updating code is executed, that gives the compiler lots of possibilities to "jiggle" things around to suit a particular execution environment. Whether updates are done in a single loop, broken down into separate loops and allocated to separate processors, whether updates are reordered relative to each other, how memory is shared and updated, when the state of the automaton is rendered to the screen and so forth are all decisions the compiler can make to optimise performance. Were we to specify the same automaton in C the experimenter would have to make all those decisions, and they would be hard to change in the future.

3. A DSL of this kind can be re-targeted to produce code for a whole range of exotic machine architectures. For example, the graphics hardware of most modern PCs is really just a huge array of very simple processing blocks. So the DSL compiler could be written to generate code that dedicated each one of those blocks to updating a single cell of the automaton. Similar arguments apply to the generation of code to execute cellular automata on [Field Programmable Gate Arrays (FPGA)](https://en.wikipedia.org/wiki/Field-programmable_gate_array), these being chips containing millions of re-configurable logic components each one of which may be devoted to the update of a single cell. Cellular automata are well suited to massively parallel execution of this kind, but the specific details need not trouble the experimenter.

4. A DSL can restrict the bad things that an experimenter can do. For example, the updating rule for a single cell should be simple enough to execute very quickly. After all, any useful cellular automaton simulation will have to execute millions (or billions) of these every second. So it would be a disaster if the experimenter wrote cell updating code that included time consuming loops or, indeed, failed to terminate. The DSL can restrict these possibilities by, for example, limiting the kinds of loops that it allows or preventing the use of recursive functions that may never terminate.

The Hipster language is intended to deliver many of these advantages. For our purposes in COMP332, it is designed to be simple to use and largely to generate code to run on the Java Virtual Machine (JVM). It could, however, easily be re-targeted to generate code for FPGAs and graphics arrays.

### The Game of Life written in Hipster ###

The following annotated implementation of the Game of Life exposes many of the peculiarities of the Hipster language:

    // Hipster cellular automaton language.
    //
    // Conway's game of life written in Hipster.
    //
    // (C) 2017, Dominic Verity, Macquarie University, All rights reserved.
    // 
    // This Source Code Form is subject to the terms of the Mozilla Public
    // License, v. 2.0. If a copy of the MPL was not distributed with this
    // file, You can obtain one at http://mozilla.org/MPL/2.0/.

    // Constant declarations giving the size of the automaton.
    // All variable declarations at top level are assumed constant.

    int width = 100;
    int height = 100;

    // Declare the dimensions of the automaton.
    // The `cyclic` suffix specifies that a dimension should
    // "wrap around" at its boundary (as in the video game Asteroids).

    dimension(width cyclic, height);

    // Declare the symbols used to refer to neighbouring cells.
    // At any point the runtime keeps track of the location of a *current*
    // cell. In the following declaration the locations of neighbours
    // are specified relative to that current cell, which is always
    // referred to by the name `me`.

    neighbourhood   // specify a Moore neighbourhood.
      N = [0,1], S = [0,-1], W = [-1,0], E = [1,0],
      NE = [1,1], SE = [1,-1], SW = [-1,-1], NW = [-1,1];

    // Declare the state information that each cell possesses.
    // For the game of life this comprises a single boolean value
    // which contains `true` if the cell is alive and `false` if
    // it is dead.

    state {
      boolean alive = false;  // default state of a cell is dead.
    }

    // The expressions `SE:alive` and `N:alive` refer to the values of
    // the `alive` field of the `SE` and `N` neighbours of the current cell
    // (in the current generation). The expression `alive` or `me:alive`
    // refer to the value of the `alive` field of the current cell.

    // In updater code we may make assignments to the fields of the current
    // cell but not to those of its neighbours. Those assignments are made to
    // the state of the cell in the *next* generation.

    // Declare the updater function.
    // During the update from one generation to the next
    // this code is called once for each cell in the simulation.

    updater {
      // Initialise counter of live neighbours.
      int count = 0;

      // Count cells that are alive.
      // The `others` keyword specifies iteration over all cells in
      // the neighbourhood except for the central cell.

      iterate current over others  
        if current:alive then count = count+1;

      // Set new state of cell according to the GoL updating rules.
      // If no explicit assignment is made to a state field then
      // the current value of that field is automatically copied to
      // the next generation.

      if alive && (count < 2 || count > 3) then alive = false;
      if !alive && count == 3 then alive = true;
    }

    // Declare some colour constants. Colours are represented by sRGB
    // integers comprising three 8-bit fields giving blue (bits 0-7),
    // green (bits 8-15) and red (bits 16-23) colour channel intensities.

    int black = 0;
    int white = 0xFFFFFF;  // Hexadecimal integer notation.

    // Declare the colour mapper function.
    // Returns a colour integer computed from the state of the
    // current cell. This colour is then used when displaying the state
    // of that cell on the screen. Code in this mapper is not allowed to
    // change the state of current cell.

    mapper {
      if alive then return(white); else return(black);
    }

    // Declare a function to place a "glider" at a specified location.

    function glider(int row, int col) {
      cell [row, column]
        iterate nbr over [N, E, SW, S, SE]
          nbr:alive = true;
    }

    // Note that the last function uses the `cell` statement. This sets the
    // current cell to a specified location and then executes the associated
    // statement (an `iterate` statement in this case). The body of the 
    // iterate loop also makes assignments to the state of neighbouring cells.
    // These operations are only allowed when initialising the automation.
    // They are not allowed during updating, the updater should only change
    // the value of the current cell and can only depend upon the values of 
    // cells in the immediate neighbourhood.

    // Declare initialisers. There can be more than one of these, each one
    // is named and the particular initialiser to run at the start of the
    // simulation can be selected by the user (default is the first listed).

    // This initialiser places a single glider in the centre of the grid.

    initialiser singleGlider {
      glider(height / 2, width / 2);
    }

    // This initialiser places 4 gliders at various equidistant points.

    initialiser fourGliders {
      glider(height / 4, width / 4);
      glider(3 * height / 4, width / 4);
      glider(height / 4, 3 * width / 4);
      glider(3 * height / 4, 3 * width / 4);
    }

### The structure of Hipster code ###

Hipster builds upon familiar principles in the design of imperative languages. A Hipster program comprises a sequence of _top level declarations_ which can be of the following constructs:

#### Declarations of global constants. ####

These are of the familiar form:

    int width = 100;

These can be of type `boolean`, `int` (32-bit integer) or `float` (double precision floating point). They can also be declared to be of type `neighbour`, such constants name locations in the neighbourhood of the current cell.

Hipster does not allow global variables, since doing so would allow information to be stored when visiting one cell and then used in updating another distant and unrelated cell. This would violate the rule that the state of a cell in the next generation should only depend on the states of the cells in its neighbourhood in the current generation.

What is more, Hipster makes no guarantees about the order in which cells will be updated and so the use of global variables in the cell updater function would lead to unpredictable behaviours.

#### Declarations of functions. #### 

These are constructs that start with the keyword `function`, like so:

    function myFunc(int p1, float p2, neighbour p3) : boolean {
      ...   // body statements
    }

Such a function can return a value of type specified after the `:` using the `return` statement,  and can either be called from within expressions (if they have a return type) or as statements (if they have no return type). Functions can only be called from source code locations after the closing brace of their bodies, so in particular they cannot call themselves recursively. 

#### A dimension declaration ####

This declaration specifies how many cells wide each dimension is:

    dimension(100 cyclic, 200);
    
In principle a cellular automaton could have an arbitrary number of dimensions, but the compiler we will build will only allow 1-D and 2-D automata. A dimension can be declared ``cyclic`` meaning that it wraps around at its edges, so cells on one side of that dimension are direct neighbours with corresponding cells on the opposite side. 

#### A neighbourhood declaration ####

A declaration of the following form

    neighbourhood   // specify a Moore neighbourhood.
      N = [0,1], S = [0,-1], W = [-1,0], E = [1,0],
      NE = [1,1], SE = [1,-1], SW = [-1,-1], NW = [-1,1];

describing the neighbourhood of a cell. This introduces a symbolic name (constant binding of type `neighbour`) for each neighbour and specifies the location of that neighbour in coordinates relative to the current cell. In this example, defining a Moore neighbourhood, we have used a traditional compass point naming scheme for neighbouring cells.

The `neighbourhood` declaration will not allow a binding of a symbol to the coordinate `[0,0]` of the current cell. Instead it binds the symbol `me` to refer to the current cell. If another name is preferred for the current cell then the `me` neighbour can be aliased by a top level, constant binding after the `neighbourhood` declaration:

    neighbour new_me = me;

#### An updater declaration ####

A declaration of the following form 

    updater {
      ..... // body statements
    }

which specifies the code which computes and sets the new state of the current cell in the next generation using its state information and the states of its neighbours. To update the simulation from one generation to the next this will be executed by its runtime exactly once for each cell. The runtime does not guarantee the order in which cells are updated, indeed it doesn't even guarantee that those updates are even made by the same machine. It could even run your automaton on a massive network of computers connected via the internet!

#### A colour mapper declaration ####

The colour mapper provides a way for the simulation to translate the state of each cell into something that can be displayed on the screen. A mapper is declared using a top level declaration of the following form

    mapper {
      ..... // body statements
    }
    
which, in essence, simply declares a specialised function whose return value in an `int`. Each cell will be displayed as a small square group of pixels in the simulator's window and the colour of that cell will be given by the integer returned on applying the mapper function to the state of that cell. Colours are represented by sRGB integers comprising three 8-bit fields giving blue (bits 0-7), green (bits 8-15) and red (bits 16-23) colour channel intensities. So the colour mapper

    int black = 0;
    int white = 0xFFFFFF;

    mapper {
      if alive then return(white); else return(black);
    }

given in the game of life example will cause a cell of the simulation to be displayed as a group of pixels in the colour white if it is alive and in the colour black if it is dead.

In order to facilitate the construction of these sRGB integers, Hipster allows programmers to use hexadecimal notation for integers and provides a number of utility functions for bit-wise manipulations of integers 


#### Initialiser declarations ####

Initialisers provide a mechanism that allows the Hipster programmer to specify how the starting state (at time ``t=0``) of a simulation will be setup. There may be any number of initialisers in a Hipster program, and these are introduced using a top level declaration of the following form:

    initialiser <name> {
      ..... // body statements
    }

An individual using a Hipster simulator can select the particular initialiser to apply at the beginning of a simulation run from a drop down by name.

Initialisers are slightly special beasts. The code they can legally contain is subject to far fewer constraints than the code that can appear in an updater. For example, it can use `for` loops and `cell` statements to iterate over the entire simulation grid and set the states of many cells. What is more, it can either set cells to pre-specified starting values or it can use a random number generator to randomly assign values to cells. The Hipster semantic analyser is constructed to ensure that the more permissive constructs that can appear in initialisers are rejected if they occur in the body of an updater or in the body of any function called from that updater.

### The grammar for top level declarations ###

We may summarise the syntactic aspects of the top level structure of a Hipster program in the following grammar:

    program : topleveldecl+
    
    topleveldecl : constdecl
                 | fundecl
                 | dimdecl
                 | statedecl
                 | neighbourdecl
                 | updaterdecl
                 | mapperdecl
                 | initdecl
    
    tipe : "boolean" | "int" | "float" | "neighbour"
    
    constdecl : tipe idndef "=" expr ";"
    
    idndecl : tipe idndef
    
    idndecllist : empty
                | idndecl ("," idndecl)* 
    
    fundecl : "function" idndef "(" idndecllist ")" (":" tipe)? "{" statement* "}"
    
    dim : expr "cyclic"?
    
    dimdecl : "dimension" "(" dim ("," dim)* ")" ";"
    
    nbrdef : idndef "=" coordexpr
    
    neighbourdecl : "neighbourhood" nbrdef ("," nbrdef)* ";"
    
    statedecl : "state" "{" vardecl* "}"
    
    updaterdecl : "updater" "{" statement* "}"
    
    mapperdecl : "mapper" "{" statement* "}"
    
    initdecl : "initialiser" idndef "{" statement* "}"
 
### Expressions in Hipster ###

Expressions in Hipster may involve values of type `boolean`, `int`, `float` or `neighbour` and the language provides a fairly standard set of operators for manipulating these. These are summarised in the following grammar fragment:

    expr : expr "&&" expr       // boolean binary operators
         | expr "||" expr
         | expr "==" expr       // relational operators
         | expr "<=" expr
         | expr ">=" expr
         | expr "<" expr
         | expr ">" expr
         | expr "+" expr        // arithmetic binary operators
         | expr "-" expr
         | expr "*" expr
         | expr "/" expr
         | expr "%" expr        // remainder operator
         | "!" expr             // unary operators
         | "-" expr
         | "+" expr
         | "(" expr ")"         // bracketed expression
         | funapp
         | idnuse
         | idnuse ":" idnuse    // reference to neighbour state variable
         | "true"
         | "false"
         | intconst
         | floatconst
         
    funapp : idnuse "(" (empty | expr ("," expr)*) ")"  // function application.
    
Hipster also provides a special expression type which is used for specifying coordinates in `cell` statements and `neighbour` declarations:

    coordexpr : "[" expr ("," expr)* "]"

This grammar is, of course, ambiguous as it stands, and it is disambiguated by the following precedence and associativity rules:

1. The following expression constructs have precedence as shown from lowest to highest with constructs on the same line having the same precedence:
    * boolean operators
    * relational operators
    * addition and subtraction
    * multiplication, division and remainder
    * all other kinds of expression 

2. All binary expression operators are left associative, except for the relational operators which are not associative.

### Statements in Hipster ###

The statements provided by Hipster will mostly be familiar. The primary novelties are a specialised statement for iterating over sets of neighbours, a construct for referring to cells by their location on the simulation grid and a case construct which can match ranges of integer and neighbour values. As discussed above, the Hipster semantic analyser restricts the use of some of these constructs within the cell updater function or any function (indirectly) called from there.

The grammar governing the statements understood by the Hipster parser follows, interspersed with brief explanations of what each statement does:

    statement = vardecl
              | block
              | ifstmt
              | iteratestmt
              | forstmt
              | returnstmt
              | cellstmt
              | assignstmt
              | funcallstmt
              | emptystmt

The syntax of variable declarations (`vardecl`) is _almost_ the same as that of the top level constants discussed above, they satisfy the following production:

    vardecl : tipe idndef ("=" expr)? ";"
    
Syntactically the only difference between the two is that constant declarations must have an initialising expression (after an `=` sign) whereas that initialisation clause is optional in in a variable declaration. 

Variables are local to the immediately enclosing block. They are scoped lexically and their extent ranges from the end of their declaration to the end of the immediately enclosing block. A block is simply a sequence of statements enclosed by a pair of curly braces (`{` and `}`):

    block : "{" stmt* "}"
    
    emptystmt : ";"
    
Assignment statements have the syntax:

    lvalue : idnuse | (idnuse ":" idnuse)

    assignstmt : lvalue "=" expr ";"
    
At run-time this evaluates the expression on the right and assigns the resulting value to the `lvalue` on the left. An `lvalue` can be a locally defined variable, a state field, or a pair `<neighbour>:<state field>` which specifies a state field of a neighbour of the current cell. we should note that:

* If a state field is referred to in an expression it evaluates to the value of that field in the **current** generation.
* If a state field is assigned to in an assignment statement then its value is set in the **next** generation.
* It is a semantic error to assign to assign a value to the state of a neighbour of the current cell in the updater function or in any function it calls.

We have already seen that functions can be called from within expressions. They can also be called as a statement with the same syntax

    funcallstmt : funapp ";"
    
this form executes the given function call and discards any returned result. A return statement

    returnstmt : "return" "(" expr? ")" ";"
    
specifies an immediate return of control from the body of a function to its call sight. The expression provided specifies the return value, which must be of the type declared in the top level function declaration or can be absent if no return type was specified. Every control path in the body of a function must contain a `return` statement.
    
The conditional `if` statement obeys a traditional semantics and its `else` branch is optional:

    ifstmt : "if" expr "then" statement ("else" statement)?

We handle the dangling `else` ambiguity in the usual way. 

The `for` loop in Hipster obeys the following syntax

    forstmt : "for" idndef "=" expr "to" expr ("step" expr)? statement

and it follows the strict semantics imposed by languages like Pascal. In particular:

* The identifier after the `for` keyword is a defining instance of a variable of type `int` whose extent is the statement comprising the body of the loop.
* The control expressions (from, to and optionally step) of a `for` loop are evaluated to values only once on loop entry.
* It is an error to make an assignment to the control variable of a `for` loop within the extent of its body.

Use of `for` loops is forbidden in the updater function or functions (indirectly) called from it. The use of `for` loops in update contexts can result in very long cell update times, and is generally unnecessary. The primary use of `for` loops is in initialiser code, where they are used to iterate over and initialise large collections of cells. 

It is, however, common for updater functions to iterate over (a subset of) the neighbours of the current cell, so Hipster provides a specialised control structure for that purpose:

    neighbourset : "all"
                 | "others"
                 | "[" idnuse ("," idnuse)* "]"
    
    iteratestmt : "iterate" idndef "over" neighbourset statement

A `neighbourset` specifies a set of neighbours to iterate over, which can be either:

* `all` meaning cells in the neighbourhood including the current cell,
* `others` meaning all cells in the neighbourhood except for the current cell,
* a specified set of neighbours given as a comma separated list between square brackets.

An `iterate` statement follows similar semantic rules to a `for` loop, viz:

* The identifier given after the `iterate` keyword is a defining instance of a variable of type `neighbour` whose extent is the statement comprising the body of the loop.
* It is an error to make an assignment to the control variable of an `iterate` loop within the extent of its body.

The effect of running an `iterate...over` statement is that it iterates over the specified set of neighbours, setting its control variable to each one in turn and then executing its body statement. So for example the code

    int count = 0;
    iterate current over others
      if current:alive then count = count+1;

counts all those cells that are alive in the neighbourhood of the current cell (excluding that cell itself) and the code

    iterate nbr over [N, E, SW, S, SE]
      nbr:alive = true;
      
iterates over the neighbour cells to the immediate _north_, _east_, _south west_, _south_, and _south east_ of the current cell and switches them to being alive. Note here that this second statement would only be legal within an initialiser, since it body makes assignments to the states of cells other than the current cell.

Another statement that is only allowed in the context of initialiser functions is the `cell` construct:

    cellstmt : "cell" coordexpr statement
    
The expression immediately after the `cell` keyword specifies the coordinate of a cell on the simulation grid and that cell is made the current cell for the execution of the following body statement. The cell updater should only have access to the current cell and it immediate neighbours, a rule that use of the `cell` construct in an updater would clearly break.

## Hipster semantic rules ##

The following sections explain the semantic rules of Hipster. A couple of plot points to look out for amongst these are:

* The Hipster language makes provision for a library or _prelude_ of primitive utility functions to be loaded when a Hipster program is executed. These largely provide a range of basic numerical functions such as maximum and minimum, trigonometric operations, a random number generator and so forth. The Hipster compiler pre-loads the initial environment with information about these symbols, the types of their parameters and so forth.

* As discussed above, there are certain statements and functions in Hipster that may **only** be executed from within an initialiser function. These include things like the `cell` and `for` statements, whose use is disallowed inside the cell updater and colour mapper functions. The use of some pre-loaded prelude functions, such as the random number generator, are also prohibited in these locations. We say that a statement or function which **may** be used in the updater function is _clean_, otherwise it is said to be _tainted_. It is the role of the semantic analyser to make sure that only clean functions may be executed from within the cell updater or colour mapper.

* In a similar fashion, access to the state fields of various cells is restricted in a number of ways. For example, while updater code my access the state of neighbour cells it may only write to the state of the current cell (`me`). On the other hand, code in the colour mapper may only access the state of the current cell and it may not write to the state of **any** cell.

### Name analysis ###

Many languages do name and type analysis together, the existance of a identifier with no type is evidence that it is not defined.  This is how Hipster's semantic analysis is done. The framework provides an implementation of name and type analysis for Hipster, along the lines of the approach used in the expression language compiler.

#### Defining and applied instances ####

Like most languages, Hipster name occurrences come in two varieties: defining occurrences (represented by `IdnDef` nodes in the tree), and applied occurrences (represented by `IdnUse` nodes). Defining occurrences appear as the:

  * names in local variable and top-level constant declarations,
  * names of state fields in a `state` declaration, 
  * neighbour names in a `neighbourhood` declaration,
  * name of a function defined in a `function` declaration,
  * names of the parameters to a function in a `function` declaration,
  * name of the initialiser block defined in an `initialiser` declaration, and
  * names of control variables in a `for` or `iterate...over` statement.

All other identifier occurrences are applied and occur as plain names in expressions and l-values. Hipster has a single name space so, for example, the names of variables can shadow identically named functions. So in the following code

     1 int width = 100;
     2 int height = 100;
     3
     4 dimension(width cyclic, height);
     5
     6 neighbourhood
     7   N = [0,1], S = [0,-1], W = [-1,0], E = [1,0],
     8   NE = [1,1], SE = [1,-1], SW = [-1,-1], NW = [-1,1];
     9
    10 state {
    11   boolean alive = false;
    12 }
    13
    14 updater {
    15   int count = 0;
    16 
    17   iterate current over others
    18     if current:alive then count = count+1;
    19
    20   if alive && (count < 2 || count > 3) then alive = false;
    21   if !alive && count == 3 then alive = true;
    22 }
    23
    24 function glider(int row, int col) {
    25   cell [row, col]
    26     iterate nbr over [N, E, SW, S, SE]
    27       nbr:alive = true;
    28 }

The following identifier instances are defining:

  * `width` and `height` in lines 1 and 2 (constant declarations),
  * `N`, `S`, `W`, `E`, `NE`, `SE`, `NW` and `SW` in lines 6-8 (neighbour definitions),
  * `alive` in line 11 (state field declaration),
  * `count` in line 15 (local variable declaration),
  * `current` in line 17 and `nbr` in line 26 (control variable of `iterate...over` statement),
  * `glider` in line 24 (declared function name), and
  * `row` and `col` in line 24 (parameter declarations).
  
All the remaining instances of identifiers in this code are applied instances. 

#### Scope rules ####

The scoping rules of Hipster are relatively straightforward, and largely follow rules you'd be familiar with from your experience of `C` or `Java`. 

In the following rules we use the term _code block_ to denote the, possibly empty, sequence of statements between a matching pair of curly braces in the body of a `function`, `updater` or `initialiser` declaration:

1. The scope of a constant defined at top-level extends from the point immediately after the defining declaration to the end of the program. In particular, that constant is **not** in scope in the initialising expression on the right of the equals sign in its declaration.

2. The scope of a local variable extends from the point immediately after the defining declaration to the end of the enclosing code block.  Here again, the variable being declared is not in scope in the initialising expression on the right of the equals sign in its declaration.

3. The scope of a state field name introduced in a top-level `state` declaration extends from the point immediately after its defining declaration to the end of the program. One exception to this rule is that any initialiser expressions associated with state fields are evaluated with respect to the environment as it exists on entry to the enclosing `state` declaration. So, in particular, we may not use any declared state fields in the initialiser expressions of any state field.

4. The scope of the neighbour names introduced in a top-level `neighbourhood` declaration extends from the point at which that defining instance occurs to the end of the program. So, in particular, each neighbour name is in scope when the neighbours that come after it are declared. This means that once a name has been introduced in a `neighbourhood` declaration then it cannot be re-defined later in that same declaration.

5. The scope of the initialiser name introduced in a top level `initialiser` declaration extends from the point just after the closing curly brace of its body to the end of the program. Initialiser names cannot occur as applied instances in a Hipster program, they are only provided as a way for the Hipster run-time to distinguish different initialisers.
    
6. The scope of the function name introduced in a top level `function` declaration extends from the point just after the closing curly brace of its body to the end of the program. In particular, a function name is not in scope in the body of its own declaration, which means that a function cannot call itself. In other words, Hipster disallows the declaration of recursive functions.

7. The scope of the parameters in a top-level `function` declaration is the body code block of that declaration.

8. The scope of a control variable defined in a `for` or `iterate...over` statement is the body of that statement.

It is an error for an identifier to be declared more than once either at the top-level or within a single code block. However, an identifier declared in an outer scope may be re-declared within an inner scope. So, for example, the following code will produce re-declaration errors:

    int size = 100;
    function size() : int { return(100); }
      // error on last line because `size` already declared in 
      // top-level scope.
    updater {
      int num = 10;
      float num = 20.2;
        // error here because `num` declared a second time in the 
        // updater scope.
    }

whereas the following code is OK:

    int size = 100;
    function f() : int { 
      float size = 23.2;
        // this is OK because `size` was originally declared in the
        // outer, top-level, scope and is now being re-declared in the
        // inner scope of this function body.
      return(100); }
    updater {
      int num = 10;
      {
        float num = 20.2;
          // statement blocks are new scopes, so this is OK because
          // `num` was originally declared in updater scope and is now
          // being re-declared in the inner scope of this statement block.
        num = num * 1.1;
      }
    }

If a name is declared inside the scope of another declaration of that same name, then the inner name hides the outer one. So in the following code

     1 updater {
     2   int i = 10;
     3   {
     4     int i = 20;
     5     i = i - 1;
     6   }
     7   i = i + 1;
     8 }

the uses of `i` in line 5 refers to the variable declared in line 4, rather than that declared on line 2. What is more, the uses of `i` in line 7 refer to the variable declared in line 2.

Note that our rules about the scopes that apply to the initialiser expressions in variable declarations imply that is the following code

     1 updater {
     2   int i = 10;
     3   {
     4     int i = 20 + i;
     5   }
     6 }

the use instance of `i` on the left hand of the equals sign in the declaration on line 4 actually refers to the variable declared in the outer scope on line 2.

#### Entities ####

When an identifier is brought into scope, an entry for it is added to the environment table. That entry is a key-value pair whose key is the name of the identifier and whose value is an _entity_ describing its declared properties.

The various kinds of entity are given as case classes, which are declared in the `SymbolTable.scala` file of the Hipster compiler source code. These entity classes are:

* `Variable` used to label those identifiers that are declared to be local variables. An object of this class encapsulates both the declared type of that variable and a reference to the (optional) initialiser expression in its declaration.

* `Constant` used to label those identifiers that are declared as constants at top-level. An object of this class encapsulates both the declared type of that constant and a reference to the initialiser expression in its declaration.

* `StateField` used to label those identifiers that are declared as cell state fields within the top-level `state` declaration. An object of this class encapsulates both the declared type of that state field and a reference to the (optional) initialiser expression in its declaration.

* `Parameter` used to label those identifiers that are declared to be parameters in a top-level `function` declaration. An object of this class encapsulates the declared type of that parameter.

* `ControlVariable` used to label those identifiers that are control variables introduced in a `for` or `iterate...over` statement. An object of this class encapsulates the type of that control variable. This type is `int` for control variables introduced in `for` statements and `neighbour` for control variables introduced in `iterate...over` statements.

* `Neighbour` used to label those identifiers that are declared to be neighbour symbols in a top-level `neighbourhood` declaration. An object of this class encapsulates a vector of coordinate values (constant integers) specifying the coordinates of that neighbour relative to the current cell.

* `Function` used to label those identifiers that are declared to be a function name in a top-level `function` declaration. These encapsulate a return type, a list of parameter types, and a reference to the defining `function` declaration node.

* `BuiltIn` used to label those identifiers that are reserved as the names of built-in functions in the Hipster prelude. These encapsulate a return type, a list of parameter types, and a `tainted` flag. This `tainted` flag is set if the built-in function in question should **not** be executed from within the `updater` or colour `mapper` functions. 

The Kiama library also pre-defines two basic error entities:

* `UnknownEntity` if we lookup an identifier in the environment and it isn't defined in there, then an object of this type is returned. 

* `MultipleEntity` if we add an identifier to the environment and it has already been defined in the current scope, then this entity is used to mark the re-defining instance of that identifier.

#### Tainted built-ins? ####

At the moment the only two tainted built-ins in Hipster are the random number generators `rnd` and `frnd`. 

By convention, the execution of a cellular automaton should be entirely _deterministic_, that is to say that the states of cells in the next generation should be completely determined only by the states of cells in the current generation. So whenever we execute the automaton from a fixed starting configuration we should always get the same result. 

All of the built-in functions, except for those that generate random numbers, are _pure_ in the sense that if you call them at two different times but pass them the same arguments on each occasion they will give the same results. So if you only use these pure functions in an `updater` function, then your cellular automaton will be guaranteed to behave deterministically. This wouldn't be the case, however, if we were able to use random numbers to determine the new states of cells.

On the other hand, random number generators are often useful in initialising a run of a cellular automaton. They allow us to set cells to randomised starting states and then see how the system evolves deterministically from that point.

#### Implementing name analysis ####

The name analyser in the Hipster compiler is largely implemented using three interrelated attributes of the Hipster program tree, whose definitions may be found in the `NameAnalysis.scala` module in the Hipster compiler source code tree:

* `envin`: an attribute defined on all nodes which holds the environment containing all bindings visible at each node in the program tree, not including any that are defined at that node.

* `envout`: an attribute defined on all nodes which holds the environment containing all bindings visible "after" a node in the program tree.  I.e., it's the environment at the node plus any new bindings introduced by the node.

* `entity`: an attribute which is only defined for identifier nodes, that is either identifier use nodes (of type `IdnUse`) or identifier definition nodes (of type `IdnDef`). This computes entities to associate with `IdnDef` nodes using information derived from the declaration context in which they occur. At `IdnUse` nodes it looks up and returns the entity for that identifier as stored in the environment held in the `envin` attribute of that node.

The `entity` attribute handles the detection of errors arising from undeclared or multiply declared identifiers. Specifically the following code from its definition

    val entity : IdnNode => Entity =
      attr {
        // At defining instances, if the identifier is already defined
        // in the current scope then attribute with the entity for
        // multiple definitions.
        case n @ IdnDef(i) if (isDefinedInScope(envin(n), i)) =>
          MultipleEntity()

        // At applied instances, if the identifier is defined in the
        // environment in at this node then return the corresponding
        // entity. Otherwise attribute with the unknown name entity.
        case n @ IdnUse (i) =>
          lookup(envin(n), i, UnknownEntity())

        // If the last case isn't triggered then we must be at an `IdnDef`
        // node, so use the enclosing context to work out the entity
        // associated with this defining instance:
    
        .... case clauses to analyse each kind of enclosing declaration.

      }
      
attributes a identifier node with the value:

* `MultipleEntity()` if it is a defining instance node whose identifier is already defined in the current scope, and
* `UnknownEntity()` if it is an applied instance node whose identifier is not in scope.

Later an error message is generated for each identifier node whose `entity` attribute is equal to one of these error entities. This is done by the following section of code, which is taken from the `SemanticAnalysis.scala`  module in the Hipster compiler source code tree:

    lazy val errors : Messages =
      collectMessages(tree) {

        // Errors related to defining and .

        case d @ IdnDef(i) if (entity(d) == MultipleEntity()) =>
          message (d, "'" + i + "' is declared more than once in current scope")

        case u @ IdnUse(i) if (entity(u) == UnknownEntity()) =>
              message (u, "'" + i + "' is not declared at this point")

        .... case clauses to handle all other kinds of errors.

    }
    
### Type analysis ###

The Hipster language doesn't provide any mechanisms for defining any new types and it only provides four basic builtin types:

* `int` the type of (32-bit) signed integers,
* `float` the type of double precision floating point numbers,
* `boolean` the type of boolean values (`true` or `false`), and
* `neighbour` the type of tokens naming neighbouring cells.

As is common in many languages, the `int` and `float` types are _compatible_, in the sense that a value of type `int` may be used wherever on of type `float` is expected (but not vice-versa). In general, we say that a type `type1` is compatible with a type `type2` if they are equal or if `type1` is `int` and `type2` is `float`.

Under the bonnet, the Hipster compiler will generate code to coerce `int` values to `float` values wherever necessary.

Any variable, constant or parameter must be declared to have one of these types. The type of any expression is determined by the following inference rules:

1. The type of an identifier expression is the declared type of that identifier. The type of an identifier expression referring to the control variable of a `for` loop is `int`. The type of an identifier expression referring to the control variable of an `iterate...over` loop is `neighbour`.

2. The type of a manifest integer constant is `int`, the type of a manifest floating point constant is `float` and the type of a boolean constant (`true` or `false`) is `boolean`.

3. The type of any neighbour constant declared in the top-level `neighbourhood` declaration is `neighbour`.

4. The operands of any arithmetic operator (`+`, `-`, `*`, `/`, `%` etc.) must be of type `int` or `float`. The rules for inferring the types of expressions built using arithmetic operators are:
     * for a unary arithmetic operator `aop` the type of an expression `aop expr` is the same as the type of the sub-expression `expr`, and 
     * for a binary arithmetic operator `aop` the type of an expression `expr1 aop expr2` is `int` if the types of `expr1` and `expr2` are both `int` and otherwise the type of that expression is `float`.
     
5. The operands of the equality operator `==` can be any of the four types and those types must either both be `boolean`, both be `neighbour`, or both be of one of the numeric types `int` or `float`. The type of an equality expression `expr1 == expr2` is `boolean`.

6. The operands of any other rational operator (`<`, `>`, `<=`, or `=>`) must be of type `boolean`, `int` or `float` and those types must either both be boolean, or both be one of the numeric types `int` or `float`. The type of a relational expression `expr1 rop expr2` is `boolean`.

7. The operands of any boolean operator (`||`, `&&`, or `!`) must be of type `boolean` and the type of a boolean expression `expr1 bop expr1` is `boolean`.

8. The type of a neighbour state expression `nbr:state` is equal to the type of the state field identifier `state`.

9. A function call expression that calls a user defined or built-in function is equal to the return type of the function being called.

The context in which an expression is used places certain _expectations_ on the type that it must have, according to the following rules:

1. The control expression in an `if` statement must be of type `boolean`.

2. The `from`, `to` and `step` expressions in a `for` loop statement must be of type `int`.

3. The constants in a subset expression of an `iterate...over` loop must be of type `neighbour`.

4. The ordinate expressions of a coordinate expression must be of type `int`.

5. The dimension expressions in a top-level `dimension` declaration must be of type `int`.

6. The left hand operand of a neighbour field expression `nbr:state` must be of type `neighbour`.

7. The type of an argument expression in a function call must be compatible with the declared type of its corresponding parameter.

8. The type of an expression on the right of the `=` symbol in an assignment, variable or constant declaration must be compatible with the type of the identifier (or neighbour state expression) on its left.

9. The type of an expression in a `return` statement in the body of a function must be compatible with the declared return type of that function.

10. An expression in a `return` statement in the body of the `mapper` block must be of type `int`.

11. A `return` statement in the body of an `updater` or `initialiser` block or in the body of a function without a return type cannot have a return expression.

12. The return type of function called in a function call _statement_ can be any type. The returned value is discarded.

#### Implementing type analysis ####

The Hipster type analyser implements type inference and type checking using two attributes:

* `tipe`: which attributes each expression node with the inferred type of that node computed according to the type inference rules outlined above.

* `expTipe`: which attributes each expression node with the set of types that it may legally assume. This is computed on the basis of the context in which the expression is located.

The definitions of these attributes may be found in the `TypeAnalysis.scala`  module in the Hipster compiler source code tree. A type error message is generated, by the error reporting code in the `SemanticAnalysis.scala` module, for any expression node whose inferred `tipe` is not a member of the set given by the `expTipe` attribute.

We might note that if the types given by the `tipe` attribute(s) of the operand children do not satisfy the rules given above for their operator parent then the `tipe` attribute of that parent node is set to the `unknown` type. Unknown types are propagated up the expression tree by letting the `tipe` attribute of a parent operator node be `unknown` whenever the `tipe` attribute of any of its children is `unknown`. We prevent the spurious over reporting of type errors by suppressing such errors for any expression node whose inferred type is `unknown`.

---
[Dominic Verity](http://orcid.org/0000-0002-4137-6982)  
Last modified: 11 September 2017  
[Copyright (c) 2017 by Dominic Verity. Macquarie University. All rights reserved.](http://mozilla.org/MPL/2.0/)

