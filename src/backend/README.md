Backend Workflow
================
The Situation
-------------

Frontend will deliver us an AST. 
Although it'll be structurally sound, it may be broken for the following reasons:

* Scope errors
* Type errors

Meanwhile, its structure is only compatible with our particular semantics,
so we'll want to do some gradual translations from AST to AST
to make it easier once we hit the codegen stage.

---
The Gameplan
------------

I propose the following sequence of steps
(delimited by the intermediate tree structures that represent layers of abstraction):

#### AST (shux Abstract Syntax Tree)

* scope checking
	* namespace flattening
	* hoise declarations
* type checking/expose types
	* insert explicit Void type
	* vectors -> float arrays

#### SAST (shux Semantically-checked Abstract Syntax Tree)

* defunctionalisation
	* hoist lambdas with access links
	* implement maps and filters iteratively
* lookback
	* state extracted from generators to yield pure functions
	* lookback operation implemented
	* for/do -> C-style loops with blocks of statements

#### CAST (C-like Abstract Syntax Tree)

* control flow
	* conditionals replaced with conditional jumps
	* loops replaced with conditional jumps
* variables
	* explicitly allocate stack variables
	* struct access via indexing
* expressions
	* unrolling expessions

#### LLAST (Lower Level Abstract Syntax Tree)

* dump into OCaml LLVM binding
