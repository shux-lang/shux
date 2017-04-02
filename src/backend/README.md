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

I propose the following sequence of steps:

_[SEM] denotes semantic check_
_[TR x -> y] denotes translation from AST x to AST y_
_AST denotes the shux AST_
_CAST denotes a C-like AST_
_LLAST denotes an LLVM-like AST_

1. [SEM] Scope checker
2. [TR AST -> AST] Flatten namespaces; this should leave `ns_decls` empty
3. [TR AST -> AST] Manual creation of unit generator, named `_`
3. [SEM] `main()` check (it should exist in the global namespcae as `_main()`)
4. [SEM] Type check
5. [TR AST -> CAST] Implement/de-abstractify:
	* generators, which will require the following:
		* definition of a struct that encompasses the state of a generator
		* definition of a (stateless) function that takes in such a struct
	* declarations: hoisted out of mixed statements
	* lambdas: these will be hoisted as special case of declarations
	* functional generator operations/loops (`for` and `do`):
		* evaluate expr to figure out number of iterations
		* initialise and manage the state struct of the generator
		* iterate through, passing on each/final yielded value
	* functional kernel operators/composers (`@`/map and `::`/filter):
		* go through each element and perform the kernel on it
		* accumulate another list to return
	* arrays: will become generators that produce those values if chained with kernel operators
	* vectors: implemented as float arrays 
	because `vector` actually means something else in LLVM land
6. [TR CAST -> LLAST] Unwrap 
	* unfold chained expressions into a flat series of statements
	* unfold ternery conditional to emulate if/then/else
	* any other things that make's codegen not look like Medusa on meth
