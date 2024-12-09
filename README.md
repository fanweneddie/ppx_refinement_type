CS560 project: refinement type

# Installation
We use OCaml compiler version 4.14.2
To install the compiler:

```
opam switch create 4.14.2 ocaml-base-compiler.4.14.2
opam switch set 4.14.2
eval $(opam env)
```

Next, the libraries need to be installed 
```
opam install dune
opam install ppxlib
opam install ocaml-compiler-libs
opam install ocamlfind
opam install z3
```

Then run
```
dune build
```

This will type check the program located in `test/rewriter/frontend.ml`.

# Testing

In test/testcases, we have a couple of test files (e.g., test_func_1.ml) that end with "(\* pass \*)" or "(\* fail \*)". If it ends with "(\* fail \*)", then we check whether "Fatal error: exception Failure" is in the output&error when running `dune build`. 

For example, test_func_1.ml should pass the type check.

``` OCaml
(* zero *)
let[@rty] zero = (v = 0 : int);;

let zero = 0;;

zero;;
(* pass *)
```

The script test.py in root directory does this checking by copying the test files into test/rewriter/frontend.ml and checking the output&error in the run.
