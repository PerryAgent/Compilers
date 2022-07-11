# README
*Name: Parichita Das*

*Roll Number: 111901039*

## Organisation  of Code
1. **`src` Folder:** It contains the Lexer `tiger.lex`, Grammar `tiger.grm`, the Abstract `ast.sml`. Other than these three main files it also contains the `ec.sml` which makes use of the above three to get the set of instructions from the source code. We also have the `graph.sml` and `block.sml` which are independently used to convert given set of instructions into graph structure and basic blocks respectively.

2. **`ir` Folder:** It contains `ir.sml` and `temp.sml` which converts the source code instructions to intermediate stage and is responsoble for `Pretty Printing`.

3. **`target` Folder:** It contains `mips.sml` and `translate.sml` which compiles to give the actual `MIPS Assembly Code` with actual registers and labels. It also contains `mp.sml` that helps printing the `MIPS Instructions`.

## Running the Program
1. The `Makefile` has been created in the top directory which has all the functionalities required for smooth running of entire code. We also have `ec.mlb` that lists all the files used in required order.

2. Use the following instructions for compiling the program and running tests:

* `make all`: Use this instruction to compiler all the  required files.
* `make test`: Use this to compile all files and run the tests given in `test` folder. 
* `make clean`: Use this to clean all the compiled files.
* In order to run other test files simply run the following after `make all`:<br><br>
  `./ec < <test_inp>.in 2> <test_err>.err > <test_out>.out`
  <br>
  <br>

> The actual MIPS Assembly code can be found in the respective test*.out files and the Pretty Printing and Basic Block can be found in respective test*.err.
