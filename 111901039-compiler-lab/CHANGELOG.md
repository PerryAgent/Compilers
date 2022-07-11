# CHANGELOG

A weekly entry on what was done.

### Week 1
Creation of the repository and hosting it on gitlab. 

### Week 2
1. To incorporate '/' in reverse polish compiler we make changes in `ast.sml` file, `expr.grm` for the grammar and `rp.lex` for the parsing. We also make changes in the `expr.lex` so that we can convert the expression using division as well. It may be noted that '/' requires both *concrete* and *abstract* changes.

2. To incorporate '(' and ')' in expressin converter, we make changes in `expr.grm` for the grammar and in the `expr.lex` for the parsing and lexing. Brackets requires only *concrete* changes.

### Week 3
1. We need to capture the datatypes `reg`, `Label`, `inst`, `direc` and `stmt` which are required in MIPS. 

2. We need to be able to convert the instructions and the statements to MIPS assembly language format. In order to do so we make different functions to convert all the given datatypes to strings and then finally join them in `prtStmt`.

### Week 4
1. Made `ast.sml`, `tiger.grm`, `tiger.lex` for the src folder. They contain abstract, grammaar and lexer for a subset of tiger.

2. Made `temp.sml` and `translate.sml` for target folders. Temp is a structure to represent registers. Translate compiler Tiger into MIPS language format.

### Week 5
1. Made some changes to previous week lab in order to debug the code. I kindly request sir to use this week code for last week evaluation.

2. In this week, we added for as a new statement type with string as the variable, two integers as the starting and ending constants, and a list of statements.

3. We make changes to `lex` and `grm` to incorporate for loop statement. We also add functionality to add new label to `temp.sml`.

4. We now make changes to translate. We must be able to compile the for loop statement itself, alongwith the list of statements inside. We should also be able to move to the for loop label during iteration, and break label when loop breaks.

5. We have added `ec.sml` and `test.in` to run the compiler

6. We have also added `Makefile` to easily make all the required files.

### Week 6
1. Wrote `block.sml` to incorporate block features which takes care of jumplike and target instructions.

2. This file contains signature and structure for `MIPSInst` which helps us determine whether an instruction is jumplike or taget or not.

3. We also made a functor to make the block which is basically a list of statements (program). We read all instrcutions from left to right and handle them appropriately.

4. We finally made a `printBlock` function to display the instructions as a list of blocks.

5. We also added `ec.sml` and `ec.mlb` to enable use of `block.sml`. We also make changes to the `Makefile`.

### Week 7
1. For `Graph.sml`, incorporated all the required functions like `newNode()`, `addEdge()` etc.

2. Created a functor with the signature GRAPH. Created two structures under the same and added nodes and edges of two different types to it.

3. Added the sml to Makefile.

