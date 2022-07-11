# SUMMARY

This summary gives a general idea on the entire `Compilers Lab` Project done thoughout the entire semester. The below three subsections give the summarized versions of how our compiler handles `Parsing`, `IR code`, and `MIPS assembly code`

## Parsing
1. The main two files for this are `src/tiger.grm` and `src/tiger.lex` which contains the grammar and the lexer. 
   
2. They can recognize the basic arithmetic operations with brackets, assignment operation, print operation, end of file, variables, newline, semicolon and for loops.

3. They make use of the `ast.sml` to map the operations used and prepare them for the next stage.

## IR Code Generation
1. We now use the `src/ast.sml` to get the `MIPS Instructions`.

2. All MIPS Instructions can be found in `target/MIPS.sml`

3. Now the job of `ir/ir.sml` is to convert these instructions to a `Pretty Print` format. We do so by using registers, but right now we do not need to worry about register allocation.

4. To achieve this we use `ir/temp.sml` which generates registers and labels one by one in string format to be printed.

## MIPS Assembly Code Generation
1. This is where we finally do the translation. This file can be found in the `target/translate.sml`. We convert the temp registers allocated to actual `MIPS registers`.

2. We use `AtomMap` to store the different variables used as keys and store their respective values.

3. We achieve this by making use of the registers we get and store intermediate or final values in these actual registers.

4. All the operations such as `assign`, `print`, `for` are handled separately and registers are allocated accordingly.

5. Finally `compile` function take the entire program and as and as we add the variables to the map, we keep updating the environment as well. 

6. In `src/ec.sml` we use this function to give our program which is a `list of stmt` and then print this using `target/mp.sml`

7. We also use `src/block.sml` to convert these assembly code into blocks of instructions such that they are separated by `jump instructions` and `labels`.

## Miscellaneous
1. We have also made `src/graph.sml` that creates a graph structure by making `nodes` of any type ane connecting them with edges. These edges signify the control flow of the program. 

2. We are yet to implement the control flow part and register allocation through this `graph.sml`. 

3. I have yet not implemented the canonical form of IR printing i.e., `Tree IR`
<br><br>
> Please refer to the `README` to see how to run the code.<br>
Please refer to the `CHANGELOG` to see weekly progress of the codes.






