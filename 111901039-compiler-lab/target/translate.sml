structure Translate = struct

fun   tempToReg 0 = MIPS.T0
    | tempToReg 1 = MIPS.T1
    | tempToReg 2 = MIPS.T2
    | tempToReg 3 = MIPS.T3
    | tempToReg 4 = MIPS.T4
    | tempToReg 5 = MIPS.T5
    | tempToReg 6 = MIPS.T6
    | tempToReg 7 = MIPS.T7
    | tempToReg 8 = MIPS.V0
    | tempToReg 9 = MIPS.A0
    | tempToReg _ = MIPS.Imm(1)

fun tempToLabel x = MIPS.TempLabel(x)

  
fun varHandler(x, env, t) = let 
                            val res =  case (AtomMap.find(env, Atom.atom(x))) of
                                 (SOME (v)) =>  ([MIPS.Inst(MIPS.Move(tempToReg(t), tempToReg(v)))], env)
                                | NONE      =>  let    
                                                val r1 = Temp.newtemp()
                                                val newenv =  AtomMap.insert(env, Atom.atom(x), r1)
                                                in
                                                    ([MIPS.Inst(MIPS.Move(tempToReg(t), tempToReg(r1)))], newenv)
                                                end  
                            in 
                                res 
                            end
                                    
fun compileExpr env e t = case e of
      Ast.Const x       		=>  ([MIPS.Inst(MIPS.Li(tempToReg(t),MIPS.Imm(x)))], env)
    | Ast.Op (x ,opr , y)   =>  let 
    val r1 = Temp.newtemp()
    val r2 = Temp.newtemp()
    val (l, new_envl) = compileExpr env x r1
    val (r, new_envr) = compileExpr new_envl y r2
    in
        case opr of
          Ast.Plus    =>  (l@r@[MIPS.Inst(MIPS.Add(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_envr)
        | Ast.Minus   =>  (l@r@[MIPS.Inst(MIPS.Sub(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_envr)
        | Ast.Mul     =>  (l@r@[MIPS.Inst(MIPS.Mul(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_envr)
        | Ast.Div     =>  (l@r@[MIPS.Inst(MIPS.Div2(tempToReg(t), tempToReg(r1), tempToReg(r2)))], new_envr)
    end
    | Ast.Var x         		=>  (varHandler(x, env, t))
   
fun assignHandler(x, y, env) =   let 
                                 val res1 =  case  (AtomMap.find(env, Atom.atom(x))) of
                                 (SOME (v)) =>  let 
                                                val (ans, newenv) = compileExpr env y v
                                                in (ans, newenv) end 
                                 | NONE   =>    let
                                                val newvar = Temp.newtemp()
                                                val newenv = AtomMap.insert(env, Atom.atom(x), newvar)
                                                val (res, new_newenv) = compileExpr newenv y newvar
                                                in 
                                                    (res, new_newenv)
                                                end
                                  in 
                                      res1
                                  end

fun printHandler(x, env) = 
let 
val newvar = Temp.newtemp()
val (res, newenv) = compileExpr env x newvar
val new_newenv    = AtomMap.insert(env, Atom.atom("A0"), newvar)
in 
(res@[MIPS.Inst(MIPS.Move(MIPS.A0, tempToReg(newvar)))]@[MIPS.Inst(MIPS.Li(MIPS.V0, MIPS.Imm(1)))]@[MIPS.Inst(MIPS.Syscall)] , new_newenv)
end

fun compileStmt env s = case s of
      Ast.Assign (x, y) => assignHandler(x, y, env)
    | Ast.Print x => printHandler(x, env)
    | Ast.For (v,c1,c2,stmtList) =>  
    let
    val loopReg = Temp.newtemp()
    val loopEnv = AtomMap.insert(env,Atom.atom(v),loopReg)
    val breakReg = Temp.newtemp()
    val loopLabel = Temp.newlabel()
    val breakLabel = Temp.newlabel()
    fun loopCompile e [] = []
    | loopCompile e (x::xs) = let
                                    val (res,env1) = compileStmt e x
                                in
                                    res@loopCompile env1 xs
                                end
    val loopStmts = loopCompile loopEnv stmtList
    in
        ([MIPS.Inst(MIPS.Li(tempToReg(loopReg),MIPS.Imm(c1))), MIPS.Inst(MIPS.Li(tempToReg(breakReg),MIPS.Imm(c2))),
        MIPS.Label(tempToLabel(loopLabel)),
        MIPS.Inst(MIPS.Bgt(tempToReg(loopReg), tempToReg(breakReg), tempToLabel(breakLabel)))]
        @loopStmts
        @[MIPS.Inst(MIPS.Addi(tempToReg(loopReg),tempToReg(loopReg),MIPS.Imm(1))),
        MIPS.Inst(MIPS.J(tempToLabel(loopLabel))),
        MIPS.Label(tempToLabel(breakLabel))], env)
    end

fun compileprog env []          = [MIPS.Inst(MIPS.Li(MIPS.V0, MIPS.Imm(10)))]@[MIPS.Inst(MIPS.Syscall)]
    | compileprog env (x :: xs)   = let 
                                    val (res, newenv) = compileStmt env x  
                                in 
                                    res@compileprog newenv xs 
                                end
    

fun compile prog = [MIPS.Direc(MIPS.globl("main")), MIPS.Label(MIPS.UserDefined("main"))]@compileprog AtomMap.empty prog

end