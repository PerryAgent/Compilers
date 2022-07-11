signature INST = sig
    type t   (* The type of the instruction *)
    val isJumpLike   : t -> bool
    val isTarget     : t -> bool
end

structure MIPSInst : INST = struct
    type t = (MIPS.label, MIPS.reg) MIPS.stmt

    fun isJumpLike (MIPS.Label(_)) = false
    |   isJumpLike (MIPS.Direc(_)) = false
    |   isJumpLike (MIPS.Inst(x)) = case x of
        MIPS.B(_)     => true 		           
        | MIPS.Bczt(_) 	=> true                           
        | MIPS.Bczf(_)  => true
        | MIPS.Beq(_, _, _)  => true	
        | MIPS.Beqz(_, _) => true
        | MIPS.Bge(_, _, _) => true
        | MIPS.Bgeu(_, _, _) => true 
        | MIPS.Bgez(_, _) => true
        | MIPS.Bgezal(_, _) => true
        | MIPS.Bgt(_, _, _) => true
        | MIPS.Bgtu(_, _, _) => true
        | MIPS.Bgtz(_, _) => true
        | MIPS.Ble(_, _, _) => true
        | MIPS.Bleu(_, _, _) => true
        | MIPS.Blez(_, _) => true
        | MIPS.Bltzal(_, _) => true
        | MIPS.Blt(_, _, _) => true
        | MIPS.Bltu(_, _, _) => true
        | MIPS.Bltz(_, _) => true
        | MIPS.Bne(_, _, _) => true
        | MIPS.Bnez(_, _) => true
        | MIPS.J(_) => true
        | MIPS.Jal(_) => true
        | MIPS.Jalr(_) => true
        | MIPS.Jr(_) => true
        | _ => false
 

    fun isTarget (MIPS.Label(_)) = true
    | isTarget _ = false
end



functor BasicBlocks (I : INST) = struct

    structure Inst = I                     (* expose the instruction module as well *)
    type block = I.t list

    fun basicBlocksHelper ([], curr, final) = final@[curr]
    | basicBlocksHelper (x::xs, curr, final) = case (I.isJumpLike(x), I.isTarget(x)) of
        (false, false) => basicBlocksHelper (xs, curr@[x], final)
        | (false, true) => basicBlocksHelper (xs, [x], final@[curr])
        | (true, false) => basicBlocksHelper (xs, [], final@[(curr@[x])])
        | (true, true) => basicBlocksHelper (xs, [], (final@[curr])@[[x]])

    
    fun basicBlocks i = basicBlocksHelper(i, [], [])

    fun printBlock [] = ""
    |  printBlock (x::xs) = (MP.mp x)^"\n"^(printBlock xs)

end

structure MIPSBasicBlocks = BasicBlocks (MIPSInst)