structure IR : sig
  type inst = (MIPS.label, MIPS.reg) MIPS.inst
  type stmt = (MIPS.label, MIPS.reg) MIPS.stmt
  type prog = stmt list

  val ppInst : inst -> string
  val ppStmt : stmt -> string
  val pp     : prog -> string
end = struct
  (* complete this *)
  type inst =  (MIPS.label, MIPS.reg) MIPS.inst
  type stmt =  (MIPS.label, MIPS.reg) MIPS.stmt
  type prog = stmt list

  fun regToString MIPS.T0 = "t0"
    | regToString MIPS.T1 = "t1"
    | regToString MIPS.T2 = "t2"
    | regToString MIPS.T3 = "t3"
    | regToString MIPS.T4 = "t4"
    | regToString MIPS.T5 = "t5"
    | regToString MIPS.T6 = "t6"
    | regToString MIPS.T7 = "t7"
    | regToString MIPS.V0 = "t8"
    | regToString MIPS.A0 = "t9"
    | regToString (MIPS.Imm x) = (Int.toString x)

  fun ppInst (MIPS.Abs(r1,r2))            = "abs "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Add(r1,r2,r3))    = "add "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Addi(r1,r2,r3))   = "addi "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Addu(r1,r2,r3))   = "addu "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Addiu(r1,r2,r3))  = "addiu "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.And(r1,r2,r3))    = "and "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Andi(r1,r2,r3))   = "andi "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Div(r1,r2))       = "div "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Divu(r1,r2))      = "divu "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Div2(r1,r2,r3))   = "div "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Divu2(r1,r2,r3))  = "divu "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Mul(r1,r2,r3))    = "mul "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Mulo(r1,r2,r3))   = "mulo "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Mulou(r1,r2,r3))  = "mulou "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Mult(r1,r2))      = "mult "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Multu(r1,r2))     = "multu "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Neg(r1,r2))       = "neg "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Negu(r1,r2))      = "negu "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Nor(r1,r2,r3))    = "nor "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Not(r1,r2))       = "not "^(regToString r1)^", "^(regToString r2)
        |    ppInst (MIPS.Or(r1,r2,r3))     = "or "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Ori(r1,r2,r3))    = "ori "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Rem(r1,r2,r3))    = "rem "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Remu(r1,r2,r3))   = "remu "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Rol(r1,r2,r3))    = "rol "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Ror(r1,r2,r3))    = "ror "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Sll(r1,r2,r3))    = "sll "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Sllv(r1,r2,r3))   = "sllv "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Sra(r1,r2,r3))    = "sra "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Srav(r1,r2,r3))   = "srav "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Srl(r1,r2,r3))    = "srl "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Srlv(r1,r2,r3))   = "srlv "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Sub(r1,r2,r3))    = "sub "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Subu(r1,r2,r3))   = "subu "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Xor(r1,r2,r3))    = "xor "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)
        |    ppInst (MIPS.Xori(r1,r2,r3))   = "xori "^(regToString r1)^", "^(regToString r2)^", "^(regToString r3)

                            (* Constant-Manipulating Instructions *)
        |    ppInst (MIPS.Li(r1,r2))             = "li "^(regToString r1)^", "^(regToString r2)   
        |    ppInst (MIPS.Lui(r1,r2))            = "lui "^(regToString r1)^", "^(regToString r2) 

                            (* Comparison Instructions *)
        |    ppInst (MIPS.Seq(r1, r2, r3)) 		= "seq "  ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)      
        |    ppInst (MIPS.Sge(r1, r2, r3))		= "sge "  ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sgeu(r1, r2, r3))		= "sgeu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sgt(r1, r2, r3))		= "sgt "  ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sgtu(r1, r2, r3)) 		= "sgtu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sle(r1, r2, r3)) 		= "sle "  ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sleu(r1, r2, r3)) 		= "sleu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Slt(r1, r2, r3)) 		= "slt "  ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Slti(r1, r2, r3)) 		= "slti " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sltu(r1, r2, r3)) 		= "sltu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sltiu(r1, r2, r3)) 	= "sltui " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)
        |    ppInst (MIPS.Sne(r1, r2, r3)) 		= "sne " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (regToString r3)

                           (* Branch and Jump Instructions *)
        |    ppInst (MIPS.B(l1)) 				= "b " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bczt(l1)) 				= "bczt " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bczf(l1)) 				= "bczf " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Beq(r1, r2, l1)) 		= "beq " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Beqz(r1, l1)) 			= "beqz " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bge(r1, r2, l1)) 		= "bge " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bgeu(r1, r2, l1)) 	    = "bgeu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bgez(r1, l1)) 			= "bgez " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bgezal(r1, l1)) 		= "bgezal " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bgt(r1, r2, l1)) 		= "bgt " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bgtu(r1, r2, l1)) 		= "bgtu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bgtz(r1, l1)) 			= "bgtz " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Ble(r1, r2, l1)) 		= "ble " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bleu(r1, r2, l1)) 	    = "bleu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Blez(r1, l1)) 			= "blez " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bltzal(r1, l1)) 		= "bltzal " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Blt(r1, r2, l1)) 		= "blt " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bltu(r1,r2, l1))       = "bltu " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bltz(r1, l1)) 			= "bltz " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bne(r1, r2, l1)) 		= "bne " ^ (regToString r1) ^ ", " ^ (regToString r2) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Bnez(r1, l1)) 			= "bnez " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.J(l1)) 				= "j " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Jal    (l1)) 			= "jal " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Jalr   (r1)) 			= "jalr " ^ (regToString r1)
        |    ppInst (MIPS.Jr    (r1)) 			= "jr " ^ (regToString r1)

                            (* Load Instructions *)
        |    ppInst (MIPS.La    (r1, l1))		= "la " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lb    (r1, l1))		= "lb " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lbu    (r1, l1))		= "lbu " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Ld    (r1, l1))		= "ld " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lh    (r1, l1))		= "lh " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lhu    (r1, l1))		= "lhu " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lw    (r1, l1))		= "lw " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lwcz    (r1, l1))		= "lwcz " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lwl    (r1, l1))		= "lwl " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Lwr    (r1, l1))		= "lwr " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)

                            (* Store Instructions *)
        |    ppInst (MIPS.Sb    (r1, l1))		= "sb " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Sd    (r1, l1))		= "sd " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Sh    (r1, l1))		= "sh " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Sw    (r1, l1))		= "sw " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Swcz    (r1, l1))		= "swcz " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Swl    (r1, l1))		= "swl " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Swr    (r1, l1))		= "swr " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Ulh    (r1, l1))		= "ulh " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Ulhu   (r1, l1))		= "ulhu " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Ulw    (r1, l1))		= "ulw " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Ush    (r1, l1))		= "ush " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1)
        |    ppInst (MIPS.Usw    (r1, l1))		= "usw " ^ (regToString r1) ^ ", " ^ (MIPS.printlabel l1) 

                            (* Trap and Exception Instructions *)  
        |    ppInst (MIPS.Rfe)                   = "rfe"
        |    ppInst (MIPS.Syscall)               = "syscall"
        |    ppInst (MIPS.Break(n))              = "break "^Int.toString n
        |    ppInst (MIPS.Nop)                   = "nop"  

                             (* Data Movement Instructions *)
        |    ppInst (MIPS.Move (r1, r2))            = "move "^(regToString r1)^" "^(regToString r2)
        |    ppInst (MIPS.Mfhi (l1))                = "mfhi "^ (regToString l1)
        |    ppInst (MIPS.Mflo (l1))                = "mflo "^ (regToString l1)
        |    ppInst (MIPS.Mthi (l1))                = "mthi "^ (regToString l1)
        |    ppInst (MIPS.Mtlo (l1))                = "mtlo "^ (regToString l1)
        |    ppInst (MIPS.Mfcz (l1, l2))            = "mfcz "^ (regToString l1)^", "^(regToString l2)
        |    ppInst (MIPS.Mtcz (l1, l2))            = "mtcz "^ (regToString l1)^", "^(regToString l2)


    fun ppDirec (MIPS.align (n)) =   ".align "^Int.toString n
    | ppDirec (MIPS.ascii (n))   =   ".ascii "^n
    | ppDirec (MIPS.asciiz(n))   =   ".asciiz "^n
    | ppDirec (MIPS.data(n))     =   ".data "^n
    | ppDirec (MIPS.extern(s,n)) =   ".extern "^s^" "^(Int.toString n)
    | ppDirec (MIPS.globl(s))    =   ".globl "^s
    | ppDirec (MIPS.kdata(s))    =   ".kdata "^s
    | ppDirec (MIPS.ktext(s))    =   ".ktext "^s
    | ppDirec (MIPS.space(n))    =   ".space "^Int.toString n
    | ppDirec (MIPS.text(s))     =   ".text "^s

    fun ppStmt (MIPS.Inst(i))   = ppInst i
    |   ppStmt (MIPS.Direc(d))  = ppDirec d
    |   ppStmt (MIPS.Label (l)) = (MIPS.printlabel l) ^ ":"

    fun pp [] = ""
	|   pp (x::xs) = (ppStmt x)^"\n"^(pp xs)

end