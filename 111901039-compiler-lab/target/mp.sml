structure MP : sig

  type stmt = (MIPS.label, MIPS.reg) MIPS.stmt
  type prog = stmt list

  val mp     : prog -> string
end = struct

  type inst = (string, Temp.temp) MIPS.inst
  type stmt = (MIPS.label, MIPS.reg) MIPS.stmt
  type prog = stmt list

  fun mp [] = ""
	|   mp (x::xs) = (MIPS.prStmt x)^"\n"^(mp xs)
end
