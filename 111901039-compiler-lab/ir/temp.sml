signature TEMP =
  sig
     
     type temp        = int
     type label       = int
     val newtemp  : unit -> temp
     val newlabel : unit -> label
     val tempToString : temp -> string
     val labelToString : label -> string

  end

structure Temp :> TEMP = struct

   exception RegisterFailure
   type temp  = int
   type label = int

   val nextTemp  = ref 0
   val nextLabel = ref 0

   fun newtemp _ = ( if (!nextTemp >= 7) then (raise RegisterFailure; ()) else (nextTemp := !nextTemp + 1);  !nextTemp )
   fun newlabel _ = ( nextLabel := !nextLabel + 1; !nextLabel )

   fun tempToString t = "t" ^ Int.toString(!nextTemp)
   fun labelToString l = "l"^Int.toString l

end