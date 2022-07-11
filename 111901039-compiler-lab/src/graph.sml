exception NotFound

signature GRAPH = sig

type node
type 'a graph

(*
   Create a new node in the graph
   This operation is not a pure function.
*)
val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

(* If the data structure was supposed to be persistent the definition
   of new will not be of this form

   addNode : graph -> graph * node
   addEdge : graph -> node * node -> graph
*)


val addEdge : 'a graph -> (node * node) -> unit

(* addEdge (a,b) should add and edge starting from a to b *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list

(* you might want functions that go over all the nodes

maps, folds etc
*)

end

structure Graph :> GRAPH = struct
    type node = word

    structure NodeHashKey : HASH_KEY = struct
        type hash_key = node    
        fun  hashVal w = w
        fun  sameKey (w1,w2) = w1 = w2
    end

    structure NodeSet = HashSetFn (NodeHashKey)

    type nodeSet = NodeSet.set

    type 'a graph = { label : (node, 'a)  HashTable.hash_table,
        (* edges *)
        successors   : (node, nodeSet) HashTable.hash_table,
        predecessors : (node, nodeSet) HashTable.hash_table,
        nextNode : node ref
            }

    fun empty () = { label = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, NotFound),
                    successors = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, NotFound),
                    predecessors = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, NotFound),
                    nextNode   = ref (Word.fromInt 0)
        }

    fun newNode (g: 'a graph) a = let
                        val nn = !(#nextNode g)
                        val _  = HashTable.insert (#label g) (nn, a)
                        val _  = HashTable.insert (#successors g) (nn, NodeSet.mkEmpty 0)
                        val _  = HashTable.insert (#predecessors g) (nn, NodeSet.mkEmpty 0)
                        val _ = (#nextNode g) := nn + (Word.fromInt 1)

                        in 
                            nn
                        end
        
    fun addEdge (g: 'a graph) (a1, a2) = let 
                                val _ = NodeSet.add ((HashTable.lookup (#successors g) a1), a2)
                                val _ =  NodeSet.add((HashTable.lookup (#predecessors g) a2), a1)
                                in
                                    ()
                                end

    fun succ (g: 'a graph) a = NodeSet.listItems (HashTable.lookup (#successors g) a)

    fun pred (g: 'a graph) a = NodeSet.listItems (HashTable.lookup (#predecessors g) a)

    fun label (g: 'a graph) a = HashTable.lookup (#label g) a

    fun clear (g: 'a graph) = let
                                val _ = HashTable.clear (#label g)
                                val _ = HashTable.clear (#successors g)
                                val _ = HashTable.clear (#predecessors g)
                                val _ = (#nextNode g) := (Word.fromInt 0)
                                in
                                    ()  
                                end

    fun all (g: 'a graph) = let
                                val x = HashTable.listItemsi (#label g)
                                fun f [] = []
                                |   f ((a,b)::xs) = (a)::(f xs)
                                in
                                    f x
                                end 


end

functor MkGraph () :> GRAPH = struct 
    type node = word

    structure NodeHashKey : HASH_KEY = struct
        type hash_key = node    
        fun  hashVal w = w
        fun  sameKey (w1,w2) = w1 = w2
    end

    structure NodeSet = HashSetFn (NodeHashKey)

    type nodeSet = NodeSet.set

    type 'a graph = { label : (node, 'a)  HashTable.hash_table,
        (* edges *)
        successors   : (node, nodeSet) HashTable.hash_table,
        predecessors : (node, nodeSet) HashTable.hash_table,
        nextNode : node ref
            }

    fun empty () = { label = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, NotFound),
                    successors = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, NotFound),
                    predecessors = HashTable.mkTable (NodeHashKey.hashVal, NodeHashKey.sameKey) (0, NotFound),
                    nextNode   = ref (Word.fromInt 0)
        }

    fun newNode (g: 'a graph) a = let
                        val nn = !(#nextNode g)
                        val _  = HashTable.insert (#label g) (nn, a)
                        val _  = HashTable.insert (#successors g) (nn, NodeSet.mkEmpty 0)
                        val _  = HashTable.insert (#predecessors g) (nn, NodeSet.mkEmpty 0)
                        val _ = (#nextNode g) := nn + (Word.fromInt 1)

                        in 
                            nn
                        end
        
    fun addEdge (g: 'a graph) (a1, a2) = let 
                                val _ = NodeSet.add ((HashTable.lookup (#successors g) a1), a2)
                                val _ =  NodeSet.add((HashTable.lookup (#predecessors g) a2), a1)
                                in
                                    ()
                                end

    fun succ (g: 'a graph) a = NodeSet.listItems (HashTable.lookup (#successors g) a)

    fun pred (g: 'a graph) a = NodeSet.listItems (HashTable.lookup (#predecessors g) a)

    fun label (g: 'a graph) a = HashTable.lookup (#label g) a

    fun clear (g: 'a graph) = let
                                val _ = HashTable.clear (#label g)
                                val _ = HashTable.clear (#successors g)
                                val _ = HashTable.clear (#predecessors g)
                                val _ = (#nextNode g) := (Word.fromInt 0)
                                in
                                    ()  
                                end

    fun all (g: 'a graph) = let
                                val x  = HashTable.listItemsi (#label g)
                                fun f [] = []
                                |   f ((a,b)::xs) = (a)::(f xs)
                                in
                                    f x
                                end 


end

structure A = MkGraph () (* For application A in your program *)
val g1 = A.empty() : int A.graph
val n11 = A.newNode g1 0
val n12 = A.newNode g1 1
val n13 = A.newNode g1 2
val n14 = A.newNode g1 3
val n15 = A.newNode g1 4

val _ = A.addEdge g1 (n11, n12)
val _ = A.addEdge g1 (n13, n12)
val _ = A.addEdge g1 (n11, n14)

val l1 = A.all g1
val s1 = A.succ g1 n11
val p1 = A.pred g1 n12
val la1 = A.label g1 n13


structure B = MkGraph () (* For application B in your program *)
val g2 = B.empty() : string B.graph
val n21 = B.newNode g2 "a"
val n22 = B.newNode g2 "b"
val n23 = B.newNode g2 "c"

val _ = B.addEdge g2 (n21, n22)
val _ = B.addEdge g2 (n23, n22)

val l2 = B.all g2
val s2 = B.succ g2 n21
val p2 = B.pred g2 n22
val la2 = B.label g2 n23


