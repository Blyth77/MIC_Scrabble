module Dictionary

    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
    
    let empty () = Leaf false

    let rec insert (str : string) x =
        match x with  
        | Leaf true when String.length str = 1 -> Node (true, Map.add str[0] (Leaf true) Map.empty<char, Dict>)  
        | Leaf true -> Node (true, Map.add str[0] (insert str.[1..] (Leaf false)) Map.empty<char, Dict>)  
        | Leaf _ when String.length str = 1 -> Node (false, Map.add str[0] (Leaf true) Map.empty<char, Dict>)
        | Leaf _ when String.length str = 0 -> Leaf true
        | Leaf _ -> Node (false, Map.add str[0] (insert str.[1..] (Leaf false)) Map.empty<char, Dict>)
        | Node (b, m) when String.length str = 0 -> Node (true, m)
        | Node (b, m) when Map.containsKey str[0] m-> Node (b, Map.add str[0] (insert str.[1..] (Map.find str[0] m)) m)
        | Node (b, m) -> Node (b, Map.add str[0] (insert str.[1..] (Leaf false)) m) 

    let rec lookup (str : string) x =
        match x with
        | Leaf true when String.length str = 0 -> true
        | Node (b, _) when String.length str = 0 && b -> true
        | _ when String.length str = 0 -> false
        | Node (_, m) when Map.containsKey str[0] m -> lookup str.[1..] (Map.find str[0] m)
        | _ -> false

    let step c x = 
        match x with
        | Node (b, m) when Map.containsKey c m -> 
            match Map.find c m with
            | Node (b,m) -> Some(b, Node(b,m))
            | Leaf b -> Some(b, Leaf b)
        | Leaf b -> Some(b, empty ())
        | _ -> None 