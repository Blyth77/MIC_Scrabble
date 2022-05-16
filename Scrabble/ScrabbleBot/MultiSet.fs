// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>
    let empty =
        MS Map.empty
    let isEmpty (MS m) = Map.isEmpty m
    let size (MS ms) =
        Map.fold (fun state _ value -> state + value) 0u ms
    let contains key (MS m) =
        Map.containsKey key m
    
    let toMap (MS m) =
        m
    let numItems key (MS ms): uint32 =
      match Map.tryFind key ms with
            | None -> 0u
            | Some value -> value 
      
//let (.+.) a b = a + b
    let add key count (MS ms)  =
        match Map.tryFind key ms with
         | None -> Map.add key count ms
         | Some value ->  Map.add key (value + count) ms
        |> MS

    let addSingle key (MS ms) =
         match Map.tryFind key ms with
         | None -> Map.add key 1u ms
         | Some value ->  Map.add key (value + 1u) ms
        |> MS
        //add key 1u ms 

    let remove key count (MS ms) =
     match Map.tryFind key ms with
        | None -> ms
        | Some value -> 
                         if value > count then Map.add key (value - count) ms
                         else Map.remove key ms
     |> MS
    let removeSingle key ms =
     remove key 1u ms
    let fold f acc (MS values) =
        Map.fold f acc values
     
    let foldBack f (MS values) acc =
        Map.foldBack f values acc

    let ofList lst =  List.fold (fun acc key -> addSingle key acc) empty lst
    let toList (MS ms) = Map.toList ms
                         |> List.collect (fun (key, value) -> List.replicate (int value) key)
    let subtract ms1 ms2 =
        fold (fun acc key value -> remove key value acc) ms1 ms2
    
    