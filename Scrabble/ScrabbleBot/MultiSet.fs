// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>
    let empty =
        MS Map.empty
    let isEmpty (MS m) = Map.isEmpty m
    let size(MS ms) =
        Map.fold (fun state key value -> state + value) 0u ms
    let contains key (MS m) =
        Map.containsKey key m
    
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

    let addSingle key ms =
        add key 1u ms 

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
