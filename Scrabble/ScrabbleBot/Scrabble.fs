namespace MIC

open System.Collections.Generic
open System.Text.RegularExpressions
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open MultiSet
open System.IO
open ScrabbleUtil.DebugPrint
module RegEx =
    open System.Text.RegularExpressions
    open Parser
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        players       : uint32 list
        playerNumber  : uint32
        playerTurn    : uint32
        tiles         : Map<uint32, tile>
        squares       : Map<coord, uint32 * (char * int)>
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pl pn turn sq h ts = {
                            board = b
                            dict = d
                            players = pl
                            playerNumber = pn
                            playerTurn = turn
                            squares = sq
                            hand = h
                            tiles = ts
                             }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let players st       = st.players
    let playerTurn st    = st.playerTurn
    let squares st       = st.squares
    let tiles st         = st.tiles
    
    let updateHand (hand: MultiSet<uint32>) (moves: MultiSet<uint32>) (newTiles: list<uint32 * uint32>) =
        let newHand   = subtract hand moves
        List.fold (fun acc (tileId, num) -> add tileId num acc) newHand newTiles
        
    let updateBoard (moves: list<coord * (uint32 * (char * int))>) (prevBoard: Map<coord, uint32 * (char * int)>) =
        List.fold (fun acc m -> Map.add (fst m) (snd m) acc) prevBoard moves
   

module Move =
    type availableSpace = (uint32 * uint32)
    
    let allPermutations lst : string list =
        let rec comb accLst elemLst =
            match elemLst with
            | h::t when Seq.length h = 1 ->
                let character : char = Seq.head h |> (fun (a,_) -> a)
                let next = [character]::List.map (fun el -> character::el) accLst @ accLst
                comb next t
            | _::t ->
                comb accLst t
            | _ -> accLst
        let charlists = comb [] lst
        List.map (fun (a : char list) -> System.String.Concat(Array.ofList(a))) charlists
        
    let allCombinations lst : string list =
        let rec aux l acc counter =
            match l with
            | _ when counter = List.length l -> acc
            | h :: lst -> aux (List.append lst [h] ) (List.append acc (allPermutations (h :: lst))) (counter + 1)
            | [] -> acc
        aux lst List.empty 0 
    let findIdAndPoint s pieces =
        
        let piece = List.filter (fun (a, b) -> (fun (c, d) -> c = s) (Seq.head b)) pieces |> List.head
        (fun (x,y) -> (fun (z,p) -> (x,p)) (Seq.head y)) piece
        
    let stringToMove (s : string) (state : State.state) coord (space : (int * bool)) =
        let toList = (Map.toList state.tiles)
        let piecesList =List.filter (fun (a,_) -> a <> 0u) (List.map (fun (l, s : tile) -> (l, List.head (Set.toList s))) toList)
        let rec aux (ycoord : int) str =
            match str with
            | a::t ->
                let mutable x = 0
                let mutable y = 0
                if (fun (_, o) -> o) space then
                    x <- ((fun (x, _) -> x) coord)
                    y <- ((fun (_, y) -> y) coord) + (ycoord + 1)                    
                else
                    x <- ((fun (x, _) -> x) coord) + (ycoord + 1)
                    y <- ((fun (_, y) -> y) coord)
                let id = (fun (x,_) -> x) (List.head (List.filter (fun (_,(x,_)) -> x = a) piecesList))
                let char = a
                let points = (fun (_,(_,y)) -> y) (List.head (List.filter (fun (_,(x,_)) -> x = a) piecesList))
                let part = sprintf "%d %d %d%A%d" x y id char points
                let strip chars = String.collect (fun c -> if Seq.exists((=)c) chars then "" else string c)
                let teil = strip "'" part
                let obj = strip "\"" teil
                obj :: aux (ycoord + 1) t
            | _ -> []
        let slist = aux 0 (Seq.toList s)
        String.concat " " slist
        
    let coordToSquarefun (state : State.state) c =
        match state.board.squares c with
        | StateMonad.Success x ->
            match x with
            | Some p -> p
            | None -> failwith "None"
        | StateMonad.Failure _ -> failwith "Failure"
        
    let toWord (m : ((int * int) * (uint32 * (char * int))) list) : Eval.word =
        List.map (fun (c, (x, y)) -> y) m
        
    let getSinglePoint (p : Parser.square) t (w : Eval.word) =
        let item = Map.find 0 p
        match item w t 0 with
            | StateMonad.Success x -> x
            | StateMonad.Failure y -> 0
    
    let getWholePoint (p : Parser.square) t (w : Eval.word) =
        let item = Map.find 1 p
        match item w t 0 with
            | StateMonad.Success x -> x
            | StateMonad.Failure _ -> 0
    
    let calculatePoints (m : ((int * int) * (uint32 * (char * int))) list) state  =
        let word = toWord m
        let numberList = List.map (fun (c, (_, (_,p))) -> (getSinglePoint (coordToSquarefun state c) p word)) m
        List.fold (fun a b -> a + b) 0 numberList
    
    let wordToString (word : Eval.word) =
            let temp = List.map (fun (x, _) -> x) word
            System.String.Concat(Array.ofList(temp))
            
    let rec flatten acc =
        function
        | (h,v)::t when h = 1u -> flatten (v :: acc) t 
        | (h,v)::t when h > 1u -> flatten (v :: acc) ((h-1u, v) :: t)
        | _ -> acc
            
    let findWordCombination (state : State.state) (avSpace : availableSpace) dict coord =
        let characters = flatten [] (List.map (fun (s,p) -> (p, Map.find s state.tiles)) (Map.toList (toMap state.hand)))
        let combinations = allCombinations characters                
        let space = (fun (x,y) -> if x > y then (x, true) else (y, false)) avSpace
        let allPossible = List.filter (fun m -> Dictionary.lookup m dict) combinations |> List.filter (fun m -> m.Length <= (fun (x,_) -> x) ((fun (x,y) -> (int x,y)) space))        
        let allPossibleWords = List.map (fun x -> stringToMove x state coord ((fun (x,y) -> (int x,y)) space)) allPossible
        let parsed = List.map RegEx.parseMove allPossibleWords
        if (List.length parsed = 0) then List.empty else List.map (fun a -> (a, calculatePoints a state)) parsed

    let checkSquareFree (coord: coord) (st : State.state) =
        match Map.tryFind coord st.squares with
        | Some _ -> false
        | None  -> true                

    let rec countSpacesDown count (x, y) (st: State.state) =
            match ((size st.hand) > count) with
            | false -> count
            | true ->
                match (checkSquareFree (x, y + 1) st)
                    && (checkSquareFree (x + 1, y + 1) st)
                    && (checkSquareFree (x - 1, y + 1) st)
                    && (checkSquareFree (x, y + 2) st) with
                | true when count = 0u -> if (checkSquareFree (x, y - 1) st) then countSpacesDown (count + 1u) (x, y + 1) st else count
                | true -> countSpacesDown (count + 1u) (x, y + 1) st
                | false -> count
            
    let rec countSpacesRight count (x, y) (st: State.state) =
            match ((size st.hand) > count) with
            | false -> count
            | true ->
                match (checkSquareFree  (x + 1, y) st) 
                        && (checkSquareFree (x + 1, y + 1) st)
                        && (checkSquareFree (x + 1, y - 1) st)
                        && (checkSquareFree  (x + 2, y) st) with
                        | true when count = 0u -> if (checkSquareFree (x - 1, y) st) then countSpacesRight (count + 1u) (x + 1, y) st else count
                        | true -> countSpacesRight (count + 1u) (x + 1, y) st
                        | false -> count
                    
    let calculateAvailableSpace (randomTile: coord * (uint32 * (char * int))) (st: State.state) =
       (countSpacesDown 0u (fst randomTile) st, countSpacesRight 0u (fst randomTile) st)
       
    let findAvailableTile (board: Map<coord, uint32 * (char * int)>) (st: State.state) =
        let rec aux m =
            match m with
              | head :: tail ->
                  match calculateAvailableSpace head st with
                  | 0u, 0u -> aux tail 
                  | _ -> (head, calculateAvailableSpace head st)
              | [] -> failwith "no available squares"
        aux (board |> Map.toList)

    let temp ((a, (b, (x, d))), (e, f)) c state : 'i list =
        c (e,f) x a
        
    let move (state : State.state) =
        let allPossible =
            if (Map.containsKey (0,0) state.squares) then
                let step c =
                    match (Dictionary.step c state.dict) with
                    | Some (_,y) -> y
                    | None _ -> state.dict
                let availableTile = findAvailableTile state.squares state
                let c = (fun ((_, (_, (y,_))), _) -> y) availableTile
                findWordCombination state ((fun (_,y) -> y) availableTile) (step c) ((fun ((a,_),_) -> a) availableTile)
            else
                findWordCombination state (7u,7u) state.dict (-1,0)
        if (List.length allPossible = 0) then List.empty else (fun (x, _) -> x) (List.sortByDescending (fun (_, y) -> y) allPossible |> List.head)
       
                
module Scrabble =
    type Direction =
        | across = 0
        | down = 1
 
    let moveOnBoard direction (x, y)=
        match direction with
        | Direction.down -> (x, y + 1)
        | Direction.across -> (x + 1, y)
        | _ -> (x,y)
    let rec removePlayer playerNumber players =
        match players with
        | head :: tail when head = playerNumber -> tail
        | head :: tail -> removePlayer playerNumber (tail @ [ head ])
        | _ -> players
    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            let msg =
                if (st.playerNumber = st.playerTurn) then
                    Print.printHand pieces (State.hand st)
                    let move = Move.move st
                    if move.Length = 0 then send cstream (SMChange (toList st.hand))
                    else send cstream (SMPlay move)
                    recv cstream
                else
                    recv cstream
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->                
                let newHand = State.updateHand st.hand (List.map (fun m -> fst (snd m)) ms |> ofList) newPieces
                let newBoard = State.updateBoard ms st.squares
                let st' = State.mkState st.board st.dict st.players st.playerNumber st.players[int (st.playerNumber) % st.players.Length] newBoard newHand st.tiles
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                let newBoard = State.updateBoard ms st.squares
                let st' = State.mkState st.board st.dict st.players st.playerNumber st.players[int (pid) % st.players.Length] newBoard st.hand st.tiles // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                let st' = State.mkState st.board st.dict st.players st.playerNumber st.players[int (pid) % st.players.Length] st.squares st.hand st.tiles // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess newTiles) ->
                let newHand = State.updateHand empty st.hand newTiles
                let st' = State.mkState st.board st.dict st.players st.playerNumber st.players[int (st.playerNumber) % st.players.Length] st.squares newHand st.tiles
                aux st'
            | RCM (CMChange (pid, numbTiles)) ->
                let st' = State.mkState st.board st.dict st.players st.playerNumber st.players[int (pid) % st.players.Length] st.squares st.hand st.tiles
                aux st'
            | RCM (CMPassed pid) ->
                let st' = State.mkState st.board st.dict st.players pid st.players[int (pid) % st.players.Length] st.squares st.hand st.tiles
                aux st'
            | RCM (CMForfeit pid) ->
                let updatedPlayers = removePlayer pid st.players
                let st' = State.mkState st.board st.dict updatedPlayers pid updatedPlayers[int (pid) % updatedPlayers.Length] st.squares st.hand st.tiles
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf false
        let board = Parser.mkBoardd boardP
                  
        let handSet = List.fold (fun acc (x, k) -> add x k acc) empty hand
        
        let wordToString (word : Eval.word) =
            let temp = List.map (fun (x, _) -> x) word
            System.String.Concat(Array.ofList(temp))
            
        let players = [ 1u .. numPlayers ]
        
        fun () -> playGame cstream tiles (State.mkState board dict players playerNumber playerTurn Map.empty handSet tiles)
        