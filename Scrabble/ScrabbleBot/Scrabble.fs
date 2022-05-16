namespace MIC

open System.Collections.Generic
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open MultiSet

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

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
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    // squares : used squares on the board (coord - position; (char * int) - letter * point value)
    // tiles : uint32 - id of a tile, tile - tile itself (char * int - letter * pv)
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
    
    let updateHand (hand: MultiSet<uint32>) (moves: list<coord * (uint32 * (char * int))>) (newTiles: list<uint32 * uint32>) =
        let usedTiles = List.map (fun m -> fst (snd m)) moves |> ofList
        let newHand   = subtract hand usedTiles
        List.fold (fun acc (tileId, num) -> add tileId num acc) newHand newTiles
        
    let updateBoard (moves: list<coord * (uint32 * (char * int))>) (prevBoard: Map<coord, uint32 * (char * int)>) =
        List.fold (fun acc m -> Map.add (fst m) (snd m) acc) prevBoard moves
   

module Move =
    type availableSpace = (uint32 * uint32)
    let allCombinations lst : string list =
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
    let findIdAndPoint s pieces =
        let piece = List.filter (fun (a, b) -> (fun (c, d) -> c = s) (Seq.head b)) pieces |> List.head
        (fun (x,y) -> (fun (z,p) -> (x,p)) (Seq.head y)) piece
        
    let stringToMove (s : string) pieces coord (space : (int * bool)) =
        let piecesList = Map.toList pieces
        let rec aux (ycoord : int) str =
            match str with
            | a::t ->
                if (fun (_, x) -> x) space then
                    let x = ((fun (x, _) -> x) coord) + (ycoord + 1)
                    let y = ((fun (_, y) -> y) coord)
                    let (id, points) = findIdAndPoint a piecesList
                    let p = if a = 'A' then 1u else id
                    let obj = sprintf "%d %d %d%A%d" x y p a points
                    obj :: aux (ycoord + 1) t
                else
                    let x = ((fun (x, _) -> x) coord)
                    let y = ((fun (_, y) -> y) coord) + (ycoord + 1)
                    let (id, points) = findIdAndPoint a piecesList
                    let strip chars = String.collect (fun c -> if Seq.exists((=)c) chars then "" else string c)
                    let teil = strip "'" (sprintf "%d%A%d" id a points)
                    let obj =strip "\"" (sprintf "%d %d %A" x y teil)
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
        | StateMonad.Failure _ -> failwith "No state"
        
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
            | StateMonad.Failure y -> 0
    
    let calculatePoints (m : ((int * int) * (uint32 * (char * int))) list) state  =
        let word = toWord m
        let numberList = List.map (fun (c, (_, (_,p))) -> (getSinglePoint (coordToSquarefun state c) p word)) m
        let number = List.fold (fun a b -> a + b) 0 numberList
        if (Map.count (coordToSquarefun state ((fun (x, _) -> x) (List.head m)))) = 1 then number else number * 2 
    
    let wordToString (word : Eval.word) =
            let temp = List.map (fun (x, _) -> x) word
            System.String.Concat(Array.ofList(temp))
            
    let findWordCombination (state : State.state) (avSpace : availableSpace) dict coord =
        let characters = Seq.toList (Map.values(Map.map (fun s _ -> Map.find s state.tiles) (MultiSet.toMap state.hand)))
        let combinations = allCombinations characters                
        let space = (fun (x,y) -> if x > y then (x, true) else (y, false)) avSpace
        let allPossible = List.filter (fun m -> Dictionary.lookup m dict) combinations |> List.filter (fun m -> m.Length <= (fun (x,_) -> x) ((fun (x,y) -> (int x,y)) space))        
        let allPossibleWords = List.map (fun x -> stringToMove x state.tiles coord ((fun (x,y) -> (int x,y)) space)) allPossible
        let parsed = List.map RegEx.parseMove allPossibleWords
        if (List.length parsed = 0) then List.empty else List.map (fun a -> (a, calculatePoints a state)) parsed

    let checkSquareFree (coord: coord) (st : State.state) =
        match Map.tryFind coord st.squares with
        | Some value -> false
        | None   -> true
   
    let rec countSpacesDown count (x, y) (st: State.state) =
        match ((size st.hand) > count) with
        | false -> count
        | true ->
            match (checkSquareFree (x, y + 1) st)
                && (checkSquareFree (x + 1, y + 1) st)
                && (checkSquareFree (x - 1, y + 1) st)
                && (checkSquareFree (x, y + 2) st) with
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
                    | true -> countSpacesRight (count + 1u) (x + 1, y) st
                    | false -> count
                    
    let calculateAvailableSpace (randomTile: coord * (uint32 * (char * int))) (st: State.state) =
       (countSpacesDown 0u (fst randomTile) st, countSpacesRight 0u (fst randomTile) st)
       
    let findAvailableTile (board: Map<coord, uint32 * (char * int)>) (st: State.state) =
         // head = coord * uint32 * (char * int)
         // key = coord
         // value = uint32 * (char * int) = Tile
        let rec aux m =
            match m with
              | head :: tail ->
                  match calculateAvailableSpace head st with
                  | 0u, 0u -> aux tail 
                  | _ -> (head, calculateAvailableSpace head st)
              | [] -> failwith "no available squares"
        aux (board |> Map.toList)

    let temp ((a, (b, (x, d))), (e, f)) c state =
        c (e,f) x a
        
    let move (state : State.state) =
        let allPossible =
            if (Map.containsKey (0,0) state.squares) then
                let step c =
                    match (Dictionary.step c state.dict) with
                    | Some (_,y) -> y
                    | None _ -> state.dict
                let combinations d c a = findWordCombination state d (step c) a
                temp (findAvailableTile state.squares state) combinations state
                
            else
                findWordCombination state (7u,7u) state.dict (0,-1)
                //(fun ((a : coord), (_, ((c : char), _)), (d : availableSpace)) -> combinations d c a) (findAvailableTile state.squares state)
        if (List.length allPossible = 0) then List.empty else (fun (x, _) -> x) (List.sortByDescending (fun (_, y) -> y) allPossible |> List.head)
       
            
                
module Scrabble =
    open System.Threading

    type Direction =
        | across = 0
        | down = 1
 
    let moveOnBoard direction (x, y)=
        match direction with
        | Direction.down -> (x, y + 1)
        | Direction.across -> (x + 1, y)
        | _ -> (x,y)
    
    // tile = id: uint32 * (letter: char * pointValue: int)
    let idTile (tile: uint32 * (char * int)) =
        fst tile
    let pvTile (tile: uint32 * (char * int)) =
        snd (snd tile)
       
    // find a tile by its id in the tiles   
    let findTile id (tiles: Map<uint32, tile>) =
        match Map.tryFind id tiles with
        | Some v -> v
        | None -> failwith "."
     
     // return tile: (letter: char * pointValue: int) if square is occupied
    // otherwise return None
    let ifSquareFree (coord: coord) (st : State.state) =
        match Map.tryFind coord st.squares with
        | Some value -> Some value
        | None   -> None
    let mutable countPass = 0
    let generateTurn (st: State.state) =
        let turn = st.playerTurn + 1u
        if turn > uint32 (List.length st.players) then 1u
        else turn
    let playGame cstream pieces (st : State.state) =
       
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            let turn = generateTurn st
//            let newPlayers =
//                match st.players with
//                | head :: tail -> tail @ [ head ]
//                | [] -> []
            let rec removePlayer playerNumber players =
                match players with
                | head :: tail when head = playerNumber -> tail
                | head :: tail -> removePlayer playerNumber (tail @ [ head ])
                | _ -> players
         
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let move = Move.move st
            if move.Length = 0 then send cstream (SMChange (toList st.hand))
            else send cstream (SMPlay move)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let newHand = State.updateHand st.hand ms newPieces
                let newBoard = State.updateBoard ms st.squares
                let st' = State.mkState st.board st.dict st.players st.playerNumber turn newBoard newHand st.tiles
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoard = State.updateBoard ms st.squares
                let st' = State.mkState st.board st.dict st.players pid turn newBoard st.hand st.tiles // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.mkState st.board st.dict st.players pid turn st.squares st.hand st.tiles // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess newTiles) ->
                let newHand = State.updateHand empty [] newTiles
                let st' = State.mkState st.board st.dict st.players st.playerNumber turn st.squares newHand st.tiles
                aux st'
            | RCM (CMPassed pid) ->
                let st' = State.mkState st.board st.dict st.players pid turn st.squares st.hand st.tiles
                aux st'
            | RCM (CMForfeit pid) ->
                let updatedPlayers = removePlayer pid st.players
                let st' = State.mkState st.board st.dict updatedPlayers pid turn st.squares st.hand st.tiles
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

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoardd boardP
                  
        let handSet = List.fold (fun acc (x, k) -> add x k acc) empty hand
        
        let wordToString (word : Eval.word) =
            let temp = List.map (fun (x, _) -> x) word
            System.String.Concat(Array.ofList(temp))
            
          
        let players = [ 1u .. numPlayers ]
            
        let tester () =
            let state = State.mkState board dict ([playerNumber] @ List.Empty) playerNumber playerTurn Map.empty handSet tiles
            let avSpace = (8u,8u)
            let dict = state.dict
            let coord = (0,0)
            let find = Move.findWordCombination state avSpace dict coord
            for (x,y) in find do
                let word = Move.toWord x |> wordToString
                printfn "%d -> (%A)\n" y word                
        tester ()
        
        fun () -> playGame cstream tiles (State.mkState board dict players playerNumber playerTurn Map.empty handSet tiles)
        