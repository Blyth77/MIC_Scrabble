namespace MIC

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
        players       : List<int>
        playerNumber  : uint32
        playerTurn    : uint32
        tiles         : Map<uint32, tile>
        squares       : Map<coord, char * int>
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pn h pl turn ts sq = {
                            board = b
                            dict = d
                            playerNumber = pn
                            hand = h
                            players = pl
                            playerTurn = turn
                            tiles = ts
                            squares = sq }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let players st       = st.players
    let playerTurn st    = st.playerTurn
    let squares st       = st.squares
    let tiles st         = st.tiles
    
   
    // hand: MultiSet<tileId: unit32>
    // moves: coord: (x, y) * (tileId: uint32) * (letter: char, pointValue: int)
    // newTiles: list <tileId: uint32, numTiles: uint32>
    let updateHand (hand: MultiSet<uint32>) (moves: list<coord * (uint32 * (char * int))>) (newTiles: list<uint32 * uint32>) =
        let usedTiles = List.map (fun m -> fst (snd m)) moves |> ofList
        let newHand   = subtract hand usedTiles
        List.fold (fun acc (tileId, num) -> add tileId num acc) newHand newTiles
        
    let updateSquares (moves: list<coord * (uint32 * (char * int))>) (prevSquares: Map<coord, char * int>) =
        List.fold (fun acc m -> Map.add (fst m) (snd (snd m)) acc) prevSquares moves
   
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
    let a (st: State.state) = Map.toList st.squares 
    
    let removePlayer (st: State.state) playerNumber =
        match List.tryFind playerNumber st.players with
        | Some index -> List.removeAt index st.players
        | None -> failwith "."
        
       
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->

                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
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
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        //fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        