module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('h', 4); ('e', 1); ('l', 1); ('l', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a: SM<int>) (b: SM<int>) =  a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 + v2))) 
    let div a b = a >>= (fun v1 -> b
                                   >>= (fun v2 ->
                                      if (v2 <> 0) then
                                          ret (v1 / v2)
                                      else
                                          fail DivisionByZero))
    let sub (a: SM<int>) (b: SM<int>) : SM<int> = 
        a >>= (fun v1 -> b >>= (fun v2 -> ret (v1-v2)))

    let mul (a: SM<int>) (b: SM<int>) : SM<int> = 
        a >>= (fun v1 -> b >>= (fun v2 -> ret (v1*v2)))
    let modulo a b = 
        a >>= (fun v1 -> b >>= (fun v2 ->
                                          if (v2 <> 0) then
                                            ret (v1 % v2)
                                          else
                                            fail DivisionByZero))
     
    let eql a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 = v2)))
    let lt a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 < v2)))
    let conj a b = a >>= (fun v1 -> b >>= (fun v2 -> ret (v1 && v2)))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
    
    let nAEq (a, b) = (.<>.) a b
    let Disj (a, b) = (.||.) a b
    let Impli (a, b) = (.->.) a b
    let SOEq (a, b) = (.<=.) a b
    let GOEq (a, b) = (.>=.) a b
    let AGt (a, b) = (.>.) a b

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> arithEval pv >>= pointValue
        | Add (a,b) -> add (arithEval a) (arithEval b)
        | Sub (a,b) -> sub (arithEval a) (arithEval b)
        | Mul (a,b) -> mul (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt c -> charEval c >>= (fun x -> ret (int(x)))     

    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV cv -> arithEval cv >>= characterValue
        | ToUpper c -> charEval c >>= (fun x -> ret (System.Char.ToUpper x))
        | ToLower c -> charEval c >>= (fun x -> ret (System.Char.ToLower x))
        | IntToChar a -> arithEval a >>= (fun x -> ret (char(x)))
                                          
    and boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> eql (arithEval a) (arithEval b)
        | ALt (a, b) -> lt (arithEval a) (arithEval b)
        | Not a -> (boolEval a) >>= (fun x -> ret (not x))
        | Conj (a, b) -> conj (boolEval a) (boolEval b)
        | IsLetter a -> (charEval a) >>= (fun x -> ret (System.Char.IsLetter x))
        | IsDigit a -> (charEval a) >>= (fun x -> ret (System.Char.IsDigit x))


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare var -> declare var
        | Ass (var, value) -> (arithEval value) >>= update var
        | Skip -> ret()
        | Seq (s1, s2) -> stmntEval s1 >>>= stmntEval s2
        | ITE (b, s1, s2) -> boolEval b >>= (fun x -> if x
                                                        then stmntEval s1
                                                        else stmntEval s2)
        | While (b, s) -> boolEval b >>= (fun x -> if x
                                                    then stmntEval s
                                                    >>>= stmntEval(While(b, s))
                                                    else stmntEval(Skip))
       

(* Part 3 (Optional) *)

//    type StateBuilder() =
//
//        member this.Bind(f, x)    = f >>= x
//        member this.Return(x)     = ret x
//        member this.ReturnFrom(x) = x
//        member this.Delay(f)      = f ()
//        member this.Combine(a, b) = a >>= (fun _ -> b)
//        
//    let prog = new StateBuilder()
//
//    let arithEval2 a = failwith "Not implemented"
//    let charEval2 c = failwith "Not implemented"
//    let rec boolEval2 b = failwith "Not implemented"
//
//    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    //type squareFun = word -> int -> int -> int

    type coord = int * int

    //type boardFun = coord -> Result<squareFun option, Error>
    type boardFun = coord -> squareFun option

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }
    let stmntToSquareFun =
        fun stm w pos acc ->
            mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]
            |> fun s -> evalSM s (stmntEval stm >>>= lookup "_result_")

    let boardState x y =
        mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] [ "_x_"; "_y_"; "_result_" ]
       
    let stmntToBoardFun stm squares =
        let boardfun (x, y) =
            let sm = stmntEval stm >>>= lookup "_result_" >>= fun id ->
                match Map.tryFind id squares with
                    | Some i -> ret (Some i)
                    | _ -> ret None
            match (evalSM (boardState x y) sm) with
                | Success(x) -> x
                | Failure(_) -> None
        boardfun