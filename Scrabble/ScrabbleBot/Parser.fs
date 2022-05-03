// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the module Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"
    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    let pletters = many palphanumeric |>> fun (charList) -> System.String.Concat(Array.ofList(charList))
    

    let anyOf ss =
        ss |> List.map pchar |> choice <?> 
        sprintf "anyOf %A" ss

    let parenthesiseCharList = ['(' ; ')'; ' ']
    let parenthesiseChar = anyOf parenthesiseCharList
    let parentesiseHelper = many parenthesiseChar
    let underscore = anyOf ['_']
    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2 
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' 
    let squarebracket p = pchar '{' >*>. p .>*> pchar '}' 
    let second_part = many (palphanumeric <|> underscore)  |>> fun (charList) -> System.String.Concat(Array.ofList(charList))
    let pid = pletter <|> underscore .>>. second_part |>> fun (c1, s2) -> System.String.Concat(c1, s2)

    
    let unop parser1 parser2 =
         parser1 >*>. parser2
    let binop p1 p2 p3 = (p2 .>*> p1) .>*>. p3
    
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let AexpParse = TermParse
    
    let CharParse, charref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    
    
    // <?> in case of a failure write to the console a message "Int"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse =  pid |>> V <?> "Var"
  
    let NegParse = pchar '-' >>. AexpParse |>> (fun x -> Mul (N -1, x)) <?>  "Negation"
    //let NegParse = pchar '-' >>. pint32 |>> (fun x -> Mul (N -1, N x)) <?>  "Negation"
    let PVParse = pstring "pointValue"  >*>. parenthesise AexpParse |>> PV <?> "PV"
    let CharToInt = pstring "charToInt" >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"
    let ParParse = parenthesise TermParse
    do aref := choice [CharToInt; NegParse; PVParse; NParse; VParse; ParParse]
    
    let CexpParse = CharParse
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let LowerParse = pstring "toLower" >*>. parenthesise CexpParse |>> ToLower <?> "toLower"
    let UpperParse = pstring "toUpper" >*>. parenthesise CexpParse |>> ToUpper <?> "toUpper"
    let CharValue = pstring "charValue" >*>. parenthesise AexpParse |>> CV <?> "CV"
    let IntToChar = pstring "intToChar" >*>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"
     
    do charref := choice [CParse; LowerParse; UpperParse; CharValue; IntToChar]

    let FirstParse, fref = createParserForwardedToRef<bExp>()
    let SecondParse, sref = createParserForwardedToRef<bExp>()
    let ThirdParse, thref = createParserForwardedToRef<bExp>()
    let BexpParse = FirstParse

    let conjParse = binop (pstring "/\\") SecondParse FirstParse |>> Conj <?> "Conj"
    let disjParse = binop (pstring "\\/") SecondParse FirstParse |>> Disj <?> "Disj"
    do fref := choice [conjParse; disjParse; SecondParse]
    
    let notEqualParse = binop (pstring "<>") AexpParse AexpParse |>> nAEq <?> "notEqual"
    let equalParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "AEq"
    let lessThanParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "ALt"
    let smallerEqualParse = binop (pstring "<=") AexpParse AexpParse |>> SOEq <?> "Smaller or Equal"
    let greaterEqualParse = binop (pstring ">=") AexpParse AexpParse |>> GOEq <?> "Greater or Equal"
    let greaterParse = binop (pchar '>') AexpParse AexpParse |>> AGt <?> "Greater or Equal"
    do sref := choice [notEqualParse; equalParse; lessThanParse; smallerEqualParse; greaterEqualParse; greaterParse; ThirdParse]

    let isVowelParse = pstring "isVowel" >*>. parenthesise CexpParse |>> IsVowel <?> "IsVower"
    let isLetterParse = pstring "isLetter" >*>. parenthesise CexpParse |>> IsLetter <?> "IsLetter"
    let isDigitParse = pstring "isDigit" >*>. parenthesise CexpParse |>> IsDigit <?> "IsDigit"
    let negParse = pchar '~' >*>. FirstParse |>> (~~) <?> "Not"
    let trueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let falseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let bParParse = parenthesise FirstParse
    do thref := choice [isVowelParse; isLetterParse; isDigitParse; negParse; trueParse; falseParse; bParParse]
    (*
    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    *)

    let ITE2 ((a, b), c) = ITE (a, b, c)
    let IT (b, stm) = ITE (b, stm, Skip) 

    let outerParse, ouref = createParserForwardedToRef<stm>()
    let innerParse, inref = createParserForwardedToRef<stm>()
    let stmntParse = outerParse

    let seqParse = outerParse .>*> pchar ';' .>*>.  outerParse |>> Seq <?> "Seq"
    do  ouref := choice [seqParse; innerParse]

    let ITEParse = pstring "if" >*>. parenthesise BexpParse .>*>  pstring "then" .>*>. squarebracket outerParse .>*> pstring "else" .>*>. squarebracket outerParse |>> ITE2 <?> "ITE"
    let ITParse = pstring "if" >*>. parenthesise BexpParse .>*>  pstring "then" .>*>. squarebracket outerParse |>> IT <?> "ITE"
    let whileParse = pstring "while" >*>. parenthesise BexpParse .>*> pstring "do" .>*>. squarebracket outerParse |>> While <?> "While"
    let declareParse = pstring "declare" >>. spaces1 >>.  pid |>> Declare <?> "Declare"
    let assParse = binop (pstring ":=") pid AexpParse |>> Ass <?> "Ass"
    do inref := choice [declareParse; ITEParse; ITParse; whileParse; assParse]

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }
    let parseBoardProg (bp : boardProg) : board = failwith "not implemented"