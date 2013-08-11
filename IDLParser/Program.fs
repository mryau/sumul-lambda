module Main
//#r @"D:\Projects\IDLParser\packages\FParsec-Big-Data-Edition.1.0.1\lib\net40-client\FParsecCS.dll";;
//#r @"D:\Projects\IDLParser\packages\FParsec-Big-Data-Edition.1.0.1\lib\net40-client\FParsec.dll";;
//#load @"D:\Projects\IDLParser\IDLParser\FsJson.fs"
//#load @"D:\Projects\IDLParser\IDLParser\FsJsonDescription.fs"
open System
open System.IO
open System.Collections.Generic
open FParsec

type Prg =
    |Lambda of string*Expr
and Expr =
    |Zero
    |One
    |Id of string
    |If0 of Expr*Expr*Expr
    |Fold of Expr*Expr*string*string*Expr
    |Not of Expr
    |Shl1 of Expr
    |Shr1 of Expr
    |Shr4 of Expr
    |Shr16 of Expr
    |And of Expr*Expr
    |Or of Expr*Expr
    |Xor of Expr*Expr
    |Plus of Expr*Expr

let lookup cont k = List.find (fst >> ((=) k)) cont |> snd
let rec eval cont = function
    |Zero -> 0UL
    |One -> 1UL
    |Id s -> lookup cont s
    |If0 (e1,e2,e3) ->
        if 0UL = eval cont e1 then eval cont e2 else eval cont e3
    |Fold (e0,e1,id1,id2,e2) ->
        let rec eval' n vn acc =
            if n < 8 then
                let v = (vn >>> n) &&& 0xFFUL
                let acc' = eval ((id1,v)::(id2,acc)::cont) e2
                eval' (n+1) vn acc'
            else
                acc
        eval' 0 (eval cont e0) (eval cont e1)
    |Not e -> 0xFFFFFFFFFFFFFFFFUL - (eval cont e)
    |Shl1 e -> (eval cont e) <<< 1
    |Shr1 e -> (eval cont e) >>> 1
    |Shr4 e -> (eval cont e) >>> 4
    |Shr16 e -> (eval cont e) >>> 16
    |And (e1, e2) -> (eval cont e1) &&& (eval cont e2)
    |Or (e1, e2) -> (eval cont e1) ||| (eval cont e2)
    |Xor (e1, e2) -> (eval cont e1) ^^^ (eval cont e2)
    |Plus (e1, e2) -> (eval cont e1) + (eval cont e2)

let ws = spaces
let str s = pstring s >>. ws
let idStr : Parser<string,unit> = many1Satisfy isLetter .>>. manySatisfy (fun c -> isLetter c || isDigit c || c = '_') .>> ws |>> (fun (s1,s2) -> s1+s2)

let (expr: Parser<Expr, 'a>), exprRef = createParserForwardedToRef()

let if0 =
    pipe3
        (str "if0" >>. expr)
        expr
        expr
        (fun e1 e2 e3 -> If0 (e1,e2,e3))
let fold = 
    pipe4
        (str "fold" >>. expr)
        expr
        (str "(" .>> str "lambda" .>> str "(" >>. idStr .>>. idStr .>> str ")")
        (expr .>> str ")")
        (fun e1 e2 (id1,id2) e3 -> Fold (e1,e2,id1,id2,e3))
let not_op = str "not" >>. expr |>> Not
let shl1_op = str "shl1" >>. expr |>> Shl1
let shr16_op = str "shr16" >>. expr |>> Shr16
let shr1_op = str "shr1" >>. expr |>> Shr1
let shr4_op = str "shr4" >>. expr |>> Shr4
let and_op = str "and" >>. expr .>>. expr |>> And
let or_op = str "or" >>. expr .>>. expr |>> Or
let xor_op = str "xor" >>. expr .>>. expr |>> Xor
let plus_op = str "plus" >>. expr .>>. expr |>> Plus

do exprRef :=
    choice [
        str "0" |>> (fun () -> Zero)
        str "1" |>> (fun () -> One)
        idStr |>> Id
        between
            (str "(") (str ")")
            (choice [ if0; fold; not_op; shl1_op; shr16_op; shr1_op; shr4_op; and_op; or_op; xor_op; plus_op ])
    ]

let prog =
    str "(" >>. str "lambda" >>. str "(" >>. idStr .>> str ")" .>>. expr .>> str ")" .>> eof |>> Lambda

let s = "(lambda (x_40004) (fold (if0 (plus (not 1) (or 1 x_40004)) 1 x_40004) 1 (lambda (x_40005 x_40006) (if0 x_40006 x_40005 x_40005))))"

let test n p str =
    match run p str with
    | Success(Lambda (v,expr), _, _)   ->
        //printfn "Success: %s: %A" v expr
        let r = eval [v, n] expr
        //printfn "Success: %s -> %d" v r
        r
    | Failure(errorMsg, _, _) -> // printfn "Failure: %s" errorMsg
        failwith errorMsg

//test prog s

let reader fname =
    seq { use reader = new StreamReader(File.OpenRead(fname))
          while not reader.EndOfStream do
              let s = reader.ReadLine()
              //printfn "got %s" s
              yield FsJson.parse s }

let fname = @"I:\Lang\icfp\2013\training_programs.json"
//let tests = File.ReadAllText(fname)
//let utf8 = File.ReadAllText(fname, System.Text.Encoding.UTF8)
let tests = reader fname
for j in tests do
    //let j = FsJson.parse s

let ops = [|"not";"shl1";"shr1";"shr4";"shr16";"and";"or";"xor";"plus"; "if0"|]
let filter = function
    |JsonString s when None <> Array.tryFind (fun elem -> s = elem) ops -> true
    |_ -> false

let fname2 = @"I:\Lang\icfp\2013\mypromlems.json"
let tests2 = reader fname
let j = Seq.head tests2
for v in j.Array do
    if v?size.Val = "8" && Array.forall filter v?operators.Array then
        printfn "%s: %A" v?id.Val v?operators.Array
        
printfn "255 = %s" (test 0x1122334455667788UL prog "(lambda (x) (fold x 0 (lambda (y z) (or y z))))")


System.Console.Read() |> ignore