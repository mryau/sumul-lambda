//#r @"D:\Projects\IDLParser\packages\FParsec-Big-Data-Edition.1.0.1\lib\net40-client\FParsecCS.dll";;
//#r @"D:\Projects\IDLParser\packages\FParsec-Big-Data-Edition.1.0.1\lib\net40-client\FParsec.dll";;
open System
open System.Collections.Generic
open FParsec

type IDL =
    |Library of string * IDL list
    |ImportLib of string
    |ImportAlias of string

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

let ws = spaces
let str s = pstring s >>. ws
let idStr = many1Satisfy isLetter .>>. many1Satisfy (fun c -> isLetter c || isDigit c || c = '_') .>> ws |>> (fun (s1,s2) -> s1+s2)

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
let or_op = str "or" >>. expr .>>. expr |>> And
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

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test prog s

System.Console.Read() |> ignore