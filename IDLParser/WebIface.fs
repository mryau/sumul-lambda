module WebIface

open System.Text
open System.IO
open System.Net

type Query =
    |MyProblems
    |Train of string
    |Eval of string
    |Guess of string
    |Status

let doQuery q = 
    let secret = "0110dZ2MDEqFSPIfHvznfra1A9o6WtCZ8XL0meWk" + "vpsH1H"
    let host = "http://icfpc2013.cloudapp.net"
    let path, arg =
        match q with
        |MyProblems -> "/myproblems", "" 
        |Train s -> "/train", s
        |Eval s -> "/eval", s
        |Guess s -> "/guess", s
        |Status -> "/status", "" 
    let url = host+path+"?auth="+secret

    // Create & configure HTTP web request
    let req = HttpWebRequest.Create(url) :?> HttpWebRequest 
    req.ProtocolVersion <- HttpVersion.Version10
    req.Method <- "POST"

    // Encode body with POST data as array of bytes
    let postBytes = Encoding.ASCII.GetBytes(arg)
    req.ContentType <- "application/json";
    req.ContentLength <- int64 postBytes.Length
    // Write data to the request
    let reqStream = req.GetRequestStream() 
    reqStream.Write(postBytes, 0, postBytes.Length);
    reqStream.Close()

    // Obtain response and download the resulting page 
    // (The sample contains the first & last name from POST data)
    let resp = req.GetResponse() 
    let stream = resp.GetResponseStream() 
    let reader = new StreamReader(stream) 
    let html = reader.ReadToEnd()
    html

//#load @"D:\Projects\IDLParser\IDLParser\FsJson.fs";;
open FsJson
let s0 = doQuery Status

let s1 = doQuery MyProblems

let jTrain =
    jval
        [
            "size", jval 3
            "operators", jval []
        ]

let q2 = jTrain |> FsJson.serialize |> Train
let s2 = doQuery q2

let jGuess3 =
    jval
        [
            "id", jval "v6MxST0BnaDZbOvopAyTw8sH"
            "program", jval "(lambda (x) (not x))"
        ]
let q3 = jGuess3 |> FsJson.serialize |> Guess
let s3 = doQuery q3

//win 3:
//4G0cKnK9BodOSrT2faIju5Kc (lambda (x) (shr4 x))
//6JIAv9Mo2yjAf1xPte6yugMU (lambda (x) (shr4 x))
//BRCZwEVtezAX3bTgs9dRA9bP (lambda (x) (shr1 x))
//EuB7WBDzYk1AdvBy9mgKN66e (lambda (x) (shl1 x))
//JW89yH3bgKZxj9WOBjdhZJpI (lambda (x) (not x))
//KdOkUwR2A6xpKc99zZm7REBy (lambda (x) (shr16 x))
//RIow3ffvGjBEsAGMlM4LZmXo (lambda (x) (shl1 x))
//SIldNgELdEvsGI6uG0UyXxIu (lambda (x) (shr4 x))
//TUER6AOsFLZBBQBdX9cUwG6p (lambda (x) (shr16 x))
//abMDeO14lq9KmzrXK56RjG5h (lambda (x) (shr16 x))
//c7sFDGBlc3CEP7faVPC1TAFx (lambda (x) (shl1 x))
//e1l9oRxARsWxGwT1p6c2LWxl (lambda (x) (shr16 x))
//eVgOITXmwjZXEThlvWXX8Vpj (lambda (x) (shl1 x))
//fxPvSU1acRhfWSTGEd7AtXYb (lambda (x) (shr16 x))
//iciDGlJpNOO6AQqq0lNqWdIy (lambda (x) (shr16 x))
//j5aUPuKIWxW7dFE35UAGzXnA (lambda (x) (shr16 x))
//msyxkBy5pKcCBMaVEcfu2vRK (lambda (x) (not x))
//rOlID5h79sP6Lce2Xnzb2C89 (lambda (x) (shr16 x))
//uIm8rnAYEDxPzZXycQ1OeOT4 (lambda (x) (shr16 x))
//v6MxST0BnaDZbOvopAyTw8sH (lambda (x) (not x))


let jGuess4_op1 id (op1,op2) v =
    jval
        [
            "id", jval id
            "program", jval (sprintf "(lambda (x) (%s (%s %s)))" op1 op2 v)
        ]
let q4 = jGuess4_op1 "wkAcDiuebl2MnEGEx9LJ7T2y" ("shr1","shl1") "x" |> FsJson.serialize |> Guess
let s4 = doQuery q4

let jGuess4_op2 id op v1 v2 =
    jval
        [
            "id", jval id
            "program", jval (sprintf "(lambda (x) (%s %s %s))" op v1 v2)
        ]
let q4_op2 = jGuess4_op2 "qqeKaChhyo0qQgJpFI8IodJk" "and" "x" "1" |> FsJson.serialize |> Guess
let s4_op2 = doQuery q4_op2
printfn "%s" s4_op2;;

//win 4:
//jGuess4_op1 "8mAotDBDVZuipjcaCGDLPApu" ("not","shr16")
//jGuess4_op1 "AXFVAqB5pJsFl0wrUbuycdzq" ("shr1","not")
//jGuess4_op2 "EYsSkqfrthng91ZK0qxlUpQ7" "or" "1"
//jGuess4_op2 "Gl1q9BTtnByDACNhsmIjT4YU" "plus" "1" (lambda (x) (%s x x))
//jGuess4_op2 "HG1vknbydkJlbMnSemGFY9LL" "and" "x" "1"
//jGuess4_op1 "PpGuWu2bEVdVQAQ1pKbT5iqa" ("not","shr1")
//jGuess4_op1 "RGBJ4FloGRX8SoUdaNwfnSW9" ("not","shl1")
//jGuess4_op2 "SiR0bNOs59oDCZ7df25pqnbD" "or" "x" "1"
//jGuess4_op1 "T6VcJtRNnDwIJeCh37BueIOT" ("shr4","shr4") "x"
//jGuess4_op1 "aHZcadNTB6rwRgd8a05pFvpA" ("shl1","shr4") "x"
//jGuess4_op1 "clJGRyWmBuCmGBOX7zcFvycb" ("not","shr1") "x"
//jGuess4_op1 "faf9ADOtT3QIL7DAGv725K4H" ("shl1","shl1") "x"
//jGuess4_op2 "g7fKgrRfbENGOeHXr6GAaT6K" "plus" "x" "1"
//jGuess4_op2 "jN7qgwXRE4tPfMyt5gNYxfp1" "plus" "x" "x"
//jGuess4_op1 "lNT4FyyboJdbPTiZhj4BDhQ2" ("shr1","shr1") "x"
//jGuess4_op1 "lycIV5QNA7gYb8AAZrQ0rtfF" ("shr1","not") "x"
//jGuess4_op1 "nISATvQeh7n1C4pg8RSwTvHd" ("shl1","shr4") "x"
//jGuess4_op2 "oiBRGxdlrDJ4IMAP5u2Y8S3O" "plus" "x" "x"
//jGuess4_op2 "qqeKaChhyo0qQgJpFI8IodJk" "and" "x" "1"
//jGuess4_op1 "wkAcDiuebl2MnEGEx9LJ7T2y" ("shr1","shl1") "x"

type Tree =
    |Term
    |Op1 of string*Tree
    |Op2 of string*Tree*Tree

let rec doit t =
    seq {
        match t with
            |Term ->
                for v in ["x";"0";"1"] do
                    yield v
            |Op1 (op,t) ->
                for v in doit t do
                    yield ("("+op+" "+v+")")
            |Op2 (op,t1,t2) ->
                for v1 in doit t1 do
                    for v2 in doit t2 do
                        yield ("("+op+" "+v1+" "+v2+")")
    }

let t1 = Op1 ("shr4", Op2 ("plus",Term,Term))
let t2 = Op2 ("plus", Term, Op1 ("shr4", Term))

//Array.ofSeq (doit t1);;
for t in [t1;t2] do
    for s in doit t do
        let jGuess5 id =
            jval
                [
                    "id", jval id
                    "program", jval ("(lambda (x) "+s+")")
                ]
        let id = "Rbny9NZuQE1URWimmvz966HP"
        let q5 = jGuess5 id |> FsJson.serialize |> Guess
        let s5 = doQuery q5
        let r5 = FsJson.parse s5
        if r5?status.Val = "win" then
            printfn "win: %s %s" id s
            failwith "Ok"
        else
            printfn "fig: %s %s" r5?status.Val s
            System.Console.Read() |> ignore

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

for s1::s2::s3::[] in permute ["not";"not";"shr4"] do
    for s in doit (Op1 (s1, Op1 (s2, Op1 (s3, Term)))) do
        let jGuess5 id =
            jval
                [
                    "id", jval id
                    "program", jval ("(lambda (x) "+s+")")
                ]
        let id = "Rbny9NZuQE1URWimmvz966HP"
        let q5 = jGuess5 id |> FsJson.serialize |> Guess
        let s5 = doQuery q5
        let r5 = FsJson.parse s5
        if r5?status.Val = "win" then
            printfn "win: %s %s" id s
            failwith "Ok"
        else
            printfn "fig: %s %s" r5?status.Val s
            System.Console.Read() |> ignore

//- 4dC9n4EECTLg7OCJyjxOvhhs
//84rOkDe5Abx0BSlaqgLzMtSB (shl1 (plus x 0))
//AsKbAbnWl5fuLV7zDBtS9hcH (xor x (shl1 x)
//- B8faDsWMJ1oBAghUaD73rzp6
//Ecrse8Txws5VkVMBYsdcjhSs (not (xor x 0))
//KeBuniWZ5vyiAlWl6AipvdL8 (or x (not 1))
//Niv851IGG0wNbVOCrccaAiHB (shl1 (or x x))
//R3o7f3FJbNq990Qahll0QZyU (shr4 (plus x 0))
