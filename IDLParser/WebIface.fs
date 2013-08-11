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

let t1 = Op1 ("shr16", Op2 ("plus",Term,Term))
let t2 = Op2 ("plus", Term, Op1 ("shr16", Term))

//Array.ofSeq (doit t1);;
for t in [t1;t2] do
    for s in doit t do
        f6 id s

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

for s1::s2::s3::[] in permute ["not";"shr1";"shr4"] do
    for s in doit (Op1 (s1, Op1 (s2, Op1 (s3, Term)))) do
        f6 id s

//- 4dC9n4EECTLg7OCJyjxOvhhs
//84rOkDe5Abx0BSlaqgLzMtSB (shl1 (plus x 0))
//AsKbAbnWl5fuLV7zDBtS9hcH (xor x (shl1 x)
//- B8faDsWMJ1oBAghUaD73rzp6
//Ecrse8Txws5VkVMBYsdcjhSs (not (xor x 0))
//KeBuniWZ5vyiAlWl6AipvdL8 (or x (not 1))
//Niv851IGG0wNbVOCrccaAiHB (shl1 (or x x))
//R3o7f3FJbNq990Qahll0QZyU (shr4 (plus x 0))
//Rbny9NZuQE1URWimmvz966HP (not (not (shr4 x)))
//VToCZrqsE8atq5Wj8h0GcIB1 (shr16 (xor x 0))
//WJGNYtJJIy5hMJzBAMYvRsmR (shr16 (or x x))
//XHYzh5JITpl0EiZSEn6jFLLV (shr1 (xor x 0))
//ab0o5Xb8vBAxvW8oHo6TiZYB (and x (shr1 x))
//bQPi5crb5Nk1CFt7cASXkdBi (and x (shl1 x))
//iB8X0uWMUZNZ9p3Q6rmmQ4Yq (xor x (shr1 x))
//o59jAxIwIIAg4ZlslyOn5IFg (shr1 (shr1 (shr16 x)))
//of91shLnVpykAimwBPm465QO (or x (shl1 x))
//uhTEPh0RnIUcaWjHwB4Ktnkg (not (shr16 (shr4 x)))
//wpGphG4OoL5SMaz9JQf8ZItB (shr1 (shr4 (not x)))
//zUHt7zhKKVP0CLpLGrz85Ivl (plus x (shr16 x))


let f6 id s =
    let start = System.DateTime.Now
    let jGuess6 =
        jval
            [
                "id", jval id
                "program", jval ("(lambda (x) "+s+")")
            ]
    let q6 = jGuess6 |> FsJson.serialize |> Guess
    let s6 = doQuery q6
    let r6 = FsJson.parse s6
    match r6?status.Val with
        |"error" -> printfn "%s: %s: %s" s r6?status.Val r6?message.Val
        |"mismatch" ->
            let arr = r6?values.Array
            printfn "%s: %s: inp: %s res: %s guess: %s" s r6?status.Val arr.[0].Val arr.[1].Val arr.[2].Val
        |"win" ->
            printfn "%s %s" id s
            failwith "Ok"
    let stop = System.DateTime.Now
    let ts = stop-start
    if ts.Seconds < 4 then
        System.Threading.Thread.Sleep(4000-ts.Seconds*1000-ts.Milliseconds)

let id = "yseEUyFdcWnlfKKlaVsYE9FO"
for s1::s2::[] in permute ["shl1";"shl1"] do
    for s in doit (Op1 (s1, Op1 (s2, Op2 ("xor", Term, Term)))) do
        f6 id s
    for s in doit (Op1 (s1, Op2 ("xor", Term, Op1 (s2, Term)))) do
        f6 id s
    for s in doit (Op2 ("xor", Term, Op1 (s1, Op1 (s2, Term)))) do
        f6 id s
    for s in doit (Op2 ("xor", Op1 (s1, Term), Op1 (s2, Term))) do
        f6 id s

let id1 = "yseEUyFdcWnlfKKlaVsYE9FO"
for s1::s2::[] in permute ["xor";"xor"] do
    for s in doit (Op2 (s1, Op2 (s2, Term, Term), Term)) do
        f6 id1 s
    for s in doit (Op2 (s1, Op2 (s1, Term, Term), Term)) do
        f6 id1 s
    for s in doit (Op2 (s2, Op2 (s2, Term, Term), Term)) do
        f6 id1 s

//3YNeaANpWGjF87HQWOziTJnR (xor x (shl1 (shr1 x)))
//- CdJCrj2h4yJfSVo59IgQHliz
//DpFhvv8EG78tGWJNs6uYNmOV (shr4 (shr16 (or x x)))
//EaokkhLsXRA3a4Lt9EpFcJl5 (and x (shr4 (shr4 x)))
//I1KgUA35NO1nLsgp2y6Phr9V (xor (or x 1) x)
//- ? {"id":"RCvFDBNCAmQsu0ORB9Ag3j9Y","size":6,"operators":["if0","shl1"]}
//UTNV4Uiq05XDGNmDAivkAjZw (and (plus x x) x)
//- WlBy7dYGx0DBTiS5vnWDSfDk !!!error ???
//Xq3zXxdskhZ84X0hMSQohBWn (plus x (shr16 (not x)))
//c9vwZZXbNDLzpOsDusshFVNj (shr4 (shr16 (and x x)))
//- cY4oBq5QDvbV5UzRMVN1eqFx
//f4M3hPK3SqPlQ13H81oAAw00 (and (xor x 1) x)
//fo8CuBeDy4hAA12dRZzSRJ2z (and x (shr16 (not x)))
//id2oSNKz7xyRRDatqm9AUd0c (and (or x x) 1)
//{"id":"ki85WBfrBh8AvFM04sjBW6Yq","size":6,"operators":["if0","shr16"]} (if0 x (shr16 (shr16 x)) (shr16 x))
//l0KCRh2ic9PTweRJT6SOL3Q6 (shl1 (not (plus x x)))
//mf8YVn9MN0TxwjYGvbgrLkCz (shr16 (shr16 (and x x)))
//io3OUmCKBfOy7wUZDrQ4tRv (shr4 (not (or x x)))
//y4NQWBMCG4tDyFGDAteuvnj6 (plus (xor x 1) 0)
//- yseEUyFdcWnlfKKlaVsYE9FO

let id1 = "ki85WBfrBh8AvFM04sjBW6Yq"
let s = "shr16"
for s1' in doit (Op1 (s, (Op1 (s, (Op1 (s, Term)))))) do
    for s2' in doit Term do
        for s3' in doit Term do
            for s1::s2::s3::[] in permute [s1';s2';s3'] do
                f6 id1 (sprintf "(if0 %s %s %s)" s1 s2 s3)

for s1' in doit (Op1 (s, (Op1 (s, Term)))) do
    for s2' in doit (Op1 (s, Term)) do
        for s3' in doit Term do
            for s1::s2::s3::[] in permute [s1';s2';s3'] do
                f6 id1 (sprintf "(if0 %s %s %s)" s1 s2 s3)

for s1' in doit (Op1 (s, Term)) do
    for s2' in doit (Op1 (s, Term)) do
        for s3' in doit (Op1 (s, Term)) do
            for s1::s2::s3::[] in permute [s1';s2';s3'] do
                f6 id1 (sprintf "(if0 %s %s %s)" s1 s2 s3)

let combine cost ops = 
    let op_cost = function
    |Op1(_,_) -> 1
    |Op2(_,_,_) -> 2
    |node -> failwith (sprintf "unknown op %A" node)

    let rec updater tree node =
        seq {
            match tree, node with
            |Op1(s1,tree'), Op1(s2,_) ->
                if (s1 <> s2) then
                    yield Op1(s1,Op1(s2,tree'))
                    yield Op1(s2,Op1(s1,tree'))
                else
                    yield Op1(s1,Op1(s1,tree'))
                for t in updater tree' node do
                    yield Op1(s1,t)
            |Op1(s1,tree'), Op2(s2,_,_) ->
                yield Op1(s1,Op2(s2,Term,tree'))
                yield Op2(s2,Term,Op1(s1,tree'))
                for t in updater tree' node do
                    yield Op1(s1, t)
            |Op2(s1,tree1,tree2), Op1(s2,_) ->
                yield Op2(s1,tree1,Op1(s2,tree2))
                yield Op2(s1,Op1(s2,tree1),tree2)
                for t in updater tree1 node do
                    yield Op2(s1, t, tree2)
                for t in updater tree2 node do
                    yield Op2(s1, tree1, t)
            |Op2(s1,tree1,tree2), Op2(s2,_,_) ->
                if (s1 <> s2) then
                    yield Op2(s1,tree1,Op2(s2,Term,tree2))
                    yield Op2(s1,Op2(s2,Term,tree1),tree2)
                    yield Op2(s2,tree1,Op2(s1,Term,tree2))
                    yield Op2(s2,Op2(s1,Term,tree1),tree2)
                else
                    yield Op2(s1,tree1,Op2(s1,Term,tree2))
                    yield Op2(s1,Op2(s1,Term,tree1),tree2)
                for t in updater tree1 node do
                    yield Op2(s1, t, tree2)
                for t in updater tree2 node do
                    yield Op2(s1, tree1, t)
            |Term,Op1(s2,_) -> yield Op1(s2,Term)
            |Term,Op2(s2,_,_) -> yield Op2(s2,Term,Term)
            |node1, node2 -> failwith (sprintf "unknown nodes %A,%A" node1 node2)
        }

    let rec comb cost acc unused_ops = 
        seq {
            if cost = 0 && unused_ops = [] then
                yield acc
            elif cost > 0 then
                for op in ops do
                    let unused_ops' = (List.filter ((<>) op) unused_ops)
                    for acc' in updater acc op do
                        for t in comb (cost - op_cost op) acc' unused_ops' do
                            yield t
        }

    let rec count_cost = function
    |Term -> 1
    |Op1 (_,t) -> 1 + count_cost t
    |Op2 (_,t1,t2) -> 1 + count_cost t1 + count_cost t2
    (*
    seq {
        for ops' in permute ops do
            for tlist in comb cost [] ops' ops do
                for t in tlist do
                    if cost = count_cost t then
                        yield t
    }
    *)
    seq {
        for op in ops do
            for t in comb (cost - op_cost op) op (List.filter ((<>) op) ops) do
                yield t
    }

let f7 id s rtests =
    let start = System.DateTime.Now
    let jGuess6 =
        jval
            [
                "id", jval id
                "program", jval ("(lambda (x) "+s+")")
            ]
    let q6 = jGuess6 |> FsJson.serialize |> Guess
    let s6 = doQuery q6
    let r6 = FsJson.parse s6
    match r6?status.Val with
        |"error" -> printfn "%s: %s: %s" s r6?status.Val r6?message.Val
        |"mismatch" ->
            let arr = r6?values.Array
            printfn "%s: %s: inp: %s res: %s guess: %s" s r6?status.Val arr.[0].Val arr.[1].Val arr.[2].Val
            let s1 = arr.[0].Val.[2..]
            let s2 = arr.[1].Val.[2..]
            let inp = System.Int64.Parse (s1,  System.Globalization.NumberStyles.AllowHexSpecifier)
            let res = System.Int64.Parse (s2,  System.Globalization.NumberStyles.AllowHexSpecifier)
            rtests := (uint64(inp),uint64(res))::!rtests
        |"win" ->
            printfn "%s %s" id s
            failwith "Ok"
    let stop = System.DateTime.Now
    let ts = stop-start
    if ts.Seconds < 4 then
        System.Threading.Thread.Sleep(4000-ts.Seconds*1000-ts.Milliseconds)

let id = "y7yLqafiW4qK3bNW4Wn1m1Fe"
let mutable set = Set.empty
//for t in combine (7-2) [Op1("shr16",Term); Op2("and",Term,Term)] do
for t in combine (7-2) [Op2("plus",Term,Term); Op1("shr16",Term); Op2("xor",Term,Term)] do
//for t in combine (7-2) [Op2("or",Term,Term); Op1("shl1",Term); Op1("shr1",Term); Op1("shr16",Term)] do
    set <- set.Add t
let rtests = ref []

let rec should_test s = function
    |[] -> true
    |(inp,res)::xs ->
        if res = test inp prog s then
            should_test s xs
        else
            false

for t in set do
    for s in doit t do
        let str = sprintf "(lambda (x) %s)" s
        if should_test str !rtests then
            f7 id s rtests

// 7:
//- 0WyiqPvvM3GUulouL4YXtbRM
//- 660XubzPVwPRzvLYVncwQ0Zq
//6KrqF5VK5MvOfSVcMkmAmPQw (and x (xor x (shl1 x)))
//? {"id":"BV6C7W6ABZvPHthJFBVJGyIA","size":7,"operators":["and","if0"]}
//BlxGrKZrmVv7pFeNwJ9Zz39M (plus x (plus x (shr1 0)))
//CpygXjUcA8UBcy9RVpKhva29 (or x (xor 1 (shr4 0)))
//EgYbCyGd5AqTqxTvIjUI0KcX (plus x (shl1 (plus 1 1)))
//OlebLM58BAvx42PBQ2VwoNBh (and (not (shr16 x)) (shl1 1))
//- ? {"id":"Ra2BJZPAGifIVIDi98zziZKQ","size":7,"operators":["and","shl1","shr16"]}
//TIcydjZRBOuD7WRl9EA3KljQ (and x (not (plus 0 1)))
//V1q5g5y24huc7HGFZRlW1EQY (not (shl1 (or 1 (shl1 x))))
//haPKQBYchjO72Hly5cCkbFZz (and x (xor 1 (shr4 x)))
//jPgBWXlsz3NVzvHxSBp0a83h (or x (shl1 (shr1 (shr16 x))))
//kUY57Anksh4jZrymgOmDIc8T (xor x (plus x (shl1 x)))
//nk3XthgliSUeEulAQJeA5abe (and 1 (not (plus x 0)))
//rRgW96xqTW3Lk9Hl7RmVbe1T (plus (shr1 0) (xor x 1))
//vMEGutoARyg1wPm6O9ybTJXI (not (plus x (xor 0 1)))
//xWYTHBKBg1S7UWcFs8pZTRHH (plus 1 (shr16 (and x x)))
//xj26dwzLrNAjRXC0LDrjv2Ig (and x (shr16 (and x x)))
//- ? {"id":"y7yLqafiW4qK3bNW4Wn1m1Fe","size":7,"operators":["plus","shr16","xor"]}

let ops1 = [|"not";"shl1";"shr1";"shr4";"shr16"|]
let ops2 = [|"and";"or";"xor";"plus"|]

let filter = function
    |JsonString s when None <> Array.tryFind (fun elem -> s = elem) ops -> true
    |_ -> false

let json2op = function
    |JsonString s ->
        if None <> Array.tryFind (fun elem -> s = elem) ops1 then
            Op1(s,Term)
        else
            Op2(s,Term,Term)
    |_ -> failwith "unknown op"

for i in 9..30 do
    for v in j.Array do
        if v?size.Val = (string i) && Array.forall filter v?operators.Array then
            //printfn "%s: %A" v?id.Val v?operators.Array
            let id = v?id.Val
            let ops = Array.map json2op v?operators.Array |> Array.toList
            printfn "%d: %s: %A" i id ops
            let mutable set = Set.empty
            for t in combine (i-2) ops do
                set <- set.Add t
            let rtests = ref []

            try
                for t in set do
                    for s in doit t do
                        let str = sprintf "(lambda (x) %s)" s
                        if should_test str !rtests then
                            f7 id s rtests
            with
            | ex -> printfn "%s" (ex.ToString())
            printfn "done for id %s" id
            System.Console.Read() |> ignore

//8 :
//9ZKAi6pnA3vceBgzckpRkB4P (and 1 (and 1 (xor x 1)))
//B1355vkPjIhFCVBifCPl9P1r (or 0 (or 0 (plus x 1)))
//BXG0ND4ZFcKy3qS6kfVMRiDP (or (shr4 0) (xor 1 (shr1 x)))
//DAAOQhEF32AZYecmmCBBNCmP (xor x (shl1 (shr4 (plus x x))))
//EoZypGj6TdmqFerSykR0Cw5w (shr16 (plus 1 (shr4 (xor x 0))))
//P0bwRupnbk72Xe4EZI0yg7LZ (not (shl1 (or x (plus x 0))))
//POhtjNU2iXEgeaNd1qcJ3SQ4 (and 1 (shr16 (shr4 (plus x 0))))
//Y18FWmFujq3jlKx7Qp4mn6AA (shr1 (shr16 (shr4 (not (plus x 0)))))
//YMYJSuOsD52E17gMfFDrp88V (or 0 (plus x (xor x 0)))
//aQ4Mw3aGqXQvech6TOAYcCsY (or 0 (plus x (or x 1)))
//dvVuVBa3Pr8AK1A5O73LxMXp (not (not (not (shl1 (or x 1)))))
//kWjVEyghWModURc1Tte2KOi6 (xor x (shr4 (xor x (shl1 0))))
//m4VNzGCvGRPvHl3mT9XViPrk (and x (or (shr1 x) (shr4 x)))
//mfwjvGRBgRO02KHiDMeABiB1 (shr16 (shl1 (or x (plus x 0))))
//pV4YAz7TNztAVIlZkoJgM9rN (plus (shr1 (shr16 x)) (shr4 (shl1 x)))
//uKPd0mTSBDzpEJqRGmC7cDQD (or 0 (plus x (and x 1)))
//xXG8fJwTym5AiroB1XjAisJY (not (plus (shr1 0) (xor x 1)))
//yy3BvsRZgjFFTplQCrC24p1J (or (shr1 0) (xor x (shr16 x)))
