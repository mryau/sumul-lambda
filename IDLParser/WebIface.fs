module WebIface

open System.Text
open System.IO
open System.Net

type Query =
    |MyProblems
    |Train of string
    |Eval of string
    |Guess of string

let doQuery q = 
    let secret = "0110dZ2MDEqFSPIfHvznfra1A9o6WtCZ8XL0meWk" + "vpsH1H"
    let host = "http://icfpc2013.cloudapp.net"
    let path, arg =
        match q with
        |MyProblems -> "/myproblems", "" 
        |Train s -> "/train", s
        |Eval s -> "/eval", s
        |Guess s -> "/guess", s
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
let s1 = doQuery MyProblems

let jTrain =
    jval
        [
            "size", jval 3
            "operators", jval []
        ]

let q2 = jTrain |> FsJson.serialize |> Train
let s2 = doQuery q

let jGuess =
    jval
        [
            "id", jval "faQBBWtilWzX36kSVc0Ps9W6"
            "program", jval "(lambda (x) (shr4 x))"
        ]
let q3 = jGuess |> FsJson.serialize |> Guess
let s3 = doQuery q3
