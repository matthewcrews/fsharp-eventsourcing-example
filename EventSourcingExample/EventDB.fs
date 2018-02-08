module EventDB

open System.IO

module Json = 
    open Newtonsoft.Json 
    let serialize obj = 
        JsonConvert.SerializeObject(obj, Formatting.Indented)
    let deserialize<'a> str = 
        JsonConvert.DeserializeObject<'a> str

let read<'a> dir topic : 'a array option =
    let file = Path.Combine(dir, topic + ".json")
    if File.Exists(file) then
        let text = File.ReadAllText(file)
        Json.deserialize<'a array>(text) |> Some
    else
        None

let readAfter<'a> dir topic (i : int) =
    (read<'a> dir topic)
    |> Option.map (fun a -> a.[i..])
        
let write<'a> dir topic e =
    let writeEvents file events =
        let data = Json.serialize events
        File.WriteAllText(file, data)

    if not(Directory.Exists(dir)) then
        Directory.CreateDirectory(dir) |> ignore

    let file = Path.Combine(dir, topic + ".json")

    let events = read<'a> dir topic

    match events with
    | Some es ->
        Array.append es [|e|]
        |> writeEvents file
    | None ->
        [|e|]
        |> writeEvents file

