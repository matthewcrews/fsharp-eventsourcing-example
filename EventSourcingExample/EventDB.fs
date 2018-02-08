module EventDB

open System.IO



module Json = 
    open Newtonsoft.Json 
    let serialize obj = 
        JsonConvert.SerializeObject(obj, Formatting.Indented)
    let deserialize<'a> str = 
        JsonConvert.DeserializeObject<'a> str

let read<'a> dir topic : 'a array =
    let file = Path.Combine(dir, topic + ".json")
    let text = File.ReadAllText(file)
    Json.deserialize<'a array>(text)

let readAfter<'a> dir topic (i : int) =
    (read<'a> dir topic).[i..]
        
let write<'a> dir topic e =
    let writeEvents file events =
        let data = Json.serialize events
        File.WriteAllText(file, data)

    if not(Directory.Exists(dir)) then
        Directory.CreateDirectory(dir) |> ignore

    let file = Path.Combine(dir, topic + ".json")

    if not(File.Exists(file)) then
        [|e|]
        |> writeEvents file
    else
        let events = read<'a> dir topic
        Array.append events [|e|]
        |> writeEvents file
