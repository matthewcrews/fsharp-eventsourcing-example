open System
open OrderDomain

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let outputDir = @".\db"
    let eventReader = EventDB.read outputDir
    let eventWriter = EventDB.write outputDir
    let orderReader = Order.composeOrderSource eventReader
    let cmdHandler = Commands.composeCommandHandler orderReader eventWriter
    
    let orderId = Guid.Parse "5687C1B2-ACA5-447B-A29A-CD174B2900CB"

    //let createCmd = Commands.Create {
    //    Id = orderId
    //}
    //let result = cmdHandler createCmd

    let addLineCmd = Commands.AddLine {
        Id = orderId
        LineId = 5
        ItemId = "chicken"
        Amount = 5M
    }
    let result = cmdHandler addLineCmd

    //let removeLineCmd = Commands.RemoveLine {
    //    Id = orderId
    //    LineId = 10
    //}
    //let result = cmdHandler removeLineCmd


    printfn "%A" result

    let result = orderReader (string orderId)
    printfn "%A" result

    0 // return an integer exit code
