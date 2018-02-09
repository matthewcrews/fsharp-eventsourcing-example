module OrderDomain

open System

type OrderLine = {
    LineId : int
    ItemId : string
    Amount : decimal
}

type Order = {
    Id : Guid
    Lines : OrderLine list
}

let init = {
    Id = Guid.Parse "00000000-0000-0000-0000-000000000000"
    Lines = []
}

module Events =
    type Created = {
        Id : Guid
    }

    type LineAdded = {
        Id : Guid
        LineId : int
        ItemId : string
        Amount : decimal
    }

    type LineRemoved = {
        Id : Guid
        LineId : int
    }

    type Event =
    | Created of Created
    | LineAdded of LineAdded
    | LineRemoved of LineRemoved

    let private create (order : Order) (c : Created) =
        {order with Id = c.Id}

    let private addLine (order : Order) (a : LineAdded) =
        let newLines = order.Lines @ [{LineId = a.LineId; ItemId = a.ItemId; Amount = a.Amount}]
        {order with Lines = newLines}

    let private removeLine (order : Order) (r : LineRemoved) =
        let newLines = order.Lines |> List.where (fun l -> l.LineId <> r.LineId)
        {order with Lines = newLines}

    let applyEvent (order : Order) (e : Event) =
        match e with
        | Created c -> create order c
        | LineAdded a -> addLine order a
        | LineRemoved r -> removeLine order r

type EventSource = string -> array<Events.Event> option
type OrderSource = string -> Order option
type EventAdder = string -> Events.Event -> unit

let orderSourceComposer (eventSource : EventSource) =
    fun (id : string) ->
        eventSource id
        |> Option.map (Array.fold Events.applyEvent init)


module Commands =
    open Events

    type private Create = Events.Created
    type private AddLine = Events.LineAdded
    type private RemoveLine = Events.LineRemoved

    type Command =
        | Create of Create
        | AddLine of AddLine
        | RemoveLine of RemoveLine
        with
            member x.Id = 
                match x with
                | Create {Id = id} -> id
                | AddLine {Id = id} -> id
                | RemoveLine {Id = id} -> id

    type CommandHandler = Command -> Result<string, string>

    let private ensureOrder (orderSource: OrderSource) (cmd: Command) = 
        match (orderSource (string cmd.Id)) with
        | Some order -> Result.Ok order
        | None -> Result.Error "Order already exists"


    let private addLine (eventAdder : EventAdder) (newLine : AddLine) order =
        match not(order.Lines |> List.exists (fun l -> l.LineId = newLine.LineId)) with
        | true ->
            eventAdder (string newLine.Id) (Event.LineAdded newLine)
            Result.Ok "Line Added"
        | false ->
            Result.Error "Line already exists"

    let private create (eventAdder : EventAdder) (createOrder : Create) =
        eventAdder (string createOrder.Id) (Event.Created createOrder)
        Result.Ok "Order created"

    let private removeLine (eventAdder : EventAdder) (remLine : RemoveLine) order =
        match (order.Lines |> List.exists (fun l -> l.LineId = remLine.LineId)) with
        | true ->
            eventAdder (string remLine.Id) (Event.LineRemoved remLine)
            Result.Ok "Line removed"
        | false ->
            Result.Error "Line does not exist"

    let composeCommandHandler orderSource eventAdder =
        fun (cmd : Command) ->
            let order = orderSource (string cmd.Id)
            match cmd, order with
            | Command.Create c, None -> create eventAdder c
            | Command.Create c, Some o -> Result.Error "Order already exists"
            | _, None -> Result.Error "Order does not exist"
            | Command.AddLine l, Some o -> addLine eventAdder l o
            | Command.RemoveLine l, Some o -> removeLine eventAdder l o

