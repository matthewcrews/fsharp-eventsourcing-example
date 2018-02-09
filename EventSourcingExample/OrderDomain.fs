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
        with 
        member x.Id = 
            match x with 
            | Created { Id = id } -> id
            | LineAdded { Id = id } -> id
            | LineRemoved { Id = id } -> id

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


type EventSource = string -> Events.Event array option
type OrderSource = string -> Order option
type EventWriter = string -> Events.Event -> unit
 
module Order =
    let private init = {
        Id = Guid.Parse "00000000-0000-0000-0000-000000000000"
        Lines = []
    }

    let composeOrderSource (eventSource : EventSource) : OrderSource =
        fun (id : string) ->
            eventSource id
            |> Option.map (Array.fold Events.applyEvent init)


module Commands =
    open Events

    type Command =
    | Create of Created
    | AddLine of LineAdded
    | RemoveLine of LineRemoved
        with 
        member x.Id = 
            match x with 
            | Create { Id = id } -> id
            | AddLine { Id = id } -> id
            | RemoveLine { Id = id } -> id

    type CommandHandler = Command -> Result<string, string>

    let private create (writer : EventWriter) (c : Created) =
        writer (string c.Id) (Event.Created c)
        Result.Ok "Order Added"

    let private addLine (writer : EventWriter) (order : Order) (a : LineAdded) =
        if not(order.Lines |> List.exists (fun l -> l.LineId = a.LineId)) then
            writer (string a.Id) (Event.LineAdded a)
            Result.Ok "Line Added"
        else
            Result.Error "Line already exists"

    let private removeLine (writer : EventWriter) (order : Order) (r : LineRemoved) =
        if order.Lines |> List.exists (fun l -> l.LineId = r.LineId) then
            writer (string r.Id) (Event.LineRemoved r)
            Result.Ok "Line Removed"
        else
            Result.Error "Line does not exist"

    let composeCommandHandler (orderSource : OrderSource) (writer : EventWriter) : CommandHandler =
        fun (cmd : Command) ->
            let order = orderSource (string cmd.Id)
            match cmd, order with
            | Create c, None -> create writer c
            | Create c, Some o -> Result.Error "Order already exists"
            | _, None -> Result.Error "Order does not exist"
            | AddLine l, Some o -> addLine writer o l
            | RemoveLine l, Some o -> removeLine writer o l
            

    



