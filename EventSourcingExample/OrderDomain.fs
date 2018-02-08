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

    let map (order : Order) (e : Event) =
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
        |> Option.map (Array.fold Events.map init)

module Commands =
    open Events

    type Create = Events.Created
    type AddLine = Events.LineAdded
    type RemoveLine = Events.LineRemoved
    type Command =
        | Create of Create
        | AddLine of AddLine
        | RemoveLine of RemoveLine

    let create (orderSource : OrderSource) (eventAdder : EventAdder) (c : Create) =
        let id = c.Id |> string
        let order = orderSource id
        match order with
        | Some _ ->
            Result.Error "Order already exists"
        | None ->
            let e = Event.Created c
            eventAdder id e
            Result.Ok "Order created"

    let addLine (orderSource : OrderSource) (eventAdder : EventAdder) (c : AddLine) =
        let id = c.Id |> string
        let order = orderSource id
        match order with
        | Some order ->
            match not(order.Lines |> List.exists (fun l -> l.LineId = c.LineId)) with
            | true ->
                eventAdder id (Event.LineAdded c)
                Result.Ok "Line Added"
            | false ->
                Result.Error "Line already exists"
        | None ->
            Result.Error "Order does not exist"

    let removeLine (orderSource : OrderSource) (eventAdder : EventAdder) (c : RemoveLine) =
        let id = c.Id |> string
        let order = orderSource id
        match order with
        | Some o ->
            match o.Lines |> List.exists (fun l -> l.LineId = c.LineId) with
            | true ->
                eventAdder id (Event.LineRemoved c)
                Result.Ok "Line Added"
            | false ->
                Result.Error "Line does not exist"
        | None ->
            Result.Error "Order does not exist"


    //type private ExistsHandler = Order -> Result<string, string>
    //type private NotExistHandler = unit -> Result<string, string>

    //let private commandHandler (orderSource : OrderSource) (id : String) (exists : ExistsHandler) (notExist : NotExistHandler) =
    //    let order = orderSource id
    //    match order with
    //    | Some o ->
    //        exists o
    //    | None ->
    //        notExist ()

    //let create (orderSource : OrderSource) (eventAdder : EventAdder) (c : Create) =
    //    let exists (order : Order) =
    //        Result.Error "Order already exists"
    //    let notExist () =
    //        let e = Event.Created c
    //        eventAdder (c.Id |> string) e
    //        Result.Ok "Order created"
    //    commandHandler orderSource (c.Id |> string) exists notExist