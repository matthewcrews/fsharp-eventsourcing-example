
// Anthony Shull - help understanding bind
// Chet Husk - code refactoring and using bind

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
    with 
        member x.Id = 
            match x with 
            | Created { Id = id } -> id
            | LineAdded { Id = id } -> id
            | LineRemoved { Id = id } -> id

type EventSource = string -> array<Events.Event> option
type OrderSource = string -> Order option
type EventAdder = string -> Events.Event -> unit

type Create = Events.Created
type AddLine = Events.LineAdded
type RemoveLine = Events.LineRemoved

open Events

let ensureOrder (orderSource: OrderSource) (event: Events.Event) = 
    match (orderSource (string event.Id)) with
    | Some order -> Result.Ok order
    | None -> Result.Error "Order already exists"


let addLine (eventAdder : EventAdder) (newLine : AddLine) order =
    match not(order.Lines |> List.exists (fun l -> l.LineId = newLine.LineId)) with
    | true ->
        eventAdder (string newLine.Id) (Event.LineAdded newLine)
        Result.Ok "Line Added"
    | false ->
        Result.Error "Line already exists"

let create (eventAdder : EventAdder) (createOrder : Create) =
    eventAdder (string createOrder.Id) (Event.Created createOrder)
    Result.Ok "Order created"

let removeLine (eventAdder : EventAdder) (remLine : RemoveLine) order =
    match (order.Lines |> List.exists (fun l -> l.LineId = remLine.LineId)) with
    | true ->
        eventAdder (string remLine.Id) (Event.LineRemoved remLine)
        Result.Ok "Line removed"
    | false ->
        Result.Error "Line does not exist"

// so now all your domain handling code validates the order finding first with 'ensureOrder', then passes that order into the event given

let handleEvent orderSource eventAdder event = 
    ensureOrder orderSource event
    |> Result.bind (fun order -> 
        match event with
        | Events.Created c -> create eventAdder c
        | Events.LineAdded al -> addLine eventAdder al order
        | Events.LineRemoved rl -> removeLine eventAdder rl order
    )