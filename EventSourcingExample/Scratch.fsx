
// Anthony Shull - help understanding bind
// Chet Husk - code refactoring and using bind

open System

type EventSource = string -> array<Events.Event> option
type OrderSource = string -> Order option
type EventAdder = string -> Events.Event -> unit

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

let ensureOrder (orderSource: OrderSource) (event: Events.Event) = 
    match orderSource (string event.Id) with
    | None -> Result.Error "Order already exists"
    | Some order -> Result.Ok order


let addLine (eventAdder : EventAdder) (({ Id = id }: AddLine) as c) order =
    match not(order.Lines |> List.exists (fun l -> l.LineId = c.LineId)) with
    | true ->
        eventAdder (string id) (Event.LineAdded c)
        Result.Ok "Line Added"
    | false ->
        Result.Error "Line already exists"

// so now all your domain handling code validates the order finding first with 'ensureOrder', then passes that order into the event given

let handleEvent orderSource eventAdder event = 
    ensureOrder orderSource event
    |> Result.bind (fun order -> 
        match event with
        | Events.Created c -> create eventAdder c
        | Events.LineAdded al -> addLine eventAdder al order
        | Events.LineRemoved rl -> removeLine eventAdder rl order
    )