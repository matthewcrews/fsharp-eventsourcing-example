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

    type ApplyCreated = Order -> Created -> Order
    let applyCreated (order : Order) (c : Created) =
        {order with Id = c.Id}

    type ApplyLineAdded = Order -> LineAdded -> Order
    let applyLineAdded (order : Order) (a : LineAdded) =
        let newLines = order.Lines @ [{LineId = a.LineId; ItemId = a.ItemId; Amount = a.Amount}]
        {order with Lines = newLines}

    type ApplyLineRemoved = Order -> LineRemoved -> Order
    let applyLineRemoved (order : Order) (r : LineRemoved) =
        let newLines = order.Lines |> List.where (fun l -> l.LineId <> r.LineId)
        {order with Lines = newLines}

    type EventApplyer = Order -> Event -> Order
    let composeEventApplyer (created : ApplyCreated) (added : ApplyLineAdded) (removed : ApplyLineRemoved) =
        fun (order : Order) (e : Event) ->
            match e with
            | Created c -> created order c
            | LineAdded a -> added order a
            | LineRemoved r -> removed order r


    type EventSource = string -> Event array option
    type OrderSource = string -> Order option
    type EventWriter = string -> Event -> unit
 


    let composeOrderSource (apply : EventApplyer)  (eventSource : EventSource) : OrderSource =
        let init = {
            Id = Guid.Parse "00000000-0000-0000-0000-000000000000"
            Lines = []
        }
        fun (id : string) ->
            eventSource id
            |> Option.map (Array.fold apply init)


module Commands =

    // I am recycling the definitions from the Events module because
    // the data is the same
    type Create = Events.Created
    type AddLine = Events.LineAdded
    type RemoveLine = Events.LineRemoved

    type Command =
    | Create of Create
    | AddLine of AddLine
    | RemoveLine of RemoveLine
        with 
        member x.Id = 
            match x with 
            | Create { Id = id } -> id
            | AddLine { Id = id } -> id
            | RemoveLine { Id = id } -> id

    type ErrorMessage = string
    type CommandResult = Result<string, ErrorMessage>
    
    type OrderNotExists = Create * Order option -> Result<Create, ErrorMessage>

    type OrderExists<'a, 'b> = 'a * 'b option -> Result<'a * 'b, ErrorMessage>


    type LineExists = RemoveLine * Order -> Result<Command * Order, ErrorMessage>

    type LineNotExist = AddLine * Order -> Result<Command * Order, ErrorMessage>


    type ApplyCommand = Command -> Result<string, ErrorMessage>
    type TestCreate = Create -> Result<Create, ErrorMessage>

    type ApplyCreate = Create -> CommandResult
    let composeApplyCreate (testCreate : TestCreate) (writer : Events.EventWriter) : ApplyCreate =
        fun (create : Create) ->
            
    type ApplyAddLine = AddLine -> CommandResult
    type ApplyRemoveLine = RemoveLine -> CommandResult

    let composeApplyCommand (ac : ApplyCreate) (ala : ApplyAddLine) (alr : ApplyRemoveLine) =
        fun (cmd : Command) ->
            match cmd with
            | Command.Create cr -> ac cr
            | Command.AddLine al -> ala al
            | Command.RemoveLine rl -> alr rl

    //type CheckCommand = Order option -> Command -> Result<Command, ErrorMessage>
    //let checkCommand (order : Order option) (cmd : Command) : Result<Command, ErrorMessage> =
    //        let getCommand x = x |> Result.map (fun y -> fst y)
    //        let orderNotExists (cmd, order) =
    //            match order with
    //            | None -> Result.Ok cmd
    //            | Some _ -> Result.Error "Order exists"
    //        let orderExists (cmd, order) =
    //            match order with
    //            | Some o -> Result.Ok (cmd, o)
    //            | None -> Result.Error "Order does not exist"
    //        let lineNotExists (cmd : LineAdded, order) =
    //            if order.Lines |> List.exists (fun l -> l.LineId = cmd.LineId) |> not then
    //                Result.Ok (cmd, order)
    //            else
    //                Result.Error "Line already exists"
    //    let lineExists (cmd, order) =
    //        if order.Lines |> List.exists (fun l -> l.LineId = cmd.LineId) then
    //            Result.Ok (cmd, order)
    //        else
    //            Result.Error "Line does not exist"
            
    //        match cmd with
    //        | Create c -> orderNotExists (c, order) |> Result.map
    //        | AddLine a -> orderExists (cmd, order) |> Result.bind lineNotExists
    //        | RemoveLine r -> orderExists (cmd, order) |> Result.bind lineExists
            

    



