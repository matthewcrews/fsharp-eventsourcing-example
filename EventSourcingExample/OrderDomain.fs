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

    type CommandResult = Result<string, string>
    type ErrorMessage = string
    
    type OrderNotExists = Command * Order option -> Result<Command, ErrorMessage>
    type OrderExists = Command * Order option -> Result<Command * Order, ErrorMessage>
    type LineExists = Command * Order -> Result<Command * Order, ErrorMessage>
    type LineNotExist = Command * Order -> Result<Command * Order, ErrorMessage>

    //type CheckCreate = Order option -> Created -> Result<Created, ErrorMessage>
    //let composeCheckCreate (check : OrderExistsTest) : CheckCreate =
    //    fun (order : Order option) (created : Created) ->
    //        match check order with
    //        | Result.Ok _ -> Result.Ok created
    //        | Result.Error err -> Result.Error err

    //type CheckAddLine = Order option -> LineAdded -> Result<LineAdded, ErrorMessage>
    //let composeCheckAddLine (testExist : OrderExistsTest) (testAdd : LineDoesNotExistTest) =
    //    fun (order : Order option) (add : LineAdded) ->
    //        match testExist order |> Result.bind testAdd with
    //        | Result.Ok _ -> Result.Ok add
    //        | Result.Error err -> Result.Error err

    //type CheckRemoveLine = Order option -> LineRemoved -> Result<LineRemoved, ErrorMessage>
    //let composeCheckRemoveLine (testExist : OrderExistsTest) (testRemove : LineExistsTest) =
    //    fun (order : Order option) (remove : LineRemoved) ->
    //        match testExist order |> Result.bind testRemove with
    //        | Result.Ok _ -> Result.Ok remove
    //        | Result.Error err -> Result.Error err

    type CheckCommand = Order option -> Command -> Result<Command, ErrorMessage>
    let composeCheckCommand (orderExists : OrderExists) (orderNotExists : OrderNotExists) (lineExists : LineExists) (lineNotExists : LineNotExist) : CheckCommand =
        fun (order : Order option) (cmd : Command) ->
            let getCommand x = x |> Result.map (fun y -> fst y)
            match cmd with
            | Create c -> orderNotExists (cmd, order)
            | AddLine a -> orderExists (cmd, order) |> Result.bind lineNotExists |> getCommand
            | RemoveLine r -> orderExists (cmd, order) |> Result.bind lineExists |> getCommand
            

    



