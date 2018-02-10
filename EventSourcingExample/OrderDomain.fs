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

    type OrderExistsTest = Order option -> Result<Order, ErrorMessage>
    type LineExistsTest = Order -> Result<Order, ErrorMessage>
    type LineDoesNotExistTest = Order -> Result<Order, ErrorMessage>

    type CheckCreate = Order option -> Created -> Result<Created, ErrorMessage>
    let composeCheckCreate (check : OrderExistsTest) : CheckCreate =
        fun (order : Order option) (created : Created) ->
            match check order with
            | Result.Ok _ -> Result.Ok created
            | Result.Error err -> Result.Error err

    type CheckAddLine = Order option -> LineAdded -> Result<LineAdded, ErrorMessage>
    let composeCheckAddLine (testExist : OrderExistsTest) (testAdd : LineDoesNotExistTest) =
        fun (order : Order option) (add : LineAdded) ->
            match testExist order |> Result.bind testAdd with
            | Result.Ok _ -> Result.Ok add
            | Result.Error err -> Result.Error err

    type CheckRemoveLine = Order option -> LineRemoved -> Result<LineRemoved, ErrorMessage>
    let composeCheckRemoveLine (testExist : OrderExistsTest) (testRemove : LineExistsTest) =
        fun (order : Order option) (remove : LineRemoved) ->
            match testExist order |> Result.bind testRemove with
            | Result.Ok _ -> Result.Ok remove
            | Result.Error err -> Result.Error err

    type CheckCommand = Order option -> Command -> Result<Command, ErrorMessage>

    //// Bad
    //let testCreate (order : Order option) (create : Created) =
    //    match order with
    //    | Some r -> false
    //    | None -> true

    // Good
    let testCreate (order : Order option) (create : Created) =
        if Option.isNone order then 
            Result.Ok create
        else
            Result.Error "Order already exists"
        


    (*

    Okay, here is the problem, I am trying to combine too many checks into a single function.
    
    //Example
    let testAddLine (order : Order option) (add : LineAdded) =
        let test (a : LineAdded) o = not(o.Lines |> List.exists (fun l -> l.LineId = a.LineId))
        Option.exists (test add) order

    Here I am trying to check that the order exists and that the OrderLine does not already exist.
    This is combining two different 'functions' into one. Better to have a function that
    check for existence and then having a function that checks for the Line not existing.
    The ExistenceCheck has the signature `Order option -> Result<Order, string>` while 
    the LineCheck has the signature `Order -> Result<Order, string>`.

    *)



    let testRemoveLine (order : Order option) (remove : LineRemoved) =
        let test (r : LineRemoved) o  = o.Lines |> List.exists (fun l -> l.LineId = r.LineId)
        Option.exists (test remove) order

    let composeTester (testCreate : TestCreate) (testAdd : TestAddLine) (testRemove : TestRemoveLine) : Tester =
        fun (order : Order option) (cmd : Command) ->
            match cmd with
            | Create c -> testCreate order c
            | AddLine a -> testAdd order a
            | RemoveLine r -> testRemove order r
        

    type Create = EventWriter -> Created -> CommandResult
    let create (writer : EventWriter) (c : Created) =
        writer (string c.Id) (Event.Created c)
        Result.Ok "Order Added"

    type AddLine = EventWriter -> Order -> LineAdded -> CommandResult
    let addLine (writer : EventWriter) (order : Order) (a : LineAdded) =
        if not(order.Lines |> List.exists (fun l -> l.LineId = a.LineId)) then
            writer (string a.Id) (Event.LineAdded a)
            Result.Ok "Line Added"
        else
            Result.Error "Line already exists"

    type RemoveLine = EventWriter -> Order -> LineRemoved -> CommandResult
    let removeLine (writer : EventWriter) (order : Order) (r : LineRemoved) =
        if order.Lines |> List.exists (fun l -> l.LineId = r.LineId) then
            writer (string r.Id) (Event.LineRemoved r)
            Result.Ok "Line Removed"
        else
            Result.Error "Line does not exist"

    type CommandHandler = Command -> CommandResult
    type CommandHandlerComposer = Create -> AddLine -> RemoveLine -> OrderSource -> EventWriter -> CommandHandler

    //type CommandHandlerBuilder = Create -> AddLine -> RemoveLine -> (OrderSource -> EventWriter -> Result<string, string>)
    //let commandHandlerBuilder (create : Create) (add : AddLine) (remove : RemoveLine) =
    //    fun (orderSource : OrderSource) (writer : EventWriter) ->
            

    //type CommandHandler = Command -> Result<string, string>
    //let composeCommandHandler (create : Create) (add : AddLine) (remove : RemoveLine) (orderSource : OrderSource) (writer : EventWriter) : CommandHandler =
    //    fun (cmd : Command) ->
    //        let order = orderSource (string cmd.Id)
    //        match cmd, order with
    //        | Create c, None -> create writer c
    //        | Create c, Some o -> Result.Error "Order already exists"
    //        | _, None -> Result.Error "Order does not exist"
    //        | AddLine l, Some o -> add writer o l
    //        | RemoveLine l, Some o -> remove writer o l
            

    



