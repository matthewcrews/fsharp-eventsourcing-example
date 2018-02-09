// Style 1
open System

type Aggregate<'a> = {
    UserId : Guid
    TimeStamp : DateTimeOffset
    Body : 'a
}

type Chicken = {
    Name : string
    Size : decimal
}

type ChickenAggregate = Aggregate<Chicken>

type Duck = {
    Name : string
    Rank : string
    Speed : decimal
}

type DuckAggregate = Aggregate<Duck>


// Style 2
open System

type Header = {
    UserId : Guid
    TimeStamp : DateTimeOffset
}

type Chicken = {
    Name : string
    Size : decimal
}

type ChickenAggregate = {
    Chicken : Chicken
    Header : Header
}

type Duck = {
    Name : string
    Rank : string
    Speed : decimal
}

type DuckAggregate = {
    Duck : Duck
    Header : Header
}