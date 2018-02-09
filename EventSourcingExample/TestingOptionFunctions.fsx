
type Chicken = {
    Name : string
}

type OptionChicken = Chicken Option

let opCh = Some { Name = "Phil" }

let testChicken (c : OptionChicken) =
    Option.exists (fun x -> if x.Name = "Phil" then true else false) c

testChicken None