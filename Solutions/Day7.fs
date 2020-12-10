module Day7

open FParsec

type Bag = | Bag of string
let bagify (adjective, color) = Bag (adjective + " " + color)

let equalBag lhv rhv =
    match lhv, rhv with
    | Bag x, Bag y when x = y -> true
    | _ -> false

let canHold bag (bigBag, holdsBags) =
    holdsBags
    |> Seq.exists (equalBag bag)

let shinyGoldBag = Bag ("shiny gold")


let space                   = pchar ' ' 
let word                    = manyChars letter
let maybePluralBag          = pstring " bag" .>> many (pchar 's')
let bag                     = word .>> space .>>. word .>> maybePluralBag |>> bagify
let counted                 = many1Chars digit >>. space
let ignoredCountBag         = counted >>. bag
let noOtherBags             = stringReturn "no other bags" []
let listOfBags              = sepBy ignoredCountBag (pstring ", ")
let bagRule                 = bag .>> pstring " contain " .>>. (noOtherBags <|> listOfBags) // Order is important here, as bags may consume "no other

let parseRules parser (input: string) =
    input
    |> splitLines
    |> Seq.map (run parser)
    |> Seq.map (function
        | Success (rule, _, _) -> Some rule
        | _ -> None)
    |> Seq.choose id


let getHodlers bag rules =
    let rec hodlersOf bag : Bag list =
        let bagHolder = canHold bag
        let bagHolders = rules |> Seq.filter bagHolder |> Seq.map fst |> Seq.toList
        let descentBagHolders = bagHolders |> Seq.map hodlersOf |> Seq.collect id |> Seq.toList
        bagHolders @ descentBagHolders // Not TCO ... hopefully that doesn't kill me
    hodlersOf bag |> Set.ofList

[<Solution(part = 1)>]
let getCountOfHodlerBags (input: string) =
    input
    |> parseRules bagRule
    |> getHodlers shinyGoldBag
    |> Set.count

