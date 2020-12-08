module Day4

open System.Text.RegularExpressions

open Utilities

let sample = @"
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"

// Goal: single-pass reducer, no regex, with a state machine pattern

type Passport =
    { ecl: string option;
      pid: string option;
      eyr: string option;
      hcl: string option;
      byr: string option;
      iyr: string option;
      hgt: string option;

      cid: string option;
    }

type ParserState =
    | GettingLabel of buff: string
    | GettingValue of label: string * buff: string
    | Awaiting

type Accumulation =
    { state: ParserState;
      passport: Passport option;
      passports: Passport list;
    }

let defaultPassport =
    { ecl = None;
      pid = None;
      eyr = None;
      hcl = None;
      byr = None;
      iyr = None;
      hgt = None;
      cid = None;
    }

let defaultAccumulation =
    { state = Awaiting;
      passport = None;
      passports = [];
    }

exception Unsupported
exception BadState

let updatePassport passport label value =
    // TODO Better way to match string to record property to reduce code duplication?
    //      Reflection?
    //      Want to keep the strong typing of Passport, so no Map
    match label with
    | "ecl" -> { passport with ecl = Some value }
    | "pid" -> { passport with pid = Some value }
    | "eyr" -> { passport with eyr = Some value }
    | "hcl" -> { passport with hcl = Some value }
    | "byr" -> { passport with byr = Some value }
    | "iyr" -> { passport with iyr = Some value }
    | "hgt" -> { passport with hgt = Some value }
    | "cid" -> { passport with cid = Some value }
    | _ -> passport // Unknown

// TODO Factor out the state change reducers
let passportReducer ( acc : Accumulation ) (nxt : char) =
    match acc.state, nxt with
    | Awaiting, ' ' ->
        // Skip whitespace
        acc
    | Awaiting, '\n' ->
        // Blank line, process passport
        match acc.passport with
        | Some passport ->
            { acc with
                state = Awaiting;
                passport = None;
                passports = passport :: acc.passports } // TODO Cons reverses the list... is that OK?
        | None -> acc
    | Awaiting, c ->
        // Start a new passport
        { acc with state = GettingLabel(string c) }
    | GettingLabel buff, ':' ->
        // Start getting the value
        { acc with state = GettingValue(buff, "") }
    | GettingLabel buff, c ->
        // Augment the label buffer
        { acc with state = GettingLabel(buff + string c)}
    | GettingValue (label, buff), (' '|'\n') ->
        // Update Passport with the label/value
        let passport = Option.defaultValue defaultPassport acc.passport
        { acc with
            state = Awaiting;
            passport = Some (updatePassport passport label buff) }
    | GettingValue (label, buff), c ->
        // Augment the value buffer
        { acc with state = GettingValue(label, buff + string c) }


let isValidPassport passport =
    match passport with
    | { byr = Some _;
        iyr = Some _;
        eyr = Some _;
        hgt = Some _;
        hcl = Some _;
        ecl = Some _;
        pid = Some _;
       } -> true
    | _ -> false

let parsePassports (passports: string) =
    passports + "\n" // Reducer needs to somehow now to process the last line
    |> Seq.fold passportReducer defaultAccumulation
    |> fun acc -> acc.passports

[<Solution(part = 1)>]
let solvePart1 (input: string) =
    // Count number of valid passports
    input
    |> parsePassports
    |> Seq.filter isValidPassport
    |> Seq.length
    |> Some

let tryInt (maybeInt: string) =
    match System.Int32.TryParse maybeInt with
    | true, num -> Some num
    | _ -> None

let tryIntOption = Option.bind tryInt

// TODO Is this included? Maybe something like inRange 0 32 Inclusive|LeftInclusive|RightInclusive or something?
let inRange min max x = x >= min && x <= max

// Active patterns match if Some
let (|IntBetween|_|) min max = tryIntOption >> Option.filter (inRange min max)
let (|BirthYear|_|) = tryIntOption >> Option.filter (inRange 1920 2002)
let (|IssueYear|_|) = tryIntOption >> Option.filter (inRange 2010 2020)
let (|ExpirationYear|_|) = tryIntOption >> Option.filter (inRange 2020 2030)

let (|Regex|_|) pattern input =
   let m = Regex.Match(input, pattern)
   if m.Success then Some input else None

let matches pattern input = Regex.Match(input, pattern).Success
let isValidHeight str =
    // I could do this with unit types, lists... blah
    let m = Regex.Match(str, "(\\d+)(cm|in)")
    if m.Success then
        let unit = m.Groups.[2].Value
        let value = tryInt m.Groups.[1].Value

        match unit, value with
        | "in", Some v when (inRange 59 76 v) -> true
        | "cm", Some v when (inRange 150 193 v) -> true
        | _ -> false
    else
        false

(* byr (Birth Year) - four digits; at least 1920 and at most 2002.
   iyr (Issue Year) - four digits; at least 2010 and at most 2020.
   eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
   hgt (Height) - a number followed by either cm or in:
       If cm, the number must be at least 150 and at most 193.
       If in, the number must be at least 59 and at most 76.
   hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
   ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
   pid (Passport ID) - a nine-digit number, including leading zeroes.
   cid (Country ID) - ignored, missing or not. *)
let isReallyValidPassport passport =
    // Let's get crazy and hammer active patterns
    // BAH -- can't seem to get active patterns on record properties :(
    // I'm tired -- doing this an easy way
    passport.byr |> Option.bind tryInt |> Option.exists (inRange 1920 2002) &&
    passport.iyr |> Option.bind tryInt |> Option.exists (inRange 2010 2020) &&
    passport.eyr |> Option.bind tryInt |> Option.exists (inRange 2020 2030) &&
    passport.hgt |> Option.exists isValidHeight &&
    passport.hcl |> Option.exists (matches "#[0-9a-f]{6}") &&
    passport.ecl |> Option.exists (matches "(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)") &&
    passport.pid |> Option.exists (matches "^\\d{9}$")

[<Solution(part = 2)>]
let solvePart2 (input: string) =
    let valid =
        input
        |> parsePassports
        |> Seq.filter isReallyValidPassport

    let s = function
        | Some t -> t
        | None -> ""

    valid |> Seq.iter (fun x -> printfn "%A,%A,%A,%A,%A,%A,%A" (s x.byr) (s x.iyr) (s x.eyr) (s x.hgt) (s x.hcl) (s x.ecl) (s x.pid))

    valid
    |> Seq.length
    |> Some

