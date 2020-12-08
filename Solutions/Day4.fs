module Day4

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

[<Solution(part = 1)>]
let solvePart1 (input : string) =
    // Count number of valid passports
    input + "\n" // Reducer needs to somehow now to process the last line
    |> Seq.fold passportReducer defaultAccumulation
    |> fun acc -> acc.passports
    |> Seq.filter isValidPassport
    |> Seq.length
    |> Some

