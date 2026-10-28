open System

let parse c =
    let parse input =
        match input with
        | s when String.IsNullOrEmpty s -> Error "no more input"
        | s when s[0] = c -> Ok(c, s[1..])
        | s -> s[0] |> sprintf "found %c" |> Error

    parse

let andThen p1 p2 =
    let andThen input =
        match p1 input with
        | Ok(result1, remaining) ->
            match p2 remaining with
            | Ok(result2, remaining) -> Ok((result1, result2), remaining)
            | Error e -> Error e
        | Error e -> Error e

    andThen

let (.>>.) = andThen

let orElse p1 p2 =
    let orElse input =
        match p1 input with
        | Ok(result, remaining) -> Ok(result, remaining)
        | Error _ -> p2 input

    orElse

let (<|>) = orElse

let a = parse 'a'

let aAndB = parse 'a' .>>. parse 'b'

let aOrB = parse 'a' <|> parse 'b'

"abc" |> a |> printfn "%A"
"bca" |> a |> printfn "%A"

"abc" |> aAndB |> printfn "%A"
"bca" |> aAndB |> printfn "%A"

"abc" |> aOrB |> printfn "%A"
"bca" |> aOrB |> printfn "%A"
"cab" |> aOrB |> printfn "%A"