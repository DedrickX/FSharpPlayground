
/// map for Options
let mapOption f opt =
    match opt with
    | None ->
        None
    | Some x ->
        Some (f x)


/// map for Lists
let rec mapList f list =
    match list with
    | [] ->
        []
    | head::tail ->
        // new head + new tail
        (f head) :: (mapList f tail)


/// Define a function in the normal world
let add1 x = x + 1
// has type : int -> int


/// A function lifted to the world of Options
let add1IfSomething = Option.map add1
// has type : int option -> int option


/// A function lifted to the world of Lists
let add1ToEachElement = List.map add1

// ==================================================================

// With these mapped versions in place you can write code like this:
Some 2 |> add1IfSomething    // Some 3
[1;2;3] |> add1ToEachElement // [2; 3; 4]

// In many cases, we don’t bother to create an intermediate function – partial application is used instead:
Some 2 |> Option.map add1    // Some 3
[1;2;3] |> List.map add1     // [2; 3; 4]