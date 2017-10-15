
type Result =
    | String of string
    | Long of int64

let x = Long 5L




//type UserName = string

type UserName = UserName of string

let getIdByName userName =
    if userName = "" then
        0
    else
        10