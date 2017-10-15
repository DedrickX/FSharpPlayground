// Validation using applicative style and monadic style

// ------------------------------------------------

type Result<'a> =
    | Success of 'a
    | Failure of string list


module Result =

    let map f xResult =
        match xResult with
        | Success x ->
            Success (f x)
        | Failure errs ->
            Failure errs

    let retn x =
        Success x

    let apply fResult xResult =
        match fResult, xResult with
        | Success f, Success x ->
            Success (f x)
        | Failure errs, Success x ->
            Failure errs
        | Success f, Failure errs ->
            Failure errs
        | Failure errs1, Failure errs2 ->
            Failure (List.concat [errs1; errs2])

    let bind f xResult =
        match xResult with
        | Success x ->
            f x
        | Failure errs ->
            Failure errs


// ------------------------------------------------

type CustomerId = CustomerId of int

type EmailAddress = EmailAddress of string

type CustomerInfo = {
    id: CustomerId
    email: EmailAddress
}

let createCustomerId id =
    if id > 0 then
        Success (CustomerId id)
    else
        Failure ["CustomerID must be positive"]

let createEmailAddress str =
    if System.String.IsNullOrEmpty(str) then
        Failure ["Email must not be empty"]
    elif str.Contains("@") then
        Success (EmailAddress str)
    else
        Failure ["Email must contain @-sign"]