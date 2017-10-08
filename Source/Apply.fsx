
// Common names: apply, ap
// Unpacks a function wrapped inside an elevated value into a lifted function
//
// Signature:
// E<(a -> b)> -> E<a> -> E<b>

let applyOption fOpt xOpt =
    match fOpt, xOpt with
    | Some f, Some x -> Some (f x)
    | _ -> None

let applyList (fList: ('a->'b) list) (xList: 'a list)  =
    [ for f in fList do
        for x in xList do
            yield f x ]