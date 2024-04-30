let listOfPrime a b =
    let rec removeMultiples lst mult =
        match lst with
        | [] -> []
        | x::xs when x % mult = 0 -> removeMultiples xs mult
        | x::xs -> x::(removeMultiples xs mult)
    let rec sieve lst =
        match lst with
        | [] -> []
        | x::xs -> x::(sieve (removeMultiples xs x))
    let rec cutList lst a =
        match lst with
        | [] -> []
        | x::xs when x < a -> cutList xs a
        | _ -> lst
    cutList (sieve [2..b]) a