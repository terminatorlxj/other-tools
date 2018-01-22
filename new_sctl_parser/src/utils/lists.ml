exception Zip_error


let last lst = 
    let rec find_last lst = 
        match lst with
        | [e] -> e
        | e::lst' -> find_last lst'
        | _ -> Failwith "list should not be empty" in
    find_last lst

let rec zip lst1 lst2 = 
    match lst1, lst2 with
    | [], [] -> []
    | e1::tl1, e2::tl2 -> (e1, e2) :: (zip tl1 tl2)
    | _ -> raise (Zip_error)

let rec exists lst f = 
    match lst with
    | [] -> false
    | e::tl -> if f e then true else (exists tl f)
    
let rec forall lst f = 
    match lst with
    | [] -> true
    | e::tl -> if f e then forall tl f else false