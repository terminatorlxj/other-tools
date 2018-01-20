
let last lst = 
    let rec find_last lst = 
        match lst with
        | [e] -> e
        | e::lst' -> find_last lst'
        | _ -> Failwith "list should not be empty" in
    find_last lst