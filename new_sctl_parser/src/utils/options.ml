let fmap oe f = 
    match oe with
    | None -> None
    | Some e -> Some (f e)