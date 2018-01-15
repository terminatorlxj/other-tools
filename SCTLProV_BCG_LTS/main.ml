open Prover

let main () =
  let spec = ref "" in
  Arg.parse
    [
      "-spec", Arg.String (fun str -> spec := str), " Specification file";
      "-deadlock", Arg.Unit (fun () -> Prover.deadlock := true), " Detecting deadlock while proving"
    ]
    (fun s ->
      try
        let spec_bindings = Parser.program Lexer.token (Lexing.from_channel (open_in !spec)) in
        let lts = Lts.create_lts s in
        Prover.prove_model lts spec_bindings
      with 
        Prover.Deadlock s -> print_endline "Deadlock detected!!!"
        | e -> 
          print_endline ("error in line "^(string_of_int !Lexer.lineno));
          raise e
    )
    "Usage: sctlbcg <filename>"

let _ = 
  Printexc.print main ()