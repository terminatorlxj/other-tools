

(* let main () = 
  let property = ref "deadlock" in
  Arg.parse
    [
      "-deadlock", Arg.Unit (fun () -> property := "deadlock"), " Detect deadlock";
      "-livelock", Arg.Unit (fun () -> property := "livelock"), " Detect livelock";
    ]
    (fun s -> 
      let lstate = Bcg_interface.read_bcg s in
      if !property = "deadlock" then
        Prover_deadlock.prove_model (Ks_deadlock.create_model lstate) [(!property, Formula_deadlock.EU (Formula_deadlock.SVar "x", Formula_deadlock.SVar "y", Formula_deadlock.Top, Formula_deadlock.Bottom, Formula_deadlock.SVar "ini"))] true true
      else 
        Prover_livelock.prove_model (Ks_livelock.create_model lstate) [(!property, Formula_livelock.EU (Formula_livelock.SVar "z", Formula_livelock.SVar "w", Formula_livelock.Top, Formula_livelock.EG (Formula_livelock.SVar "x", Formula_livelock.Top, Formula_livelock.SVar "w"), Formula_livelock.SVar "ini"))] false false
    )
    "Usage: sctlbcg [<-deadlock>/<-livelock>] <filename>" *)

let main () =
  let spec = ref "" in
  Arg.parse
    [
      "-spec", Arg.String (fun str -> spec := str), " Specification file"
    ]
    (fun s ->
      let spec_bindings = Parser.program Lexer.token (Lexing.from_channel (open_in !spec)) in
      let lts = Lts.create_lts s in
      Prover.prove_model lts spec_bindings
    )
    "Usage: sctlbcg <filename>"

  (* Parser.input Lexer.token (Lexing.from_channel (open_in input_file))  *)

let _ = 
  Printexc.print main ()