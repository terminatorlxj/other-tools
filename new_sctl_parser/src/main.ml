open Printf
open Parsetree

let _ = 
  let only_parsing = ref false in
  let inputfiles = ref [] in
  (* let only_typechecking = ref false in *)
  Arg.parse
    [
      (* "-t", Arg.Unit (fun () -> only_typechecking := true), "\tOnly perform the type checking" *)
      "-p", Arg.Unit (fun () -> only_parsing := true), "\tOnly paring, does not performing verification";
    ]
    (fun filename -> inputfiles := filename :: !inputfiles)
    "Usage: sctl [-p] file1 ... filen";
  if !inputfiles = [] then begin
    printf "Error: No input file.";
    exit 1
  end else begin
    match !only_parsing with
    | true ->
      let pmodules = List.map (fun inputfile -> 
        let pmodule = Parser.program Lexer.token (Lexing.from_channel (open_in inputfile)) in
        let module_name = List.hd (String.split_on_char '.' inputfile) in
        if module_name = "" then begin
          pmodule.name <- String.capitalize_ascii inputfile;
          pmodule.filename <- inputfile
        end else begin
          pmodule.name <- String.capitalize_ascii module_name;
          pmodule.filename <- inputfile
        end;
        pmodule
        ) !inputfiles in
      Typechecker.check_modules pmodules;
      (* prove pmodules *)
    | false ->
      let pmodules = List.map (fun inputfile -> 
      let pmodule = Parser.program Lexer.token (Lexing.from_channel (open_in inputfile)) in
      let module_name = List.hd (String.split_on_char '.' inputfile) in
      if module_name = "" then begin
        pmodule.name <- String.capitalize_ascii inputfile;
        pmodule.filename <- inputfile
      end else begin
        pmodule.name <- String.capitalize_ascii module_name;
        pmodule.filename <- inputfile
      end;
      pmodule
      ) !inputfiles in
      Typechecker.check_modules pmodules;
      Prover.prove_modules pmodules
  end