
type pmodule_env = {
    pmodule: Parsetree.pmodule;
    env: (string, Type.ttype) Hashtbl.t
  }


let check_modules pmodules = 
  let pmenvs = Hashtbl.create 1 in  
  List.iter (fun pm -> Hashtbl.add pmenvs pm.name {pmodule = pm; env = Hashtbl.create 1;}) pmodules;

