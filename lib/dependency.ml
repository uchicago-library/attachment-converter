open Prelude.Prereq

module Application = struct
  type t = 
    | LibreOffice
    | Pandoc
    | Vips
    | GhostScript
    | Verapdf
end

module Package = struct 
  type t = {app : Application.t; packageName: string; executable: validator}

  let linux : t list = [
    {app = LibreOffice; packageName = "libreoffice"; executable = Exists "soffice"};
    {app = Pandoc; packageName = "pandoc"; executable = Exists "pandoc"};
    {app = Vips; packageName = "libvips"; executable = Exists "vips"};
    {app = GhostScript; packageName = "ghostscript"; executable = Exists "gs"}
  ]

  let darwin : t list = [
    {app = LibreOffice; packageName = "libreoffice"; executable = Exists "soffice"};
    {app = Pandoc; packageName = "pandoc"; executable = Exists "pandoc"};
    {app = Vips; packageName = "libvips"; executable = Exists "vips"};
    {app = GhostScript; packageName = "ghostscript"; executable = Exists "gs"}
  ]

end

module Error = struct 
  type t = [ 
    |`NotInstalled of Package.t list (*keep track of the whole package*)
    |`UnsupportedOS of string
  ] 

  let printError error = match error with
    |`UnsupportedOS os -> print_endline (os^" is not a supported operating system for Attachment Converter.")
    |`NotInstalled lis -> print_string "the following applications still need to be installed: "; List.iter (let open Package in fun pckg -> print_string (pckg.packageName^" ")) lis 
end

let getUserOS () = 
  let open Prelude.Unix.Shell in
  let userPltfrm = snd (input Prelude.readline @@ cmd ["uname";"-s"]) in
    match userPltfrm with 
      |"Linux" -> Ok (Package.linux)
      |"Darwin" -> Ok (Package.darwin)
      |_ -> Error (`UnsupportedOS userPltfrm)

let checkExecutables pkgs = 
  let rec checkExecutables' (pkgs: Package.t list) acc = 
    match pkgs with
    |[] -> acc
    |p::t -> 
      match check [p.executable] with 
      |(Ok _)::_ -> checkExecutables' t acc
      |(Error _)::_ -> checkExecutables' t (acc @ [p]) 
      |[] -> [] in 
  let results = (checkExecutables' pkgs []) in 
  match results with 
    |[] -> Ok () 
    |h::t -> Error (`NotInstalled (h::t))

let checkDependencies () = 
  let open Prelude.Result in   
  let ( let* ) = (>>=) in 
  let* userPckg = getUserOS () in 
  match checkExecutables userPckg with 
    |Ok _ -> Ok ()
    |Error e -> Error.printError e; Error e





 
(*make fails take the whole app and then ok if all results are or list is empty and error of list if something is missing*)
(* let fails res = 
  let rec fails' res acc = match res with
    | [] -> acc
    | (pn, exec) :: t -> begin
      match exec with
      | Ok _ -> fails' t acc
      | Error exec -> (pn, exec) :: fails' t acc 
      end in 
  match (fails' res [] ) with 
  |[] -> Ok []
  |lis -> Error (`NotInstalled lis) *)

  

(* let printMissingPkgs execs = (*this all gets handled by an error printer?*)
  let open Error in 
  if execs = [] then () 
  else
    let execsString = String.concat ", " (List.map (fun x -> getPkgFromExec x) execs) in 
    failwith (execsString ^ " still need(s) to be installed") *)
