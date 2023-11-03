open Prelude.Prereq

type app = 
  | LibreOffice
  | Pandoc
  | Vips
  | GhostScript
  | Verapdf

type executable = validator

type packageName = PackageName of string

type package = (app * (packageName * executable)) list

type platform = 
  | Linux of package
  | Darwin of package

let linux : package = [
    (LibreOffice, (PackageName "libreoffice", Exists "soffice"))
  ; (Pandoc, (PackageName "pandoc", Exists "pandoc"))
  ; (Vips, (PackageName "libvips", Exists "vips"))
  ; (GhostScript, (PackageName "ghostscript", Exists "gs"))
]

let darwin : package = [
    (LibreOffice, (PackageName "libreoffice", Exists "soffice"))
  ; (Pandoc, (PackageName "pandoc", Exists "pandoc"))
  ; (Vips, (PackageName "libvips", Exists "vips"))
  ; (GhostScript, (PackageName "ghostscript", Exists "gs"))
]

let getPackage pltfrm = match pltfrm with 
  |Linux p -> p 
  |Darwin p -> p 

let getExecutables pltfrm = pltfrm |> getPackage |> List.map (fun pkg -> snd (snd pkg))

let fails res = 
  let rec fails' res acc = match res with
    | [] -> acc
    | h :: t -> begin
      match h with
      | Ok _ -> fails' t acc
      | Error exec -> exec :: fails' t acc 
      end
  in fails' res []


let checkForExecutables pltfrm = pltfrm |> getExecutables |> check |> fails 


let getPkgFromExec exec = 
  let execNmToPkgNm = [
    ("soffice", "libreoffice");
    ("pandoc", "pandoc");
    ("vips", "libvips");
    ("gs", "ghostscript");
    ("verapdf", "verapdf")
  ] in 
List.assoc exec execNmToPkgNm 

let printMissingPkgs execs =
  if execs = [] then () 
  else
    let execsString = String.concat ", " (List.map (fun x -> getPkgFromExec x) execs) in 
    failwith (execsString ^ " still need(s) to be installed")

let getPltfrm = 
  let open Prelude.Unix.Shell in
  let userPltfrm = snd (input Prelude.readline @@ cmd ["uname";"-s"]) in 
    match userPltfrm with 
      |"Linux" -> Linux linux
      |"Darwin" -> Darwin darwin
      |_ -> failwith (userPltfrm ^ " is not a supported platform")

let checkDependencies = 
  getPltfrm |> checkForExecutables |>  printMissingPkgs