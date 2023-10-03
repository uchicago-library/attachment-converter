open Prelude.Prereq

type platform = 
        | Linux
        | Darwin
        | WSL
        | MacOS

type app = 
        | LibreOffice
        | Pandoc
        | Vips
        | GhostScript
        | Verapdf

type packageName = PackageName of string
type executable = validator

type package = (app * (packageName * executable)) list
type lookup = (platform * package) list

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

let wsl : package = [
          (LibreOffice, (PackageName "libreoffice", Exists "soffice"))
        ; (Pandoc, (PackageName "pandoc", Exists "pandoc"))
        ; (Vips, (PackageName "libvips", Exists "vips"))
        ; (GhostScript, (PackageName "ghostscript", Exists "gs"))
        ; (Verapdf, (PackageName "verapdf", Exists "verapdf"))
]

let macos : package = [
          (LibreOffice, (PackageName "libreoffice", Exists "soffice"))
        ; (Pandoc, (PackageName "pandoc", Exists "pandoc"))
        ; (Vips, (PackageName "libvips", Exists "vips"))
        ; (GhostScript, (PackageName "ghostscript", Exists "gs"))
        ; (Verapdf, (PackageName "verapdf", Exists "verapdf"))
]


(*
        TODO: Delete this and use previous macos value on line 44. 
        This is for testing.
*)
let macos : package = [
          (LibreOffice, (PackageName "libreoffice", Exists "soffice1"))
        ; (Pandoc, (PackageName "pandoc", Exists "pandoc"))
        ; (Vips, (PackageName "libvips", Exists "vips1"))
        ; (GhostScript, (PackageName "ghostscript", Exists "gs"))
        ; (Verapdf, (PackageName "verapdf", Exists "verapdf1"))
]

let pltfrmToPkgs : lookup = [
          (Linux, linux)
        ; (Darwin, darwin)
        ; (WSL, wsl)
        ; (MacOS, macos)
]

let listPackages pltfrm = List.assoc pltfrm pltfrmToPkgs

let getExecutables pltfrm = pltfrm |> listPackages |> List.map (fun pkg -> snd (snd pkg))


let fails res = 
        let rec fails' res acc = match res with
                | [] -> acc
                | h :: t -> begin
                        match h with
                        | Ok _ -> fails' t acc
                        | Error exec -> exec :: fails' t acc 
                end
        in fails' res []

(*
TODO: make platform - package into a sum type
*)
let checkDependencies pltfrm = pltfrm |> getExecutables |> check |> fails 


(*
let getExecutables pltfrm =
        let pkgs = listPackages pltfrm in 
                List.map (fun pkg -> snd (snd pkg)) pkgs
*)

(*
        This is a lookup table from Executale names to package names. 
        soffice -> libreoffice
*)
let getPkgFromExec pkg = ()