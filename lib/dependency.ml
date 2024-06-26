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
  type t =
    { app : Application.t;
      packageName : string;
      executable : validator
    }

  let getName pckg = pckg.packageName

  let linux : t list =
    [ { app = LibreOffice;
        packageName = "libreoffice";
        executable = Exists "soffice"
      };
      { app = Pandoc;
        packageName = "pandoc";
        executable = Exists "pandoc"
      };
      { app = Vips;
        packageName = "libvips";
        executable = Exists "vips"
      };
      { app = GhostScript;
        packageName = "ghostscript";
        executable = Exists "gs"
      }
    ]

  let darwin : t list =
    [ { app = LibreOffice;
        packageName = "libreoffice";
        executable = Exists "soffice"
      };
      { app = Pandoc;
        packageName = "pandoc";
        executable = Exists "pandoc"
      };
      { app = Vips;
        packageName = "libvips";
        executable = Exists "vips"
      };
      { app = GhostScript;
        packageName = "ghostscript";
        executable = Exists "gs"
      }
    ]

  let toString pckglist =
    if pckglist = linux
    then
      "linux: ["
      ^ String.concat ", " (List.map getName linux)
      ^ "]"
    else if pckglist = darwin
    then
      "darwin: ["
      ^ String.concat ", " (List.map getName darwin)
      ^ "]"
    else ""
end

module Error = struct
  type t =
    [ `NotInstalled of Package.t list
    | `UnsupportedOS of string ]

  let message err =
    match err with
    | `UnsupportedOS os ->
      os
      ^ " is not a supported operating system for \
         Attachment Converter.\n\
         Here is a list of supported Os-es:\n\n\
         \tmacOS\n\
         \tArch Linux\n\
         \tWSL Debian\n\
         \r"
    | `NotInstalled lis ->
      "Attachment Converter will not run unless all of its \
       OS-level dependencies are installed.\n\n\
       It looks like the following software packages still \
       need to be installed:\n\
       \t"
      ^ String.concat "\n\t" (List.map Package.getName lis)
      ^ "\n\r"

  let toString err =
    match err with
    | `UnsupportedOS os -> "`UnsupportedOS " ^ os
    | `NotInstalled lis ->
      "`NotInstalled ["
      ^ String.concat ", " (List.map Package.getName lis)
      ^ "]"
end

let getUserOS () =
  let open Prelude.Unix.Shell in
  let userPltfrm =
    snd (input Prelude.readline @@ cmd [ "uname"; "-s" ])
  in
  match userPltfrm with
  | "Linux" -> Ok Package.linux
  | "Darwin" -> Ok Package.darwin
  | _ -> Error (`UnsupportedOS userPltfrm)

let checkExecutables pkgs =
  let rec checkExecutables' (pkgs : Package.t list) acc =
    match pkgs with
    | [] -> acc
    | p :: t -> (
      match check [ p.executable ] with
      | Ok _ :: _ -> checkExecutables' t acc
      | Error _ :: _ -> checkExecutables' t (acc @ [ p ])
      | [] -> [] )
  in
  let results = checkExecutables' pkgs [] in
  match results with
  | [] -> Ok ()
  | h :: t -> Error (`NotInstalled (h :: t))

let checkDependencies () =
  let open Prelude.Result in
  let ( let* ) = ( >>= ) in
  let* userPckg = getUserOS () in
  match checkExecutables userPckg with
  | Ok _ -> Ok ""
  | Error e -> Error e

let main () =
  match checkDependencies () with
  | Ok _ -> ()
  | Error e -> print_endline (Error.message e)
