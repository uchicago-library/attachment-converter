(* Attachment Converter is distributed under the terms of the GNU *)
(* GPL-3.0-or-later. *)

(* Copyright 2026 Matt Teichman and Nathan Mull *)


open Prelude.Prereq

module Application = struct
  type t =
    | LibreOffice
    | Pandoc
    | Vips
    | ImageMagick
    | GhostScript
    | Verapdf
    | PdfToText
end

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
    { app = ImageMagick;
      packageName = "imagemagick";
      executable = Exists "magick"
    };
    { app = GhostScript;
      packageName = "ghostscript";
      executable = Exists "gs"
    };
    { app = PdfToText;
      packageName = "poppler";
      executable = Exists "pdftotext"
    };
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
    { app = ImageMagick;
      packageName = "imagemagick";
      executable = Exists "magick"
    };
    { app = GhostScript;
      packageName = "ghostscript";
      executable = Exists "gs"
    };
    { app = PdfToText;
      packageName = "poppler";
      executable = Exists "pdftotext"
    };
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


(* This file is part of Attachment Converter. *)

(* Attachment Converter is free software: you can redistribute it *)
(* and/or modify it under the terms of the GNU General Public License *)
(* as published by the Free Software Foundation, either version 3 of *)
(* the License, or (at your option) any later version. *)

(* Attachment Converter is distributed in the hope that it will be *)
(* useful, but WITHOUT ANY WARRANTY; without even the implied warranty *)
(* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU *)
(* General Public License for more details. *)

(* You should have received a copy of the GNU General Public License *)
(* along with Attachment Converter. If not, see *)
(* <https://www.gnu.org/licenses/>. *)
