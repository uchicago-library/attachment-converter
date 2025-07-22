open Prelude.Prereq

module Application = struct
  type t =
    | LibreOffice
    | Pandoc
    | Vips
    | ImageMagick
    | GhostScript
    | Verapdf
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
    { app = ImageMagick;
      packageName = "imagemagick";
      executable = Exists "magick"
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
