type t =
  [ `NotInstalled of Package.t list
  | `UnsupportedOS of string ]

module Smart = struct
  let not_installed_err pks = `NotInstalled pks
  let unsupported_os_err name = `UnsupportedOS name
end
