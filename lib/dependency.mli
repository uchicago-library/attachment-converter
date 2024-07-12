val getUserOS :
  unit -> (Package.Package.t list, Error.t) result

val checkExecutables :
  Package.Package.t list -> (unit, Error.t) result

val checkDependencies : unit -> (unit, Error.t) result
