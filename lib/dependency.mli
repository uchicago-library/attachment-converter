val getUserOS :
  unit -> (Package.t list, Error.t) result

val checkExecutables :
  Package.t list -> (unit, Error.t) result

val checkDependencies : unit -> (unit, Error.t) result
