module type S = sig
  type t
  val zero : t
  val (+) : t -> t -> t
end
