type t

val of_char : char -> t
val to_char : t -> char

val zero : t
val one : t

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t

val exp : t -> t
val log : t -> t

module Infix : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end
