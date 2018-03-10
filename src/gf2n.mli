module type Field = sig
  type t

  val zero : t
  val one : t
  val compare : t -> t -> int

  (** Must raise Invalid_argument if the input is out of bounds. *)
  val of_int : int -> t
  (** Must raise Invalid_argument if the input is out of bounds. *)
  val of_string : string -> t
  val to_string : t -> string

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  module Infix: sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
  end
end

module type GaloisField = sig
  include Field
  (** Power of the order (which must be a prime power). *)
  val power : int
  (** Irreducible polynomial of this representation, of degree equal to the power. *)
  val irred_poly : t
end

module GF2_8: GaloisField with type t = Z.t
module GF2_16: GaloisField with type t = Z.t
module GF2_24: GaloisField with type t = Z.t
module GF2_32: GaloisField with type t = Z.t
module GF2_40: GaloisField with type t = Z.t
module GF2_48: GaloisField with type t = Z.t
module GF2_56: GaloisField with type t = Z.t
module GF2_64: GaloisField with type t = Z.t
module GF2_72: GaloisField with type t = Z.t
module GF2_80: GaloisField with type t = Z.t
module GF2_88: GaloisField with type t = Z.t
module GF2_96: GaloisField with type t = Z.t
module GF2_104: GaloisField with type t = Z.t
module GF2_112: GaloisField with type t = Z.t
module GF2_120: GaloisField with type t = Z.t
module GF2_128: GaloisField with type t = Z.t
module GF2_136: GaloisField with type t = Z.t
module GF2_144: GaloisField with type t = Z.t
module GF2_152: GaloisField with type t = Z.t
module GF2_160: GaloisField with type t = Z.t
module GF2_168: GaloisField with type t = Z.t
module GF2_176: GaloisField with type t = Z.t
module GF2_184: GaloisField with type t = Z.t
module GF2_192: GaloisField with type t = Z.t
module GF2_200: GaloisField with type t = Z.t
module GF2_208: GaloisField with type t = Z.t
module GF2_216: GaloisField with type t = Z.t
module GF2_224: GaloisField with type t = Z.t
module GF2_232: GaloisField with type t = Z.t
module GF2_240: GaloisField with type t = Z.t
module GF2_248: GaloisField with type t = Z.t
module GF2_256: GaloisField with type t = Z.t
module GF2_264: GaloisField with type t = Z.t
module GF2_272: GaloisField with type t = Z.t
module GF2_280: GaloisField with type t = Z.t
module GF2_288: GaloisField with type t = Z.t
module GF2_296: GaloisField with type t = Z.t
module GF2_304: GaloisField with type t = Z.t
module GF2_312: GaloisField with type t = Z.t
module GF2_320: GaloisField with type t = Z.t
module GF2_328: GaloisField with type t = Z.t
module GF2_336: GaloisField with type t = Z.t
module GF2_344: GaloisField with type t = Z.t
module GF2_352: GaloisField with type t = Z.t
module GF2_360: GaloisField with type t = Z.t
module GF2_368: GaloisField with type t = Z.t
module GF2_376: GaloisField with type t = Z.t
module GF2_384: GaloisField with type t = Z.t
module GF2_392: GaloisField with type t = Z.t
module GF2_400: GaloisField with type t = Z.t
module GF2_408: GaloisField with type t = Z.t
module GF2_416: GaloisField with type t = Z.t
module GF2_424: GaloisField with type t = Z.t
module GF2_432: GaloisField with type t = Z.t
module GF2_440: GaloisField with type t = Z.t
module GF2_448: GaloisField with type t = Z.t
module GF2_456: GaloisField with type t = Z.t
module GF2_464: GaloisField with type t = Z.t
module GF2_472: GaloisField with type t = Z.t
module GF2_480: GaloisField with type t = Z.t
module GF2_488: GaloisField with type t = Z.t
module GF2_496: GaloisField with type t = Z.t
module GF2_504: GaloisField with type t = Z.t
module GF2_512: GaloisField with type t = Z.t
module GF2_520: GaloisField with type t = Z.t
module GF2_528: GaloisField with type t = Z.t
module GF2_536: GaloisField with type t = Z.t
module GF2_544: GaloisField with type t = Z.t
module GF2_552: GaloisField with type t = Z.t
module GF2_560: GaloisField with type t = Z.t
module GF2_568: GaloisField with type t = Z.t
module GF2_576: GaloisField with type t = Z.t
module GF2_584: GaloisField with type t = Z.t
module GF2_592: GaloisField with type t = Z.t
module GF2_600: GaloisField with type t = Z.t
module GF2_608: GaloisField with type t = Z.t
module GF2_616: GaloisField with type t = Z.t
module GF2_624: GaloisField with type t = Z.t
module GF2_632: GaloisField with type t = Z.t
module GF2_640: GaloisField with type t = Z.t
module GF2_648: GaloisField with type t = Z.t
module GF2_656: GaloisField with type t = Z.t
module GF2_664: GaloisField with type t = Z.t
module GF2_672: GaloisField with type t = Z.t
module GF2_680: GaloisField with type t = Z.t
module GF2_688: GaloisField with type t = Z.t
module GF2_696: GaloisField with type t = Z.t
module GF2_704: GaloisField with type t = Z.t
module GF2_712: GaloisField with type t = Z.t
module GF2_720: GaloisField with type t = Z.t
module GF2_728: GaloisField with type t = Z.t
module GF2_736: GaloisField with type t = Z.t
module GF2_744: GaloisField with type t = Z.t
module GF2_752: GaloisField with type t = Z.t
module GF2_760: GaloisField with type t = Z.t
module GF2_768: GaloisField with type t = Z.t
module GF2_776: GaloisField with type t = Z.t
module GF2_784: GaloisField with type t = Z.t
module GF2_792: GaloisField with type t = Z.t
module GF2_800: GaloisField with type t = Z.t
module GF2_808: GaloisField with type t = Z.t
module GF2_816: GaloisField with type t = Z.t
module GF2_824: GaloisField with type t = Z.t
module GF2_832: GaloisField with type t = Z.t
module GF2_840: GaloisField with type t = Z.t
module GF2_848: GaloisField with type t = Z.t
module GF2_856: GaloisField with type t = Z.t
module GF2_864: GaloisField with type t = Z.t
module GF2_872: GaloisField with type t = Z.t
module GF2_880: GaloisField with type t = Z.t
module GF2_888: GaloisField with type t = Z.t
module GF2_896: GaloisField with type t = Z.t
module GF2_904: GaloisField with type t = Z.t
module GF2_912: GaloisField with type t = Z.t
module GF2_920: GaloisField with type t = Z.t
module GF2_928: GaloisField with type t = Z.t
module GF2_936: GaloisField with type t = Z.t
module GF2_944: GaloisField with type t = Z.t
module GF2_952: GaloisField with type t = Z.t
module GF2_960: GaloisField with type t = Z.t
module GF2_968: GaloisField with type t = Z.t
module GF2_976: GaloisField with type t = Z.t
module GF2_984: GaloisField with type t = Z.t
module GF2_992: GaloisField with type t = Z.t
module GF2_1000: GaloisField with type t = Z.t
module GF2_1008: GaloisField with type t = Z.t
module GF2_1016: GaloisField with type t = Z.t
module GF2_1024: GaloisField with type t = Z.t

module GF2_1536: GaloisField with type t = Z.t
module GF2_2048: GaloisField with type t = Z.t
module GF2_3072: GaloisField with type t = Z.t
module GF2_4096: GaloisField with type t = Z.t

(** Implementation of GF(2^8) *)
module GF256: sig
  include Field
  val of_char : char -> t
  val to_char : t -> char
end
