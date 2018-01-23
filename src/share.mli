module type Field = sig
  type t

  val size : int
  val zero : t
  val one : t

  val of_int : int -> t option

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val exp : t -> t
  val log : t -> t

  module Infix: sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
  end
end

module type DHField = sig
  include Field
  type g

  val zero_g : g
  val one_g : g
  val publish : t -> g
  (* publish one == one_g *)
  val mul_g : t -> g -> g
  (* mul_g size one_g == one_g *)
  val add_g : g -> g -> g
end

(** Implementation of GF(2^8) *)
module GF256: sig
  include Field with type t = int
  val of_char : char -> t
  val to_char : t -> char
  val of_string : string -> t array
  val to_string : t array -> string
end

(** Pure RNG type. *)
type ('a, 's) rng = 's -> ('a * 's)

(** [share_byte secret threshold shares] splits a string [secret] into [shares]
 * shares where (at least) [threshold] shares are necessary to reconstruct
 * [secret]. *)
val share : ?g:Nocrypto.Rng.g -> string -> int -> int -> (GF256.t * string) array

(** [unshare shares] combines [shares] to reconstruct the secret. Note that
 * there's no integrity checks. It is up to the user to ensure that there are
 * enough shares, and that none of the shares have been tampered with or
 * otherwise modified. *)
val unshare : (GF256.t * string) array -> string

(** [extend k shares] generates k more shares from [shares]. The same caveats as
 * [unshare] apply here too. *)
val extend : ?g:Nocrypto.Rng.g -> int -> (GF256.t * string) array -> (GF256.t * string) array

(** Same as [share], but if you only want to share a one-byte secret *)
val share_byte : ?g:Nocrypto.Rng.g -> char -> int -> int -> (GF256.t * GF256.t) array

(** Same as [unshare], but if you only want to reconstruct a one-byte secret *)
val unshare_byte : (GF256.t * GF256.t) array -> char

(** Same as [extend], but if you only want to extend a one-byte secret. *)
val extend_byte : ?g:Nocrypto.Rng.g -> int -> (GF256.t * GF256.t) array -> (GF256.t * GF256.t) array

module type Share = sig
  type t (* master secret / share index *)
  type g (* share / recovered secret *)
  type shares = (t * g) array
  type array_shares = (t * g array) array

  val share : t -> int -> int -> (int -> (t array, 's) rng) -> (shares, 's) rng
  val unshare : shares -> g
  val extend : t array -> shares -> shares
  val extend' : int -> shares -> (int -> (t array, 's) rng) -> (shares, 's) rng

  val share_array : t array -> int -> int -> (int -> (t array, 's) rng) -> (array_shares, 's) rng
  val unshare_array : array_shares -> g array
  val extend_array : t array -> array_shares -> array_shares
  val extend_array' : int -> array_shares -> (int -> (t array, 's) rng) -> (array_shares, 's) rng
end

(** Construct a SecretShare module over an arbitrary field. *)
module SecretShare (F: Field): Share with type t = F.t and type g = F.t

(** Construct a PublicShare module over an arbitrary DH field.

    The master secret (of type F.t) remains secret even given enough public
    shares. Instead, the value (of type F.g) recovered by [unshare] can be used
    to verify proofs produced by someone that *does* know the master secret.

    The SecretShare module is available as a sub-module. The secret shares may
    be used to produce public *proof-shares* (via DHField.publish) that when
    combined via [unshare], forms a single proof that can be verified as if it
    was produced by the master secret. In other words, this forms a basic
    deterministic threshold signature scheme where the secret keys are generated
    by one party to be distributed to other signers. (More secure schemes are
    possible, where the key generation is not dependent on one party, but this
    is not implemented here yet.)

    Note: if you need the latter functionality then you must use [Secret.share]
    and call [DHField.publish] on the resulting secret shares, rather than using
    [share] directly.
*)
module PublicShare (F: DHField): sig
  include Share with type t = F.t and type g = F.g
  module Secret: Share with type t = F.t and type g = F.t
end
