let (%) f g x = f (g x)
let first f (x, y) = (f x, y)
let second f (x, y) = (x, f y)
let pair x y = x, y
let check_arg b msg = if not b then invalid_arg msg else ()

open Gf2n

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

module Polynomial (F: Field) = struct
  let sum a =
    Array.fold_left
      F.add
      F.zero
      a

  let product a =
    Array.fold_left
      F.mul
      F.one
      a

  let eval (a : F.t array) (x : F.t) =
    (* compute a.(0) + x * (a.(1) + x * (... + x * (a.(m)) ...)) *)
    let rec loop i a x acc =
      let open F in
      if i = 0
      then add a.(0) (mul x acc)
      else loop (i-1) a x (add a.(i) (mul x acc)) in
    loop (Array.length a - 1) a x F.zero

  (* Generate coefficients a.(0), ..., a.(m-1) such that a.(0) is [secret], and
   * a.(1), ..., a.(m-1) are random. *)
  let coefficients secret threshold rng s =
    let elts, s = rng (threshold - 1) s in
    let a = Array.init threshold
        (fun i ->
           if i = 0
           then secret
           else Array.get elts (i-1)) in
    assert (Array.length a = threshold);
    a, s

  let lagrange_basis i u x =
    let open F.Infix in
    Array.mapi
      (fun j u_j ->
         if i = j
         then F.one
         else (u_j - x) / (u_j - u.(i)))
      u
    |> product

  let interpolate u v x =
    let open F.Infix in
    Array.mapi
      (fun i v_i ->
         lagrange_basis i u x * v_i)
      v
    |> sum
end

module DHPolynomial (F: DHField) = struct
  include Polynomial (F)
  let sum_g a =
    Array.fold_left
      F.add_g
      F.zero_g
      a

  let eval_g a x = eval a x |> F.publish

  let interpolate_g u v x =
    Array.mapi
      (fun i v_i ->
         F.mul_g (lagrange_basis i u x) v_i)
      v
    |> sum_g
end

type ('a, 's) rng = 's -> ('a * 's)

(* Array.init but f is a state-transition not a function *)
let array_s_init n f s =
  if n = 0 then [||], s else
    let a0, s0 = f 0 s in
    let arr = Array.make n a0 in
    let s' = ref s0 in
    for i = 1 to n-1 do
      let a, next_s = f i !s' in
      Array.set arr i a;
      s' := next_s
    done;
    arr, !s'

(* Transpose a 2-d matrix, assumed non-empty *)
let array_transpose v =
  Array.init (Array.length v.(0))
    (fun i ->
       Array.map (fun s -> s.(i)) v)

module GenericShare (Poly: sig
    module F: Field
    type g
    val coefficients : F.t -> int -> (int -> (F.t array, 's) rng) -> (F.t array, 's) rng
    val eval : F.t array -> F.t -> g
    val interpolate : F.t array -> g array -> F.t -> g
  end) = struct
  open Poly
  type t = F.t
  type g = Poly.g
  type shares = (t * g) array
  type array_shares = (t * g array) array

  let check_unique_shares xs =
    let l = Array.to_list xs in
    List.(sort_uniq F.compare l |> length = length l)

  let share secret threshold shares rng s =
    ignore (F.of_int shares);
    check_arg (threshold <= shares) "GenericShare.share: need threshold <= shares";
    check_arg (threshold > 0) "GenericShare.share: need threshold > 0";
    (* Use 1,..., n as indices *)
    let a, s = Poly.coefficients secret threshold rng s in
    Array.init shares succ
    |> Array.map F.of_int
    (* For each index compute the polynomial and return the point *)
    |> Array.map (fun x -> x, Poly.eval a x), s

  let unshare shares =
    ignore (F.of_int (Array.length shares));
    (* u is the indices *)
    let u = Array.map fst shares in
    check_arg (check_unique_shares u) "shares are not distinct";
    (* v is the shares *)
    let v = Array.map snd shares in
    Poly.interpolate u v F.zero

  let extend xs shares =
    ignore (F.of_int (Array.length shares));
    let u = Array.map fst shares in
    check_arg (Array.(append xs u) |> check_unique_shares) "shares are not distinct";
    let v = Array.map snd shares in
    let ys = Array.map (Poly.interpolate u v) xs in
    Array.map2 pair xs ys

  let extend' more shares rng_uniq s =
    let xs, s = rng_uniq more Array.(map fst shares) s in
    extend xs shares, s

  let share_array secret threshold shares rng s =
    ignore (F.of_int shares);
    check_arg (threshold <= shares) "GenericShare.share_array: need threshold <= shares";
    check_arg (threshold > 0) "GenericShare.share: need threshold > 0";
    (* Use 1, ..., n as indices *)
    let xs = Array.init shares succ in
    (* Generate coefficients for a polynomial for each character in the secret. *)
    let as_, s =
      array_s_init (Array.length secret) (fun i s ->
          (* Compute the secrets for each index *)
          Poly.coefficients secret.(i) threshold rng s) s in
    Array.map (fun x ->
        let x = F.of_int x in
        x,
        Array.init (Array.length secret)
          (fun i -> Poly.eval as_.(i) x))
      xs, s

  let unshare_array shares =
    ignore (F.of_int (Array.length shares));
    (* u is the indices *)
    let u = Array.map fst shares in
    check_arg (check_unique_shares u) "shares are not distinct";
    (* v is the share strings *)
    let v = Array.map snd shares in
    (* Diagonalize the share string array so the resulting array consists of
     * arrays that each consist of shares of the same character, i.e.
     * vs.(j).(i) := v.(i).[j] for all i, j *)
    let vs = array_transpose v in
    Array.init (Array.length vs)
      (fun i ->
         (* Zip each one-byte share with its index *)
         let zipped =
           Array.init (Array.length u)
             (fun j -> u.(j), vs.(i).(j)) in
         unshare zipped)

  let extend_array xs shares =
    ignore (F.of_int (Array.length shares));
    let u = Array.map fst shares in
    check_arg (Array.(append xs u) |> check_unique_shares) "shares are not distinct";
    let v = Array.map snd shares in
    let vs = array_transpose v in
    Array.init (Array.length vs)
      (fun i ->
         (* Zip each one-byte share with its index *)
         let zipped =
           Array.init (Array.length u)
             (fun j -> u.(j), vs.(i).(j)) in
         extend xs zipped |> Array.map snd)
    |> array_transpose
    |> Array.map2 pair xs

  let extend_array' more shares rng_uniq s =
    let xs, s = rng_uniq more Array.(map fst shares) s in
    extend_array xs shares, s
end

module type Share = sig
  type t (* master secret / share index *)
  type g (* share / recovered secret *)
  type shares = (t * g) array
  type array_shares = (t * g array) array

  val share : t -> int -> int -> (int -> (t array, 's) rng) -> (shares, 's) rng
  val unshare : shares -> g
  val extend : t array -> shares -> shares
  val extend' : int -> shares -> (int -> t array -> (t array, 's) rng) -> (shares, 's) rng

  val share_array : t array -> int -> int -> (int -> (t array, 's) rng) -> (array_shares, 's) rng
  val unshare_array : array_shares -> g array
  val extend_array : t array -> array_shares -> array_shares
  val extend_array' : int -> array_shares -> (int -> t array -> (t array, 's) rng) -> (array_shares, 's) rng
end

module SecretShare (F: Field) = struct
  include GenericShare (struct
      include Polynomial (F)
      module F = F
      type g = F.t
    end)
end

module PublicShare (F: DHField) = struct
  include GenericShare (struct
      include DHPolynomial (F)
      module F = F
      type g = F.g
      let eval a x = eval_g a x
      let interpolate u v x = interpolate_g u v x
    end)

  module Secret = SecretShare (F)
end

module SecretShare_GF256 = SecretShare (GF256)

let string_to_array s = Array.init (String.length s) (GF256.of_char % String.get s)
let array_to_string s = String.init (Array.length s) (GF256.to_char % Array.get s)

let array_rng_of rng n s =
  let rs = ref s in
  let results = Array.init n (fun _ ->
      let r, s = rng !rs in
      rs := s; r) in
  results, !rs

let uniq_rng_of rng more banned s =
  let rec get_1_more accum s =
    let next, s = rng s in
    if Array.mem next banned || List.mem next accum then
      get_1_more accum s
    else
      next, s in
  let rec get_n_more n accum s = match n with
    | 0 -> accum, s
    | n -> let elem, s = get_1_more accum s in
      get_n_more (n-1) (elem::accum) s in
  get_n_more more [] s |> first Array.of_list

(* not actually pure but lets us reuse some code *)
let nocrypto_array_rng_gf256 ?g n () =
  let chars = Nocrypto.Rng.generate ?g n in
  Array.init n (GF256.of_int % Cstruct.get_uint8 chars), ()

let nocrypto_uniq_rng_gf256 ?g =
  let rng () =
    let chars = Nocrypto.Rng.generate ?g 1 in
    GF256.of_int @@ Cstruct.get_uint8 chars 0, () in
  uniq_rng_of rng

let nocrypto_rng_gf2n ?g deg () =
  Nocrypto.Rng.generate ?g (deg / 8) |>
  Cstruct.to_bytes |>
  Bytes.to_string |>
  Z.of_bits, ()

let share ?g secret threshold shares =
  SecretShare_GF256.share_array (string_to_array secret) threshold shares
    (nocrypto_array_rng_gf256 ?g) () |> fst
  |> Array.map (second array_to_string)

let unshare shares =
  shares
  |> Array.map (second string_to_array)
  |> SecretShare_GF256.unshare_array |> array_to_string

let extend ?g more shares =
  let shares' = Array.map (second string_to_array) shares in
  SecretShare_GF256.extend_array' more shares'
    (nocrypto_uniq_rng_gf256 ?g) () |> fst
  |> Array.map (second array_to_string)

let share_byte ?g secret threshold shares =
  SecretShare_GF256.share (GF256.of_char secret) threshold shares
    (nocrypto_array_rng_gf256 ?g) () |> fst

let unshare_byte shares = SecretShare_GF256.unshare shares |> GF256.to_char

let extend_byte ?g more shares =
  SecretShare_GF256.extend' more shares
    (nocrypto_uniq_rng_gf256 ?g) () |> fst
