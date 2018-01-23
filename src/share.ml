let (%) f g x = f (g x)
let second f (x, y) = (x, f y)
let pair x y = x, y

module type Field = sig
  type t

  val size : int
  val zero : t
  val one : t

  val of_int : int -> t

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

(* See https://tools.ietf.org/html/draft-mcgrew-tss-03 *)
module GF256 = struct
  type t = int

  let size = 256
  let zero = 0
  let one = 1

  let of_int x = x & 0xff
  let of_char = int_of_char
  let to_char = char_of_int
  let of_string s = Array.init (String.length s) (int_of_char % String.get s)
  let to_string s = String.init (Array.length s) (char_of_int % Array.get s)

  (* Table taken from https://tools.ietf.org/html/draft-mcgrew-tss-03 *)
  let exp_table = [|
    0x01; 0x03; 0x05; 0x0f; 0x11; 0x33; 0x55; 0xff;
    0x1a; 0x2e; 0x72; 0x96; 0xa1; 0xf8; 0x13; 0x35;
    0x5f; 0xe1; 0x38; 0x48; 0xd8; 0x73; 0x95; 0xa4;
    0xf7; 0x02; 0x06; 0x0a; 0x1e; 0x22; 0x66; 0xaa;
    0xe5; 0x34; 0x5c; 0xe4; 0x37; 0x59; 0xeb; 0x26;
    0x6a; 0xbe; 0xd9; 0x70; 0x90; 0xab; 0xe6; 0x31;
    0x53; 0xf5; 0x04; 0x0c; 0x14; 0x3c; 0x44; 0xcc;
    0x4f; 0xd1; 0x68; 0xb8; 0xd3; 0x6e; 0xb2; 0xcd;
    0x4c; 0xd4; 0x67; 0xa9; 0xe0; 0x3b; 0x4d; 0xd7;
    0x62; 0xa6; 0xf1; 0x08; 0x18; 0x28; 0x78; 0x88;
    0x83; 0x9e; 0xb9; 0xd0; 0x6b; 0xbd; 0xdc; 0x7f;
    0x81; 0x98; 0xb3; 0xce; 0x49; 0xdb; 0x76; 0x9a;
    0xb5; 0xc4; 0x57; 0xf9; 0x10; 0x30; 0x50; 0xf0;
    0x0b; 0x1d; 0x27; 0x69; 0xbb; 0xd6; 0x61; 0xa3;
    0xfe; 0x19; 0x2b; 0x7d; 0x87; 0x92; 0xad; 0xec;
    0x2f; 0x71; 0x93; 0xae; 0xe9; 0x20; 0x60; 0xa0;
    0xfb; 0x16; 0x3a; 0x4e; 0xd2; 0x6d; 0xb7; 0xc2;
    0x5d; 0xe7; 0x32; 0x56; 0xfa; 0x15; 0x3f; 0x41;
    0xc3; 0x5e; 0xe2; 0x3d; 0x47; 0xc9; 0x40; 0xc0;
    0x5b; 0xed; 0x2c; 0x74; 0x9c; 0xbf; 0xda; 0x75;
    0x9f; 0xba; 0xd5; 0x64; 0xac; 0xef; 0x2a; 0x7e;
    0x82; 0x9d; 0xbc; 0xdf; 0x7a; 0x8e; 0x89; 0x80;
    0x9b; 0xb6; 0xc1; 0x58; 0xe8; 0x23; 0x65; 0xaf;
    0xea; 0x25; 0x6f; 0xb1; 0xc8; 0x43; 0xc5; 0x54;
    0xfc; 0x1f; 0x21; 0x63; 0xa5; 0xf4; 0x07; 0x09;
    0x1b; 0x2d; 0x77; 0x99; 0xb0; 0xcb; 0x46; 0xca;
    0x45; 0xcf; 0x4a; 0xde; 0x79; 0x8b; 0x86; 0x91;
    0xa8; 0xe3; 0x3e; 0x42; 0xc6; 0x51; 0xf3; 0x0e;
    0x12; 0x36; 0x5a; 0xee; 0x29; 0x7b; 0x8d; 0x8c;
    0x8f; 0x8a; 0x85; 0x94; 0xa7; 0xf2; 0x0d; 0x17;
    0x39; 0x4b; 0xdd; 0x7c; 0x84; 0x97; 0xa2; 0xfd;
    0x1c; 0x24; 0x6c; 0xb4; 0xc7; 0x52; 0xf6; 0x00;
  |]

  (* Table taken from https://tools.ietf.org/html/draft-mcgrew-tss-03 *)
  let log_table = [|
    0000; 0000; 0025; 0001; 0050; 0002; 0026; 0198;
    0075; 0199; 0027; 0104; 0051; 0238; 0223; 0003;
    0100; 0004; 0224; 0014; 0052; 0141; 0129; 0239;
    0076; 0113; 0008; 0200; 0248; 0105; 0028; 0193;
    0125; 0194; 0029; 0181; 0249; 0185; 0039; 0106;
    0077; 0228; 0166; 0114; 0154; 0201; 0009; 0120;
    0101; 0047; 0138; 0005; 0033; 0015; 0225; 0036;
    0018; 0240; 0130; 0069; 0053; 0147; 0218; 0142;
    0150; 0143; 0219; 0189; 0054; 0208; 0206; 0148;
    0019; 0092; 0210; 0241; 0064; 0070; 0131; 0056;
    0102; 0221; 0253; 0048; 0191; 0006; 0139; 0098;
    0179; 0037; 0226; 0152; 0034; 0136; 0145; 0016;
    0126; 0110; 0072; 0195; 0163; 0182; 0030; 0066;
    0058; 0107; 0040; 0084; 0250; 0133; 0061; 0186;
    0043; 0121; 0010; 0021; 0155; 0159; 0094; 0202;
    0078; 0212; 0172; 0229; 0243; 0115; 0167; 0087;
    0175; 0088; 0168; 0080; 0244; 0234; 0214; 0116;
    0079; 0174; 0233; 0213; 0231; 0230; 0173; 0232;
    0044; 0215; 0117; 0122; 0235; 0022; 0011; 0245;
    0089; 0203; 0095; 0176; 0156; 0169; 0081; 0160;
    0127; 0012; 0246; 0111; 0023; 0196; 0073; 0236;
    0216; 0067; 0031; 0045; 0164; 0118; 0123; 0183;
    0204; 0187; 0062; 0090; 0251; 0096; 0177; 0134;
    0059; 0082; 0161; 0108; 0170; 0085; 0041; 0157;
    0151; 0178; 0135; 0144; 0097; 0190; 0220; 0252;
    0188; 0149; 0207; 0205; 0055; 0063; 0091; 0209;
    0083; 0057; 0132; 0060; 0065; 0162; 0109; 0071;
    0020; 0042; 0158; 0093; 0086; 0242; 0211; 0171;
    0068; 0017; 0146; 0217; 0035; 0032; 0046; 0137;
    0180; 0124; 0184; 0038; 0119; 0153; 0227; 0165;
    0103; 0074; 0237; 0222; 0197; 0049; 0254; 0024;
    0013; 0099; 0140; 0128; 0192; 0247; 0112; 0007;
  |]

  let add x y =
    x lxor y

  let sub = add

  let log x =
    if x = 0
    then raise (Invalid_argument "zero divisor")
    else log_table.(x)

  let exp x =
    exp_table.(x)

  let mul x y =
    match x, y with
    | 0, _ | _, 0 -> 0
    | x, y -> exp ((log x + log y) mod 255)

  let div x y =
    if y = 0
    then raise (Invalid_argument "zero divisor")
    else if x = 0
    then 0
    else exp ((255 + log x - log y) mod 255)

  module Infix = struct
    let ( + ) e = add e
    let ( - ) e = sub e
    let ( * ) e = mul e
    let ( / ) e = div e
  end
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

  let share secret threshold shares rng s =
    assert (shares < F.size);
    assert (threshold <= shares);
    assert (threshold > 0);
    (* Use 1,..., n as indices *)
    let a, s = Poly.coefficients secret threshold rng s in
    Array.init shares succ
    |> Array.map F.of_int
    (* For each index compute the polynomial and return the point *)
    |> Array.map (fun x -> x, Poly.eval a x), s

  let unshare shares =
    (* u is the indices *)
    let u = Array.map fst shares in
    (* v is the shares *)
    let v = Array.map snd shares in
    Poly.interpolate u v F.zero

  let extend xs shares =
    let u = Array.map fst shares in
    let v = Array.map snd shares in
    let ys = Array.map (Poly.interpolate u v) xs in
    Array.map2 pair xs ys

  let extend' more shares rng s =
    let xs, s = rng more s in
    extend xs shares, s

  let share_array secret threshold shares rng s =
    assert (shares < F.size);
    assert (threshold <= shares);
    assert (threshold > 0);
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
    (* u is the indices *)
    let u = Array.map fst shares
    (* v is the share strings *)
    and v = Array.map snd shares in
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
    let u = Array.map fst shares
    and v = Array.map snd shares in
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

  let extend_array' more shares rng s =
    let xs, s = rng more s in
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
  val extend' : int -> shares -> (int -> (t array, 's) rng) -> (shares, 's) rng

  val share_array : t array -> int -> int -> (int -> (t array, 's) rng) -> (array_shares, 's) rng
  val unshare_array : array_shares -> g array
  val extend_array : t array -> array_shares -> array_shares
  val extend_array' : int -> array_shares -> (int -> (t array, 's) rng) -> (array_shares, 's) rng
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

(* not actually pure but lets us reuse some code *)
let nocrypto_rng ?g n () =
  let chars = Nocrypto.Rng.generate ?g n in
  Array.init n (Cstruct.get_uint8 chars), ()

let share ?g secret threshold shares =
  SecretShare_GF256.share_array (GF256.of_string secret) threshold shares
    (nocrypto_rng ?g) () |> fst
  |> Array.map (second GF256.to_string)

let unshare shares =
  shares
  |> Array.map (second GF256.of_string)
  |> SecretShare_GF256.unshare_array |> GF256.to_string

let extend ?g more shares =
  let shares' = Array.map (second GF256.of_string) shares in
  SecretShare_GF256.extend_array' more shares'
    (nocrypto_rng ?g) () |> fst
  |> Array.map (second GF256.to_string)

let share_byte ?g secret threshold shares =
  SecretShare_GF256.share (GF256.of_char secret) threshold shares
    (nocrypto_rng ?g) () |> fst

let unshare_byte shares = SecretShare_GF256.unshare shares |> GF256.to_char

let extend_byte ?g more shares =
  SecretShare_GF256.extend' more shares
    (nocrypto_rng ?g) () |> fst
