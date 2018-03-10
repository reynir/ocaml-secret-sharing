let (%) f g x = f (g x)
let check_arg b msg = if not b then invalid_arg msg else ()

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
  val power : int
  val irred_poly : t
end

module type GF2NP = sig
  val power: int
  val irred_poly: Z.t
end

module GF2N(P: GF2NP): GaloisField with type t = Z.t = struct
  open Z
  type t = Z.t
  include P

  let is_valid x = geq x zero && numbits x <= P.power
  let check_valid x = if is_valid x then x else
      invalid_arg (Printf.sprintf "invalid elem for GF2^%d: %s" P.power (Z.to_string x))

  let zero = Z.zero
  let one = Z.one
  let compare = Z.compare
  let of_int = check_valid % Z.of_int
  let of_string = check_valid % Z.of_bits
  let to_string t =
    (* GMP sometimes puts trailing zeros, strip them out here *)
    let open String in
    let n = Pervasives.(P.power / 8) in
    let s = Z.to_bits t in
    if length s = n then s
    else
      let r = sub s n Pervasives.(length s - n) in
      assert (split_on_char '\x00' r |> concat "" |> length = 0);
      sub s 0 n

  let add = Z.logxor
  let sub = Z.logxor

  (* FIXME: these are pretty inefficient for large P.degree because Zarith doesn't
     offer a mutable API. *)

  let mul x y =
    let b = ref x in
    let z = ref (if testbit y 0 then !b else zero) in
    for i = 1 to Pervasives.(P.power - 1) do
      b := shift_left !b 1;
      if testbit !b P.power then b := logxor !b P.irred_poly;
      if testbit y i then z := logxor !z !b;
    done;
    !z

  let swap x y = let z = !x in x := !y; y := z

  let inv x =
    if equal x zero then raise Division_by_zero;
    let u = ref x in
    let v = ref P.irred_poly in
    let g = ref zero in
    let z = ref one in
    let h = ref zero in
    let i = ref 0 in
    while (not @@ equal !u one) do
      i := Pervasives.(-) (numbits !u) (numbits !v);
      if !i < 0 then begin
        swap u v;
        swap z g;
        i := Pervasives.(~-) (!i);
      end;
      h := shift_left !v !i;
      u := logxor !u !h;
      h := shift_left !g !i;
      z := logxor !z !h;
    done;
    !z

  let div a b = mul a (inv b)

  module Infix = struct
    let ( + ) e = add e
    let ( - ) e = sub e
    let ( * ) e = mul e
    let ( / ) e = div e
  end
end

let gf2np coeffs =
  let open List in
  let pow = hd coeffs in
  let g = Array.make Pervasives.(pow + 1) 0 in
  iter (fun i -> Array.set g i 1) (0 :: coeffs);
  let px =
    Array.to_list g |>
    map string_of_int |>
    rev |>
    String.concat "" in
  (* print_endline @@ (string_of_int deg) ^ ":" ^ px; *)
  let p = px |>
    Z.of_string_base 2 in
  (module struct let power = pow let irred_poly = p end: GF2NP)

(*
below numbers are from
Table of Low-Weight Binary Irreducible Polynomials
Gadiel Seroussi
Computer Systems Laboratory
HPL-98-135
August, 1998
*)
module GF2_8 = GF2N(val gf2np [8;4;3;1])
module GF2_16 = GF2N(val gf2np [16;5;3;1])
module GF2_24 = GF2N(val gf2np [24;4;3;1])
module GF2_32 = GF2N(val gf2np [32;7;3;2])
module GF2_40 = GF2N(val gf2np [40;5;4;3])
module GF2_48 = GF2N(val gf2np [48;5;3;2])
module GF2_56 = GF2N(val gf2np [56;7;4;2])
module GF2_64 = GF2N(val gf2np [64;4;3;1])
module GF2_72 = GF2N(val gf2np [72;10;9;3])
module GF2_80 = GF2N(val gf2np [80;9;4;2])
module GF2_88 = GF2N(val gf2np [88;7;6;2])
module GF2_96 = GF2N(val gf2np [96;10;9;6])
module GF2_104 = GF2N(val gf2np [104;4;3;1])
module GF2_112 = GF2N(val gf2np [112;5;4;3])
module GF2_120 = GF2N(val gf2np [120;4;3;1])
module GF2_128 = GF2N(val gf2np [128;7;2;1])
module GF2_136 = GF2N(val gf2np [136;5;3;2])
module GF2_144 = GF2N(val gf2np [144;7;4;2])
module GF2_152 = GF2N(val gf2np [152;6;3;2])
module GF2_160 = GF2N(val gf2np [160;5;3;2])
module GF2_168 = GF2N(val gf2np [168;15;3;2])
module GF2_176 = GF2N(val gf2np [176;11;3;2])
module GF2_184 = GF2N(val gf2np [184;9;8;7])
module GF2_192 = GF2N(val gf2np [192;7;2;1])
module GF2_200 = GF2N(val gf2np [200;5;3;2])
module GF2_208 = GF2N(val gf2np [208;9;3;1])
module GF2_216 = GF2N(val gf2np [216;7;3;1])
module GF2_224 = GF2N(val gf2np [224;9;8;3])
module GF2_232 = GF2N(val gf2np [232;9;4;2])
module GF2_240 = GF2N(val gf2np [240;8;5;3])
module GF2_248 = GF2N(val gf2np [248;15;14;10])
module GF2_256 = GF2N(val gf2np [256;10;5;2])
module GF2_264 = GF2N(val gf2np [264;9;6;2])
module GF2_272 = GF2N(val gf2np [272;9;3;2])
module GF2_280 = GF2N(val gf2np [280;9;5;2])
module GF2_288 = GF2N(val gf2np [288;11;10;1])
module GF2_296 = GF2N(val gf2np [296;7;3;2])
module GF2_304 = GF2N(val gf2np [304;11;2;1])
module GF2_312 = GF2N(val gf2np [312;9;7;4])
module GF2_320 = GF2N(val gf2np [320;4;3;1])
module GF2_328 = GF2N(val gf2np [328;8;3;1])
module GF2_336 = GF2N(val gf2np [336;7;4;1])
module GF2_344 = GF2N(val gf2np [344;7;2;1])
module GF2_352 = GF2N(val gf2np [352;13;11;6])
module GF2_360 = GF2N(val gf2np [360;5;3;2])
module GF2_368 = GF2N(val gf2np [368;7;3;2])
module GF2_376 = GF2N(val gf2np [376;8;7;5])
module GF2_384 = GF2N(val gf2np [384;12;3;2])
module GF2_392 = GF2N(val gf2np [392;13;10;6])
module GF2_400 = GF2N(val gf2np [400;5;3;2])
module GF2_408 = GF2N(val gf2np [408;5;3;2])
module GF2_416 = GF2N(val gf2np [416;9;5;2])
module GF2_424 = GF2N(val gf2np [424;9;7;2])
module GF2_432 = GF2N(val gf2np [432;13;4;3])
module GF2_440 = GF2N(val gf2np [440;4;3;1])
module GF2_448 = GF2N(val gf2np [448;11;6;4])
module GF2_456 = GF2N(val gf2np [456;18;9;6])
module GF2_464 = GF2N(val gf2np [464;19;18;13])
module GF2_472 = GF2N(val gf2np [472;11;3;2])
module GF2_480 = GF2N(val gf2np [480;15;9;6])
module GF2_488 = GF2N(val gf2np [488;4;3;1])
module GF2_496 = GF2N(val gf2np [496;16;5;2])
module GF2_504 = GF2N(val gf2np [504;15;14;6])
module GF2_512 = GF2N(val gf2np [512;8;5;2])
module GF2_520 = GF2N(val gf2np [520;15;11;2])
module GF2_528 = GF2N(val gf2np [528;11;6;2])
module GF2_536 = GF2N(val gf2np [536;7;5;3])
module GF2_544 = GF2N(val gf2np [544;8;3;1])
module GF2_552 = GF2N(val gf2np [552;19;16;9])
module GF2_560 = GF2N(val gf2np [560;11;9;6])
module GF2_568 = GF2N(val gf2np [568;15;7;6])
module GF2_576 = GF2N(val gf2np [576;13;4;3])
module GF2_584 = GF2N(val gf2np [584;14;13;3])
module GF2_592 = GF2N(val gf2np [592;13;6;3])
module GF2_600 = GF2N(val gf2np [600;9;5;2])
module GF2_608 = GF2N(val gf2np [608;19;13;6])
module GF2_616 = GF2N(val gf2np [616;19;10;3])
module GF2_624 = GF2N(val gf2np [624;11;6;5])
module GF2_632 = GF2N(val gf2np [632;9;2;1])
module GF2_640 = GF2N(val gf2np [640;14;3;2])
module GF2_648 = GF2N(val gf2np [648;13;3;1])
module GF2_656 = GF2N(val gf2np [656;7;5;4])
module GF2_664 = GF2N(val gf2np [664;11;9;8])
module GF2_672 = GF2N(val gf2np [672;11;6;5])
module GF2_680 = GF2N(val gf2np [680;23;16;9])
module GF2_688 = GF2N(val gf2np [688;19;14;6])
module GF2_696 = GF2N(val gf2np [696;23;10;2])
module GF2_704 = GF2N(val gf2np [704;8;3;2])
module GF2_712 = GF2N(val gf2np [712;5;4;3])
module GF2_720 = GF2N(val gf2np [720;9;6;4])
module GF2_728 = GF2N(val gf2np [728;4;3;2])
module GF2_736 = GF2N(val gf2np [736;13;8;6])
module GF2_744 = GF2N(val gf2np [744;13;11;1])
module GF2_752 = GF2N(val gf2np [752;13;10;3])
module GF2_760 = GF2N(val gf2np [760;11;6;5])
module GF2_768 = GF2N(val gf2np [768;19;17;4])
module GF2_776 = GF2N(val gf2np [776;15;14;7])
module GF2_784 = GF2N(val gf2np [784;13;9;6])
module GF2_792 = GF2N(val gf2np [792;9;7;3])
module GF2_800 = GF2N(val gf2np [800;9;7;1])
module GF2_808 = GF2N(val gf2np [808;14;3;2])
module GF2_816 = GF2N(val gf2np [816;11;8;2])
module GF2_824 = GF2N(val gf2np [824;11;6;4])
module GF2_832 = GF2N(val gf2np [832;13;5;2])
module GF2_840 = GF2N(val gf2np [840;11;5;1])
module GF2_848 = GF2N(val gf2np [848;11;4;1])
module GF2_856 = GF2N(val gf2np [856;19;10;3])
module GF2_864 = GF2N(val gf2np [864;21;10;6])
module GF2_872 = GF2N(val gf2np [872;13;3;1])
module GF2_880 = GF2N(val gf2np [880;15;7;5])
module GF2_888 = GF2N(val gf2np [888;19;18;10])
module GF2_896 = GF2N(val gf2np [896;7;5;3])
module GF2_904 = GF2N(val gf2np [904;12;7;2])
module GF2_912 = GF2N(val gf2np [912;7;5;1])
module GF2_920 = GF2N(val gf2np [920;14;9;6])
module GF2_928 = GF2N(val gf2np [928;10;3;2])
module GF2_936 = GF2N(val gf2np [936;15;13;12])
module GF2_944 = GF2N(val gf2np [944;12;11;9])
module GF2_952 = GF2N(val gf2np [952;16;9;7])
module GF2_960 = GF2N(val gf2np [960;12;9;3])
module GF2_968 = GF2N(val gf2np [968;9;5;2])
module GF2_976 = GF2N(val gf2np [976;17;10;6])
module GF2_984 = GF2N(val gf2np [984;24;9;3])
module GF2_992 = GF2N(val gf2np [992;17;15;13])
module GF2_1000 = GF2N(val gf2np [1000;5;4;3])
module GF2_1008 = GF2N(val gf2np [1008;19;17;8])
module GF2_1016 = GF2N(val gf2np [1016;15;6;3])
module GF2_1024 = GF2N(val gf2np [1024;19;6;1])

module GF2_1536 = GF2N(val gf2np [1536;21;6;2])
module GF2_2048 = GF2N(val gf2np [2048;19;14;13])
module GF2_3072 = GF2N(val gf2np [3072;11;10;5])
module GF2_4096 = GF2N(val gf2np [4096;27;15;1])

module GF256Slow = struct
  include GF2_8
  let of_char = Z.of_int % int_of_char
  let to_char = char_of_int % Z.to_int
end

(* See https://tools.ietf.org/html/draft-mcgrew-tss-03 *)
module GF256 = struct
  type t = int

  let zero = 0
  let one = 1
  let compare = compare

  let of_int x = check_arg (0 <= x && x < 256) "GF256.of_int: need 0 <= x < 256"; x
  let of_char = int_of_char
  let to_char = char_of_int
  let of_string s = if String.length s <> 1 then
      invalid_arg "GF256.of_string: invalid length" else of_char (String.get s 1)
  let to_string = String.make 1 % to_char

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
    then raise Division_by_zero
    else log_table.(x)

  let exp x =
    exp_table.(x)

  let mul x y =
    match x, y with
    | 0, _ | _, 0 -> 0
    | x, y -> exp ((log x + log y) mod 255)

  let div x y =
    if y = 0
    then raise Division_by_zero
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
