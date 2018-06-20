open Secret_sharing
open Secret_sharing.Gf2n

let gf256 =
  QCheck.map
    GF256.of_char
    QCheck.char

let gf256_suite = GF256.[
  QCheck.Test.make ~count:1000 ~name:"add_associativity"
    QCheck.(triple gf256 gf256 gf256)
    (fun (a, b, c) ->
       let open Infix in
       a + (b + c) = (a + b) + c);
  QCheck.Test.make ~count:1000 ~name:"mul_associativity"
    QCheck.(triple gf256 gf256 gf256)
    (fun (a, b, c) ->
       let open Infix in
       a * (b * c) = (a * b) * c);
  QCheck.Test.make ~count:1000 ~name:"add_commutativity"
    QCheck.(pair gf256 gf256)
    (fun (a, b) ->
       let open Infix in
       a + b = b + a);
  QCheck.Test.make ~count:1000 ~name:"mul_commutativity"
    QCheck.(pair gf256 gf256)
    (fun (a, b) ->
       let open Infix in
       a * b = b * a);
  QCheck.Test.make ~count:1000 ~name:"add_identity"
    gf256
    (fun a ->
       let open Infix in
       zero + a = a);
  QCheck.Test.make ~count:1000 ~name:"mul_identity"
    gf256
    (fun a ->
       let open Infix in
       one * a = a);
  QCheck.Test.make ~count:1000 ~name:"add_inverse"
    gf256
    (fun a ->
       let open Infix in
       a + a = zero);
  QCheck.Test.make ~count:1000 ~name:"mul_inverse"
    gf256
    (fun a ->
       let open Infix in
       a = zero || a * (one / a) = one);
  QCheck.Test.make ~count:1000 ~name:"distributivity"
    QCheck.(triple gf256 gf256 gf256)
    (fun (a, b, c) ->
       let open Infix in
       a * (b + c) = a * b + a * c);
]

let seed = QCheck.(string_of_size Gen.(return 16))

let g_of_seed seed =
  Nocrypto.Rng.(create ~seed:(Cstruct.of_string seed) (module Generators.Fortuna))

let perm_3_of_5 = QCheck.(triple
                           (int_range 0 4)
                           (int_range 1 4)
                           (int_range 2 4))

let swap a idx1 idx2 =
  let t = a.(idx1) in
  a.(idx1) <- a.(idx2);
  a.(idx2) <- t

let share_suite = [
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_all_identity"
    QCheck.(pair seed string)
    (fun (seed, s) ->
       let g = g_of_seed seed in
       let shares = Share.share ~g s 3 5 in
       Share.unshare shares = s);
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_first_3_identity"
    QCheck.(pair seed string)
    (fun (seed, s) ->
       let g = g_of_seed seed in
       let shares = Share.share ~g s 3 5 in
       Share.unshare (Array.sub shares 0 3) = s);
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_last_3_identity"
    QCheck.(pair seed string)
    (fun (seed, s) ->
       let g = g_of_seed seed in
       let shares = Share.share ~g s 3 5 in
       Share.unshare (Array.sub shares 2 3) = s);
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_3_random"
    QCheck.(triple seed string perm_3_of_5)
    (fun (seed, s, (idx1, idx2, idx3)) ->
       let g = g_of_seed seed in
       let shares = Share.share ~g s 3 5 in
       swap shares 0 idx1;
       swap shares 1 idx2;
       swap shares 2 idx3;
       let shares = Array.sub shares 0 3 in
       Share.unshare shares = s);
  QCheck.Test.make ~count:25 ~name:"share_random_params_unshare_all"
    QCheck.(triple seed string (pair (int_range 1 255) (int_range 1 255)))
    (fun (seed, s, (m, n)) ->
       let g = g_of_seed seed in
       let m = min m n
       and n = max m n in
       let shares = Share.share ~g s m n in
       Share.unshare shares = s);
]


let mk_share_gf2n_suite ?(count=20) (gf2n: (module GaloisField with type t = Z.t)) =
  let module GF2N = (val gf2n) in
  QCheck.[
    Test.make ~count:count ~name:"basic consistency check"
      (quad
         seed
         (string_of_size (Gen.return (GF2N.power / 8)))
         (int_range 1 100)
         (int_range 1 100))
      (fun (seed, secret, a, b) ->
         assume (a > 1);
         assume (b > 1);
         assume (String.length secret = GF2N.power / 8);
         let threshold = min a b in
         let nshares = max a b in
         let rng = Share.nocrypto_rng_gf2n ~g:(g_of_seed seed) GF2N.power in
         let module SecretShare = Share.SecretShare (GF2N) in
         let open SecretShare in
         let shares = share (GF2N.of_string secret) threshold nshares (Share.array_rng_of rng) () |> fst in
         let secret' = unshare shares |> GF2N.to_string in
         let shares' = extend' 2 shares (Share.uniq_rng_of rng) () |> fst in
         let shares'' = Array.(append shares' (sub shares 2 (length shares - 2))) in
         let secret'' = unshare shares'' |> GF2N.to_string in
         secret = secret' && secret' = secret''
      );
  ]


let _ = QCheck_runner.run_tests @@
  gf256_suite @
  share_suite @
  (mk_share_gf2n_suite (module GF2_8)) @
  (mk_share_gf2n_suite (module GF2_16)) @
  (mk_share_gf2n_suite ~count:10(module GF2_80)) @
  (mk_share_gf2n_suite ~count:4 (module GF2_128)) @
  (mk_share_gf2n_suite ~count:4 (module GF2_256)) @
  (mk_share_gf2n_suite ~count:2 (module GF2_720)) @
  (mk_share_gf2n_suite ~count:2 (module GF2_1536)) @
  (* e.g. this one takes around 12 seconds due to inefficiency in Zarith API,
     see gf2n.ml FIXME for details *)
  (mk_share_gf2n_suite ~count:1 (module GF2_4096))
