let gf256 =
  QCheck.map
    Share.GF256.of_char
    QCheck.char

let gf256_suite = Share.GF256.[
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


let _ =
  QCheck_runner.run_tests
    gf256_suite
let _ =
  QCheck_runner.run_tests
    share_suite
