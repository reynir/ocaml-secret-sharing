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

let rng = QCheck.(map 
                    (fun seed ->
                       let seed = Cstruct.of_string seed in
                       Nocrypto.Rng.create ~seed (module Nocrypto.Rng.Generators.Fortuna))
                    (string_of_size Gen.(return 16)))

let share_suite = [
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_all_identity"
    QCheck.(pair rng string)
    (fun (g, s) ->
       let shares = Share.share ~g s 3 5 in
       Share.unshare shares = s);
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_first_3_identity"
    QCheck.(pair rng string)
    (fun (g, s) ->
       let shares = Share.share ~g s 3 5 in
       Share.unshare (Array.sub shares 0 3) = s);
  QCheck.Test.make ~count:200 ~name:"share_3x5_unshare_last_3_identity"
    QCheck.(pair rng string)
    (fun (g, s) ->
       let shares = Share.share ~g s 3 5 in
       Share.unshare (Array.sub shares 2 3) = s);
]


let _ =
  QCheck_runner.run_tests
    gf256_suite
let _ =
  QCheck_runner.run_tests
    share_suite
