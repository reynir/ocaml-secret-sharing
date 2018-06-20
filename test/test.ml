open Secret_sharing
open Secret_sharing.Gf2n

let () = Printexc.record_backtrace true
let () = Nocrypto_entropy_unix.initialize ()
let threshold, nshares = 2, 3
let secret = 'x'
let shares = Share.share_byte secret threshold nshares
let () = Printf.printf "Number of shares: %d\n" (Array.length shares)
let secret' = Share.unshare_byte (Array.sub shares 0 threshold)
let () = Printf.printf "Secret %C, computed secret %C\n" secret secret'
let secret' = Share.unshare_byte shares
let () = Printf.printf "Secret %C, computed secret %C\n" secret secret'

let secret = '\x74'
let shares = [| 1, 0xB9; 2, 0xF5 |]
           |> Array.map (fun (x, s) ->
                 Gf2n.GF256.(of_char (char_of_int x), of_char (char_of_int s)))
let secret' = Share.unshare_byte shares
let () = Printf.printf "Secret %C, computed secret %C\n" secret secret'

let secret = "Hello, World!"
let shares = Share.share secret threshold nshares
let secret' = Share.unshare shares
let () = Printf.printf "Secret %S, computed secret %S\n" secret secret'

let secret = "\x74\x65\x73\x74\x00"
let shares = [|
  1, "\xB9\xFA\x07\xE1\x85";
  2, "\xF5\x40\x9B\x45\x11";
|] |> Array.map (fun (x, s) ->
    Gf2n.GF256.of_int x, s)
let secret' = Share.unshare shares
let () = Printf.printf "Secret %S, computed secret %S\n" secret secret'

let shares' = Share.extend 2 shares
let secret' = Share.unshare shares'
let () = Printf.printf "Secret %S, re-computed secret %S\n" secret secret'

let g_of_seed seed =
  Nocrypto.Rng.(create ~seed:(Cstruct.of_string seed) (module Generators.Fortuna))

(*
("\001\226\253H \0190\025o\233\168c*\237\239\014", "\208", 43, 43)
*)
let print_shares to_string shares = shares |> Array.iter (fun (x, y) ->
    Printf.printf "(%S, %S)\n" (to_string x) (to_string y))

module SecretShare_GF2_8 = Share.SecretShare (GF2_8)
let rng = Share.nocrypto_rng_gf2n ~g:(g_of_seed "") 8
let secret = "\237"
let shares = SecretShare_GF2_8.share (GF2_8.of_string secret) threshold nshares (Share.array_rng_of rng) () |> fst
let secret' = SecretShare_GF2_8.unshare shares |> GF2_8.to_string
let () = Printf.printf "Secret %S, computed secret %S\n" secret secret'
let shares' = SecretShare_GF2_8.extend' 2 shares (Share.uniq_rng_of rng) () |> fst
let shares'' = Array.(append shares' (sub shares 2 (length shares - 2)))
let secret'' = SecretShare_GF2_8.unshare shares'' |> GF2_8.to_string
let () = Printf.printf "Secret %S, re-computed secret %S\n" secret secret''

module SecretShare_GF2_512 = Share.SecretShare (GF2_512)
let rng = Share.nocrypto_rng_gf2n 512
let secret = "1Tv8EutK/YXjfST5A6REUNea3jtGQLcbrD9/33nzVyMM6OJTEnDfczCnXERaRZyr"
let shares = SecretShare_GF2_512.share (GF2_512.of_string secret) threshold nshares (Share.array_rng_of rng) () |> fst
let secret' = SecretShare_GF2_512.unshare shares |> GF2_512.to_string
let () = Printf.printf "Secret %S, computed secret %S\n" secret secret'
let shares' = SecretShare_GF2_512.extend' 2 shares (Share.uniq_rng_of rng) () |> fst
let shares'' = Array.(append shares' (sub shares 2 (length shares - 2)))
let secret'' = SecretShare_GF2_512.unshare shares'' |> GF2_512.to_string
let () = Printf.printf "Secret %S, re-computed secret %S\n" secret secret'
