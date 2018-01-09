let () = Printexc.record_backtrace true
let () = Nocrypto_entropy_unix.initialize ()
let secret = 'x'
let threshold, nshares = 2, 3
let shares = Share.share_byte secret threshold nshares
let () = Printf.printf "Number of shares: %d\n" (Array.length shares)
let () = Array.iter (fun (x, f_x) -> Printf.printf "%d -> %d\n" x f_x) shares
let secret' = Share.unshare_byte (Array.sub shares 0 threshold)
let () = Printf.printf "Secret %C, computed secret %C\n" secret secret'
let secret' = Share.unshare_byte shares
let () = Printf.printf "Secret %C, computed secret %C\n" secret secret'

let secret = '\x74'
let shares = [| 1, 0xB9; 2, 0xF5 |]
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
|]
let secret' = Share.unshare shares
let () = Printf.printf "Secret %S, computed secret %S\n" secret secret'
