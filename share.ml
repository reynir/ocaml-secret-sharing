
let f (a : GF256.t array) (x : GF256.t) =
  let rec loop i a x acc =
    let open GF256 in
    if i = 0
    then add a.(0) (mul x acc)
    else loop (i-1) a x (add a.(i) (mul x acc)) in
  loop (Array.length a - 1) a x GF256.zero


let share_byte secret threshold shares =
  assert (threshold <= shares);
  assert (threshold > 0);
  let secret = int_of_char secret in
  let a = Nocrypto.Rng.generate (threshold - 1) in
  let a = Array.init threshold
      (fun i ->
         if i = 0
         then secret
         else Cstruct.get_uint8 a (i-1)) in
  Array.init shares succ
  |> Array.map (f a)
