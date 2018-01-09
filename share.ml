let sum a =
  Array.fold_left
    GF256.add
    GF256.zero
    a

let product a =
  Array.fold_left
    GF256.mul
    GF256.one
    a

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
  |> Array.map (fun x -> x, f a x)

let l u i =
  Array.mapi 
    (fun j u_j ->
       let open GF256 in
       if i = j
       then one
       else div u_j (add u_j u.(i)))
    u
  |> product

let i u v =
  Array.mapi
    (fun i v_i ->
       GF256.mul (l u i) v_i)
    v
  |> sum

let unshare_byte shares =
  let u = Array.map fst shares
  and v = Array.map snd shares in
  i u v |> GF256.to_char
