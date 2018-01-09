val share_byte : shares:int -> threshold:int -> char -> (GF256.t * GF256.t) array
val share : shares:int -> threshold:int -> string -> (GF256.t * string) array

val unshare_byte : (GF256.t * GF256.t) array -> char
val unshare : (GF256.t * string) array -> string
