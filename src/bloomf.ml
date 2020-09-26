type priv = { m : int; k : int; p_len : (int * int) list; b : Bitv.t }

type 'a t = priv

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let partition_lengths m k =
  let rec aux sum acc i =
    if List.length acc = k then (sum, acc)
    else
      let rec loop step =
        let k = i + step in
        let gcd_k = gcd k in
        if List.for_all (fun p -> gcd_k p = 1) acc then
          aux (sum + k) (k :: acc) (k + 1)
        else loop (step + 1)
      in
      loop 1
  in
  aux 0 [] (m / k)

let v m k =
  let m, lengths = partition_lengths m k in
  let p_len =
    let rec aux acc off = function
      | [] -> acc
      | h :: t -> aux ((off, h) :: acc) (off + h) t
    in
    aux [] 0 lengths
  in
  try
    let b = Bitv.create m false in
    { m; k; p_len; b }
  with Invalid_argument _ -> invalid_arg "Bloomf.create"

let estimate_parameters n p =
  let log2 = log 2. in
  let nf = float_of_int n in
  let m = ceil (-.nf *. log p /. log (2. ** log2)) in
  let k = ceil (log2 *. m /. nf) in
  (m, k)

let create ?(error_rate = 0.01) n_items =
  let m, k = estimate_parameters n_items error_rate in
  if error_rate <= 0. || error_rate >= 1. then invalid_arg "Bloomf.create";
  v (int_of_float m) (int_of_float k)

let add_priv t hashed_data =
  let rec loop = function
    | [] -> ()
    | (off, len) :: tl ->
        let loc = off + (hashed_data mod len) in
        let () = Bitv.unsafe_set t.b loc true in
        loop tl
  in
  loop t.p_len

let add bf data = add_priv bf (Hashtbl.hash data)

let mem_priv t hashed_data =
  let rec loop = function
    | [] -> true
    | (off, len) :: tl ->
        let loc = off + (hashed_data mod len) in
        let res = Bitv.unsafe_get t.b loc in
        if res then loop tl else false
  in
  loop t.p_len

let mem bf data = mem_priv bf (Hashtbl.hash data)

let clear t = Bitv.fill t.b 0 t.m false

(* Bitv.pop is really slow *)
let size_estimate t =
  let mf = float_of_int t.m in
  let kf = float_of_int t.k in
  let xf = float_of_int (Bitv.pop t.b) in
  int_of_float (-.mf /. kf *. log (1. -. (xf /. mf)))

let to_bytes t =
  let encoded_b = Bitv.to_bytes t.b in
  let encoded_b_len = Bytes.length encoded_b in
  let buf =
    8 (* m *) + 8 (* k *) + (t.k * 16) (* p_len *) + encoded_b_len
    |> Bytes.create
  in
  let blit_int i x =
    for j = 0 to 7 do
      Char.chr ((x lsr (8 * j)) land 0xFF) |> Bytes.set buf (i + j)
    done
  in
  blit_int 0 t.m;
  blit_int 8 t.k;
  List.iteri
    (fun i (off, len) ->
      blit_int (16 + (i * 16)) off;
      blit_int (16 + (i * 16) + 8) len)
    t.p_len;
  Bytes.blit encoded_b 0 buf (16 + (t.k * 16)) encoded_b_len;
  buf

let read_int b off =
  let rec build x i =
    if i < 0 then x
    else build ((x lsl 8) lor Char.code (Bytes.get b (i + off))) (pred i)
  in
  build 0 7

let of_bytes buf =
  let error () = invalid_arg "invalid bytes sequence" in
  if Bytes.length buf < 16 then error ();
  let m = read_int buf 0 in
  let k = read_int buf 8 in
  if Bytes.length buf < 16 + (k * 16) then error ();
  let p_len =
    List.init k (fun i ->
        let off = read_int buf (16 + (i * 16)) in
        let len = read_int buf (16 + (i * 16) + 8) in
        (off, len))
  in
  try
    let b =
      Bytes.sub buf (16 + (k * 16)) (Bytes.length buf - (16 + (k * 16)))
      |> Bitv.of_bytes
    in
    { m; k; p_len; b }
  with _ -> error ()

module type Hashable = sig
  type t

  val hash : t -> int
end

module Make (H : Hashable) = struct
  type t = priv

  let create = create

  let add bf data = add_priv bf (H.hash data)

  let mem bf data = mem_priv bf (H.hash data)

  let clear = clear

  let size_estimate = size_estimate
end
