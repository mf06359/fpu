open Unix
open Atomic
open Int64

(* ================= 設定 ================= *)

let chunk_size = 1_000_000
let max_ulp_bucket = 4 (* 0,1,2,>=3 *)
let total_floats = shift_left 1L 32

(* ================= 対象関数 ================= *)

type target = Sin | Cos | Atan

let target_of_string = function
  | "sin" -> Sin
  | "cos" -> Cos
  | "atan" -> Atan
  | _ ->
      prerr_endline "Usage: ./check [sin|cos|atan]";
      exit 1

(* ===== ユーザー実装 ===== *)
let pi = 3.1415926535

let rec reduction_2pi a =
  let rec f a p = if a < p then p else f a (2. *. p) in
  let rec g a p =
    if a < 2. *. pi then a
    else
      let a_new = if a < p then a else a -. p in
      g a_new (p *. 0.5)
  in
  let p = f a (2. *. pi) in
  g a p

let rec sin_taylor a =
  let a2 = a *. a in
  (1. -. ((0.16666668 -. ((0.008332824 -. (0.00019587841 *. a2)) *. a2)) *. a2))
  *. a

let rec cos_taylor a =
  let a2 = a *. a in
  1. -. ((0.5 -. ((0.04166368 -. (0.0013695068 *. a2)) *. a2)) *. a2)

let rec my_sin a =
  let sign = a >= 0. in
  let a = if sign then a else -.a in
  let a = reduction_2pi a in
  let sign, a = if a < pi then (sign, a) else (not sign, a -. pi) in
  let a = if a < pi /. 2. then a else pi -. a in
  if a <= pi /. 4. then if sign then sin_taylor a else -.sin_taylor a
  else
    let a = (pi /. 2.) -. a in
    if sign then cos_taylor a else -.cos_taylor a

let rec my_cos a =
  let a = if a < 0. then -.a else a in
  let a = reduction_2pi a in
  let sign, a = if a < pi then (true, a) else (false, a -. pi) in
  let sign, a = if a < pi /. 2. then (sign, a) else (not sign, pi -. a) in
  if a <= pi /. 4. then if sign then cos_taylor a else -.cos_taylor a
  else
    let a = (pi /. 2.) -. a in
    if sign then sin_taylor a else -.sin_taylor a

let rec atan_taylor a =
  let a2 = a *. a in
  (1.
  -. (0.3333333
     -. (0.2
        -. (0.142857142
           -. (0.111111104 -. ((0.08976446 -. (0.060035485 *. a2)) *. a2))
              *. a2)
           *. a2)
        *. a2)
     *. a2)
  *. a

let rec my_atan a =
  let sign = a >= 0. in
  let a = if sign then a else -.a in
  if a < 0.4735 then if sign then atan_taylor a else -.atan_taylor a
  else if a < 2.4735 then
    let b = atan_taylor ((a -. 1.) /. (a +. 1.)) in
    if sign then (pi /. 4.) +. b else (-.pi /. 4.) -. b
  else if sign then (pi /. 2.) -. atan_taylor (1. /. a)
  else atan_taylor (1. /. a) -. (pi /. 2.)

(* ================= IEEE utilities ================= *)

let f32 x = Int32.float_of_bits (Int32.bits_of_float x)

let ordered_bits (x : float) : int32 =
  let b = Int32.bits_of_float x in
  if Int32.logand b 0x8000_0000l <> 0l then Int32.lognot b
  else Int32.logxor b 0x8000_0000l

let ulp_diff a b =
  let oa = ordered_bits a in
  let ob = ordered_bits b in
  Int32.to_int (Int32.abs (Int32.sub oa ob))

(* ================= 統計 ================= *)

let ulp_hist = Array.init max_ulp_bucket (fun _ -> Atomic.make 0)
let ulp_over = Atomic.make 0
let nan_mismatch = Atomic.make 0
let error_fd = Unix.openfile "error.txt" [ O_CREAT; O_WRONLY; O_APPEND ] 0o644

(*
let log_error bits x ref_v my_v ulp =
  let s =
    Printf.sprintf "input=0x%08lx x=%.9g ref=0x%08lx my=0x%08lx ulp=%d\n" bits x
      (Int32.bits_of_float ref_v)
      (Int32.bits_of_float my_v) ulp
  in
  ()  ignore (Unix.write_substring error_fd s 0 (String.length s)) *)

(* ================= Progress ================= *)

let done_chunks = Atomic.make 0
let start_time = Unix.gettimeofday ()

let report_progress total_chunks =
  let d = Atomic.get done_chunks in
  let elapsed = Unix.gettimeofday () -. start_time in
  let percent = float d /. float total_chunks *. 100.0 in
  let speed = float d *. float chunk_size /. elapsed /. 1e6 in
  Printf.eprintf "\rProgress: %.3f%% | %.2f M/s" percent speed;
  flush Stdlib.stderr

(* ================= Worker ================= *)
  let is_normal_float x =
  let b = Int32.bits_of_float x in
  let exp = Int32.logand b 0x7F80_0000l in
  exp <> 0l && exp <> 0x7F80_0000l

let check_chunk target chunk_id =
  let base = mul (of_int chunk_id) (of_int chunk_size) in
  for i = 0 to chunk_size - 1 do
    let idx = add base (of_int i) in
    if idx < total_floats then begin
      let bits =
        Int32.of_int (to_int (logand idx 0xFFFF_FFFFL))
      in
      let x = Int32.float_of_bits bits in

      (* ★ 正規数のみを調査対象にする ★ *)
      if is_normal_float x then begin
        let ref_v, my_v =
          match target with
          | Sin  -> (f32 (sin x),  f32 (my_sin x))
          | Cos  -> (f32 (cos x),  f32 (my_cos x))
          | Atan -> (f32 (atan x), f32 (my_atan x))
        in

        let ulp = ulp_diff ref_v my_v in
        if ulp < max_ulp_bucket then
          ignore (Atomic.fetch_and_add ulp_hist.(ulp) 1)
        else
          ignore (Atomic.fetch_and_add ulp_over 1)
      end
    end
  done;
  ignore (Atomic.fetch_and_add done_chunks 1)

(* ================= Main ================= *)

let () =
  let target_arg =
    Array.to_list Sys.argv |> List.tl
    |> List.find_opt (fun s -> s = "sin" || s = "cos" || s = "atan")
  in
  match target_arg with
  | None ->
      prerr_endline "Usage: ./check [sin|cos|atan] [-domain-count N]";
      exit 1
  | Some arg ->
      let target = target_of_string arg in
      let domains = Domain.recommended_domain_count () in
      let total_chunks = to_int (div total_floats (of_int chunk_size)) + 1 in

      Printf.eprintf "Target=%s Domains=%d Total=2^32\n%!" Sys.argv.(1) domains;

      let next_chunk = Atomic.make 0 in

      let worker () =
        let rec loop () =
          let c = Atomic.fetch_and_add next_chunk 1 in
          if c < total_chunks then begin
            check_chunk target c;
            if c mod 10 = 0 then report_progress total_chunks;
            loop ()
          end
        in
        loop ()
      in

      let ds = Array.init (domains - 1) (fun _ -> Domain.spawn worker) in
      worker ();
      Array.iter Domain.join ds;

      Printf.eprintf "\n\n=== RESULT ===\n";
      for i = 0 to max_ulp_bucket - 1 do
        Printf.eprintf "ULP=%d : %d\n" i (Atomic.get ulp_hist.(i))
      done;
      Printf.eprintf "ULP>=%d : %d\n" max_ulp_bucket (Atomic.get ulp_over);
      Printf.eprintf "NaN mismatch : %d\n" (Atomic.get nan_mismatch);

      Unix.close error_fd
