let pi = 3.1415926535

let rec reduction_2pi a =
  let rec f a p =
    if a < p then p else f a (2. *. p) in
  let rec g a p =
    if a < 2. *. pi then a else
      let a_new = if a < p then a else a -. p in
      g a_new (p *. 0.5) in
  let p = f a (2. *. pi) in
  g a p


let rec sin_taylor a =
  let a2 = a *. a in
  (1. -. (0.16666668 -. (0.008332824 -. 0.00019587841 *. a2) *. a2) *. a2) *. a

let rec cos_taylor a =
  let a2 = a *. a in
  1. -. (0.5 -. (0.04166368 -. 0.0013695068 *. a2) *. a2) *. a2

let rec min_caml_sin a =
  let sign = a >= 0. in
  let a = if sign then a else -.a in
  let a = reduction_2pi a in
  let (sign, a) = if a < pi then (sign, a) else (not sign, a -. pi) in
  let a = if a < pi/.2. then a else pi -. a in
  if a <= pi/.4. then if sign then sin_taylor a else -.sin_taylor a
  else
    let a = pi/.2. -. a in
    if sign then cos_taylor a else -.cos_taylor a

let rec min_caml_cos a =
  let a = if a < 0. then -.a else a in
  let a = reduction_2pi a in
  let (sign, a) = if a < pi then (true, a) else (false, a -. pi) in
  let (sign, a) = if a < pi/.2. then (sign, a) else (not sign, pi -. a) in
  if a <= pi/.4. then if sign then cos_taylor a else -.cos_taylor a
  else
    let a = pi/.2. -. a in
    if sign then sin_taylor a else -.sin_taylor a

let rec atan_taylor a =
  let a2 = a *. a in
  (1. -. (0.3333333 -. (0.2 -. (0.142857142 -. (0.111111104 -. (0.08976446 -. 0.060035485 *. a2) *. a2) *. a2) *. a2) *. a2) *. a2) *. a

let rec min_caml_atan a =
  let sign = a >= 0. in
  let a = if sign then a else -.a in
  if a < 0.4735 then if sign then atan_taylor a else -.atan_taylor a
  else if a < 2.4735 then
    let b = atan_taylor ((a -. 1.)/.(a +. 1.)) in
    if sign then pi/.4. +. b else -.pi/.4. -. b
  else if sign then pi/.2. -. atan_taylor (1./.a) else atan_taylor (1./.a) -. pi/.2.
(* ==========================================
   2. 分布集計ロジック
   ========================================== *)

(* 集計結果を保持する型 *)
type stats = {
  hist : (int32, int) Hashtbl.t; (* 誤差の大きさ -> 出現回数 *)
  mutable sign_mismatch : int;   (* 符号が違う回数 *)
  mutable nan_count : int;       (* NaNが含まれていた回数(除外用) *)
}

let create_stats () = {
  hist = Hashtbl.create 1024;
  sign_mismatch = 0;
  nan_count = 0;
}

let check_range start_idx end_idx target_func std_func =
  let local_stats = create_stats () in
  
  for i = start_idx to end_idx do
    let i32 = Int32.of_int i in
    let x = Int32.float_of_bits i32 in
    
    let y_target = target_func x in
    let y_std = std_func x in
    
    if Float.is_nan y_target || Float.is_nan y_std then
      local_stats.nan_count <- local_stats.nan_count + 1
    else
      let bits_target = Int32.bits_of_float y_target in
      let bits_std = Int32.bits_of_float y_std in
      
      (* 符号ビットチェック (最上位ビット) *)
      (* XORをとって負(最上位が1)なら符号が違う *)
      if (Int32.logxor bits_target bits_std) < 0l then
        (* ただし +0.0 と -0.0 の違いは数値的には等価なので、
           実装によっては許容したいかもしれません。今回は厳密に分けます。 *)
        local_stats.sign_mismatch <- local_stats.sign_mismatch + 1
      else
        (* 符号が同じなら、整数としての差分を取る *)
        (* 差が0なら完全一致 *)
        let diff = Int32.abs (Int32.sub bits_target bits_std) in
        
        (* カウントアップ *)
        match Hashtbl.find_opt local_stats.hist diff with
        | Some count -> Hashtbl.replace local_stats.hist diff (count + 1)
        | None -> Hashtbl.add local_stats.hist diff 1
  done;
  local_stats

let run_census name target_func std_func =
  Printf.printf "Starting Distribution Analysis for: %s\n%!" name;
  let t_start = Unix.gettimeofday () in
  let num_domains = Domain.recommended_domain_count () in
  let total_range = 0x100000000L in
  let chunk_size = Int64.div total_range (Int64.of_int num_domains) in

  let tasks = Array.init num_domains (fun d ->
    let start_n = Int64.mul (Int64.of_int d) chunk_size in
    let end_n = 
      if d = num_domains - 1 then Int64.sub total_range 1L 
      else Int64.sub (Int64.add start_n chunk_size) 1L 
    in
    Domain.spawn (fun () -> 
      check_range (Int64.to_int start_n) (Int64.to_int end_n) target_func std_func
    )
  ) in

  (* 結果のマージ *)
  let total_stats = create_stats () in
  
  Array.iter (fun domain ->
    let res = Domain.join domain in
    total_stats.sign_mismatch <- total_stats.sign_mismatch + res.sign_mismatch;
    total_stats.nan_count <- total_stats.nan_count + res.nan_count;
    
    (* ハッシュテーブルのマージ *)
    Hashtbl.iter (fun diff count ->
      match Hashtbl.find_opt total_stats.hist diff with
      | Some c -> Hashtbl.replace total_stats.hist diff (c + count)
      | None -> Hashtbl.add total_stats.hist diff count
    ) res.hist
  ) tasks;

  let t_end = Unix.gettimeofday () in
  
  (* CSVへの書き出し *)
  let filename = Printf.sprintf "dist_%s.csv" name in
  let oc = open_out filename in
  Printf.fprintf oc "diff_size_int32,count\n";
  
  (* 誤差の小さい順にソートして出力したいのでListへ変換 *)
  let sorted_hist = 
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) total_stats.hist []
    |> List.sort (fun (k1, _) (k2, _) -> Int32.unsigned_compare k1 k2)
  in
  
  List.iter (fun (diff, count) ->
    Printf.fprintf oc "%ld,%d\n" diff count
  ) sorted_hist;
  
  close_out oc;

  Printf.printf "Completed %s in %.2f seconds.\n" name (t_end -. t_start);
  Printf.printf "  Sign Mismatches: %d\n" total_stats.sign_mismatch;
  Printf.printf "  NaN Skips:       %d\n" total_stats.nan_count;
  Printf.printf "  Unique Error Sizes: %d\n" (List.length sorted_hist);
  Printf.printf "  Results saved to %s\n" filename;
  Printf.printf "--------------------------------------------\n%!"

let () =
  (* コマンド: ocamlopt -I +unix unix.cmxa -O3 -o test_trig_hist test_trig_hist.ml *)
  (*run_census "sin" min_caml_sin sin;
  run_census "cos" min_caml_cos cos;*)
  run_census "atan" min_caml_atan atan;
