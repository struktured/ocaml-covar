(* Generic Gaussian-process fit demo.

   Reads a CSV with columns (x, y[, z]), fits an exact GP with a
   squared-exponential kernel to (x, y), and prints the posterior mean and
   standard deviation at each input plus on a refined grid. Observations lying
   more than 2 standard deviations from the posterior mean are flagged.

   If a third column z is present, it is treated as a separate series to
   *score* against the fit: each z is compared to the posterior at x and the
   deviation reported in standard deviations. (E.g. fit fair value on bid/ask
   mids, then score last-trade prints to flag stale / off-curve quotes.)

   usage: gp_fit <csv> [-amplitude a] [-bandwidth b] [-noise n] *)

module SE = Covar_kernels.Squared_exponential
module GP = Covar_base.Covar_gp.Make (SE)

let read_rows path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line ->
      let acc =
        match String.split_on_char ',' line with
        | x :: y :: rest -> (
          match
            ( float_of_string_opt (String.trim x)
            , float_of_string_opt (String.trim y) )
          with
          | Some x, Some y ->
            let z =
              match rest with
              | z :: _ -> float_of_string_opt (String.trim z)
              | [] -> None
            in
            (x, y, z) :: acc
          | _ -> acc (* skip header / non-numeric rows *))
        | _ -> acc
      in
      loop acc
    | exception End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

let arg name default =
  let rec find = function
    | a :: b :: _ when String.equal a name -> (
      try float_of_string b with _ -> default)
    | _ :: rest -> find rest
    | [] -> default
  in
  find (Array.to_list Sys.argv)

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline
      "usage: gp_fit <csv> [-amplitude a] [-bandwidth b] [-noise n]";
    exit 1);
  let data = read_rows Sys.argv.(1) in
  let inputs = Array.of_list (List.map (fun (x, _, _) -> x) data) in
  let targets = Array.of_list (List.map (fun (_, y, _) -> y) data) in
  let scores = Array.of_list (List.map (fun (_, _, z) -> z) data) in
  let n = Array.length inputs in
  if n = 0 then (
    prerr_endline "no numeric (x,y) rows found";
    exit 1);
  let amplitude = arg "-amplitude" 1.0 in
  let bandwidth = arg "-bandwidth" 1.0 in
  let noise = arg "-noise" 1e-4 in
  let opt = SE.Optional_args.make ~amplitude ~bandwidth () in
  let kernel = SE.create ~opt () in
  (* Constant mean function: fit the GP to de-meaned targets and add the mean
     back on prediction. Without this a zero-mean GP reverts toward 0 in gaps
     and extrapolation, which is wrong for data not centred on zero. *)
  let ybar = Array.fold_left ( +. ) 0.0 targets /. float_of_int n in
  let centered = Array.map (fun y -> y -. ybar) targets in
  let gp = GP.create ~noise ~kernel ~inputs ~targets:centered () in
  let predict x =
    let m, v = GP.predict gp x in
    m +. ybar, v
  in
  Printf.printf
    "GP fit: n=%d  amplitude=%g  bandwidth=%g  noise=%g  mean=%.4f  \
     log-marginal-lik=%.4f\n"
    n amplitude bandwidth noise ybar (GP.log_marginal_likelihood gp);
  Printf.printf "\n  x        y(obs)   mean     std      resid    flag\n";
  Array.iteri
    (fun i x ->
      let m, v = predict x in
      let s = sqrt v in
      let y = targets.(i) in
      let resid = y -. m in
      let flag =
        if abs_float resid > 2.0 *. s && s > 0.0 then "  <-- off-curve (>2s)"
        else ""
      in
      Printf.printf "  %-7.3f  %-7.3f  %-7.3f  %-7.3f  %+7.3f%s\n" x y m s resid
        flag)
    inputs;
  let xmin = Array.fold_left min inputs.(0) inputs in
  let xmax = Array.fold_left max inputs.(0) inputs in
  Printf.printf "\n  smooth posterior on a grid (mean +/- std):\n";
  let steps = 20 in
  for k = 0 to steps do
    let x =
      xmin +. ((xmax -. xmin) *. float_of_int k /. float_of_int steps)
    in
    let m, v = predict x in
    Printf.printf "  %-7.3f  %.3f  +/- %.3f\n" x m (sqrt v)
  done;
  if Array.exists (fun z -> z <> None) scores then (
    Printf.printf
      "\n  scoring 3rd column against the fit:\n\
      \  x        z        mean     std      dev(sigma)  flag\n";
    Array.iteri
      (fun i x ->
        match scores.(i) with
        | None -> ()
        | Some z ->
          let m, v = predict x in
          let s = sqrt v in
          let dev = if s > 0.0 then (z -. m) /. s else 0.0 in
          let flag =
            if abs_float dev > 2.0 then "  <-- off-curve (>2s)" else ""
          in
          Printf.printf "  %-7.3f  %-7.3f  %-7.3f  %-7.3f  %+8.2f%s\n" x z m s
            dev flag)
      inputs)
