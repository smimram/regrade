open Common

module A = Assignment

let () =
  Printexc.record_backtrace true;
  let csv = ref "" in
  let files = ref [] in
  let extensions = ref [] in
  let formulas = ref false in
  let show_regexp = ref false in
  let round = ref 0.5 in
  let outfile = ref "grades.csv" in
  let exclude = ref [] in
  let exclude_zero = ref true in
  let verbose = ref false in
  Arg.parse
    (Arg.align
       [
         "--extension", Arg.String (fun s -> extensions := s :: !extensions), " Extension of files to consider.";
         "--exclude", Arg.String (fun s -> exclude := Re.Posix.compile_pat ("^"^s^"$") :: !exclude), " Exclude files whose name match the given regexp.";
         "--formulas", Arg.Set formulas, " Create a csv with formulas.";
         "--show-regexp", Arg.Set show_regexp, " Show regular expressions.";
         "--round", Arg.Set_float round, Printf.sprintf " Round notes (default: %s)." (string_of_float !round);
         "--include-zero", Arg.Unit (fun () -> exclude_zero := false), " Consider files graded zero as valid (they are excluded by default).";
         "--verbose", Arg.Set verbose, " Be more verbose.";
         "-o", Arg.Set_string outfile, Printf.sprintf " Output file (default: %s)." !outfile;
       ]
    )
    (fun s ->
       if !csv = "" then csv := s
       else files := s :: !files
    )
    "regrade configuration [files]";
  let csv = !csv in
  let extensions = !extensions in
  let filter_extension fnames =
    if extensions = [] then fnames else
      List.filter (fun f -> try Sys.is_directory f || List.exists (fun ext -> String.ends_with ~suffix:ext f) extensions with Sys_error _ -> false) fnames
  in
  let round = !round in
  let files = List.rev !files in
  let files =
    if files = [] then
      let l = Sys.readdir "." |> Array.to_list |> List.sort compare in
      let l = filter_extension l in
      let l = List.filter (fun f -> not (List.exists (fun re -> Re.execp re (Filename.basename f)) !exclude)) l in
      l
    else files
  in
  if csv = "" then error "Please provide a configuration file.";
  info "Reading %s" csv;
  let name = Filename.basename csv |> Filename.remove_extension in
  let csv = CSV.of_file csv in
  let a = A.of_csv ~name csv in
  info "Loaded %s (%.02f points, %.02f coef)" (A.name a) (A.maximum a) (A.coefficient a);
  let final_grades = ref [] in
  let rows =
    List.filter_map
      (fun fname ->
         info "Grading %s" fname;
         let files =
           if Sys.is_directory fname then File.find fname
           else [fname]
         in
         let files = filter_extension files in
         if !verbose then List.iter (debug "Considering %s") files;
         let f = List.map File.contents files |> String.concat "\n" in
         let q =
           List.map
             (fun q ->
                let regexp = q.A.Q.regexp in
                let has_file = List.exists (fun fname -> Re.execp q.A.Q.file (Filename.basename fname)) files in
                if has_file || Re.execp regexp f then q.A.Q.points else 0.
             ) a.A.questions
         in
         let grade = A.coefficient a *. List.fold_left (+.) 0. q in
         let grade = min (A.maximum a) grade in
         let grade = Float.round (grade /. round) *. round in
         final_grades := grade :: !final_grades;
         let grade = string_of_float grade in
         if List.for_all (fun x -> x = 0.) q then None
         else Some (fname::grade::(List.map string_of_float q))
      ) files
  in
  let header = "File"::"Grade"::(List.map A.Q.name (A.questions a)) in
  let rows =
    if not !show_regexp then rows else
      let re = ""::""::(List.map A.Q.regexp_string (A.questions a)) in
      re::rows
  in
  let out = CSV.to_string (header::rows) in
  print_newline ();
  print_string out;
  print_newline ();
  Printf.printf "Average is %.02f\n%!" (List.average !final_grades);
  let out =
    if not !formulas then out
    else
      (* Grades with formulas. *)
      let coef = Printf.sprintf "=%s/SUM(C2:%s2)" (A.maximum a |> string_of_float) (CSV.column (A.questions a |> List.length |> Int.succ)) in
      let coefficients = ""::coef::(List.map A.Q.points (A.questions a) |> List.map string_of_float) in
      let rows =
        List.mapi
          (fun i row ->
             match row with
             | name::_grade::grades ->
               let n = List.length grades in
               let grade = Printf.sprintf "=MROUND(MIN(%s,SUMPRODUCT($C$2:$%s$2;C%d:%s%d)*$B$2);0.5)" (A.maximum a |> string_of_float) (CSV.column (n+1)) (i+3) (CSV.column (n+1)) (i+3) in
               let grades = List.map (fun x -> if x = "0." then "0." else "1.") grades in
               name::grade::grades
             | _ -> assert false
          ) rows
      in
      CSV.to_string (header::coefficients::rows)
  in
  File.write !outfile out
