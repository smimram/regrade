open Common

module A = Assignment

let () =
  Printexc.record_backtrace true;
  let csv = ref "" in
  let files = ref [] in
  let extension = ref None in
  let formulas = ref false in
  let outfile = ref "grades.csv" in
  Arg.parse
    [
      "--extension", Arg.String (fun s -> extension := Some s), "Extension of files to consider.";
      "--formulas", Arg.Set formulas, "Create a csv with formulas.";
      "-o", Arg.Set_string outfile, "Output file.";
    ]
    (fun s ->
       if !csv = "" then csv := s
       else files := s :: !files
    )
    "grader configuration files";
  let csv = !csv in
  let extension = !extension in
  let files = List.rev !files in
  let files =
    if files = [] then
      let l = Sys.readdir "." |> Array.to_list |> List.sort compare in
      let l = match extension with Some ext -> List.filter (fun f -> try Sys.is_directory f || String.ends_with ~suffix:ext f with Sys_error _ -> false) l | None -> l in
      l
    else files
  in
  if csv = "" then error "Please provide a configuration file.";
  info "Reading %s" csv;
  let name = Filename.basename csv |> Filename.remove_extension in
  let csv = CSV.of_file csv in
  let a = A.of_csv ~name csv in
  info "Loaded %s (%.02f points, %.02f coef)" (A.name a) (A.maximum a) (A.coefficient a);
  let rows =
    List.map
      (fun fname ->
         info "Grading %s" fname;
         let files =
           if Sys.is_directory fname then File.find ?extension fname
           else [fname]
         in
         let f = List.map File.contents files |> String.concat "\n" in
         let q =
           List.map
             (fun q ->
                let regexp = q.A.Q.regexp in
                if Str.string_match_forward regexp f 0 then q.A.Q.points else 0.
             ) a.A.questions
         in
         let grade = A.coefficient a *. List.fold_left (+.) 0. q in
         let grade = string_of_float grade in
         fname::grade::(List.map string_of_float q)
      ) files
  in
  let header = "File"::"Grade"::(List.map A.Q.name (A.questions a)) in
  let out = CSV.to_string (header::rows) in
  print_newline ();
  print_string out;
  let out =
    if not !formulas then out
    else
      (* Grades with formulas. *)
      let coefficients = ""::(A.coefficient a |> string_of_float)::(List.map A.Q.points (A.questions a) |> List.map string_of_float) in
      let rows =
        List.mapi
          (fun i row ->
             match row with
             | name::_grade::grades ->
               let n = List.length grades in
               let grade = Printf.sprintf "=SUMPRODUCT($C$2:$%s$2;C%d:%s%d)*$B$2" (CSV.column (n+2)) (i+3) (CSV.column (n+2)) (i+3) in
               let grades = List.map (fun x -> if x = "0." then "0." else "1.") grades in
               name::grade::grades
             | _ -> assert false
          ) rows
      in
      CSV.to_string (header::coefficients::rows)
  in
  File.write !outfile out
