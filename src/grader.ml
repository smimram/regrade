open Common

module A = Assignment

let () =
  Printexc.record_backtrace true;
  let csv = ref "" in
  let files = ref [] in
  Arg.parse
    []
    (fun s ->
       if !csv = "" then csv := s
       else files := s :: !files
    )
    "grader configuration files";
  let csv = !csv in
  (* let files = List.rev !files in *)
  if csv = "" then error "Please provide a configuration file.";
  info "Reading %s" csv;
  let csv = CSV.of_file csv in
  let a = A.of_csv csv in
  info "Loaded %s (%.02f points, %.02f coef)" (A.name a) (A.maximum a) (A.coefficient a);
  let rows =
    List.map
      (fun fname ->
         let f = File.contents fname in
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
      ) !files
  in
  let rows = ("File"::"Grade"::(List.map A.Q.name (A.questions a)))::rows in
  let out =
    let buf = Buffer.create 0 in 
    let oc = CSV.to_buffer buf in
    CSV.output_all oc rows;
    Buffer.contents buf
  in
  print_newline ();
  print_string out;
  let oc = open_out "grades.csv" in
  output_string oc out;
  close_out oc
