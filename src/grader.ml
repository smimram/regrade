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
  info "Loaded %s (%.02f points, %.02f coef)" (A.name a) (A.maximum a) (A.coefficient a)
