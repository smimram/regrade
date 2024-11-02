let () =
  let conf = ref "" in
  let files = ref [] in
  Arg.parse
    []
    (fun s ->
       if !conf = "" then conf := s
       else files := s :: !files
    )
    "grader configuration files";
  let files = List.rev !files in
  ignore files
