open Common

module A = Assignment

let () =
  Printexc.record_backtrace true;
  let conf = ref "" in
  let files = ref [] in
  Arg.parse
    []
    (fun s ->
       if !conf = "" then conf := s
       else files := s :: !files
    )
    "grader configuration files";
  let conf = !conf in
  (* let files = List.rev !files in *)
  if conf = "" then error "Please provide a configuration file.";
  let conf =
    let f = open_in conf in
    let s = really_input_string f (in_channel_length f) in
    close_in f;
    info "Parsed %s" conf;
    s
  in
  let conf = Yaml.of_string_exn conf in
  let a = A.of_yaml conf in
  info "Loaded %s (%.02f points, %.02f coef)" (A.name a) (A.points a) (A.coefficient a)
