open Common

module Yaml = struct
  include Yaml
  include Yaml.Util

  let to_obj_exn = function
    | `O l -> l
    | _ -> assert false
end

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
  let assignment =
    let a = Assignment.create () in
    let conf = Yaml.to_obj_exn conf in
    List.iter (fun (k,v) ->
        match k with
        | "name" -> a.name <- Yaml.to_string_exn v
        | k -> warning "Unhandled key: %s" k
      ) conf
  in
  ignore assignment
