let error fmt =
  Printf.ksprintf (fun s -> Printf.printf "[EE] %s\n%!" s; exit 1) fmt

let warning fmt =
  Printf.ksprintf (fun s -> Printf.printf "[WW] %s\n%!" s) fmt

let info fmt =
  Printf.ksprintf (fun s -> Printf.printf "[II] %s\n%!" s) fmt

module Yaml = struct
  include Yaml
  include Yaml.Util

  let to_list_exn = function
    | `A l -> l
    | _ -> assert false

  let to_obj_exn = function
    | `O l -> l
    | _ -> assert false

  let to_int_exn v = to_float_exn v |> int_of_float
end
