let error fmt =
  Printf.ksprintf (fun s -> Printf.printf "[EE] %s\n%!" s; exit 1) fmt

let warning fmt =
  Printf.ksprintf (fun s -> Printf.printf "[WW] %s\n%!" s) fmt

let info fmt =
  Printf.ksprintf (fun s -> Printf.printf "[II] %s\n%!" s) fmt

module CSV = struct
  include Csv

  let of_file f =
    let f = open_in f in
    let c = of_channel f in
    let c = Rows.input_all c in
    Stdlib.close_in f;
    List.map Row.to_list c
end
