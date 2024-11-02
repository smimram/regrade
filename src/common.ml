let error fmt =
  Printf.ksprintf (fun s -> Printf.printf "[EE] %s\n%!" s; exit 1) fmt

let warning fmt =
  Printf.ksprintf (fun s -> Printf.printf "[WW] %s\n%!" s) fmt

let info fmt =
  Printf.ksprintf (fun s -> Printf.printf "[II] %s\n%!" s) fmt
