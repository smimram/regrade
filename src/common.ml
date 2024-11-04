let error fmt =
  Printf.ksprintf (fun s -> Printf.printf "[EE] %s\n%!" s; exit 1) fmt

let warning fmt =
  Printf.ksprintf (fun s -> Printf.printf "[WW] %s\n%!" s) fmt

let info fmt =
  Printf.ksprintf (fun s -> Printf.printf "[II] %s\n%!" s) fmt

module CSV = struct
  include Csv

  let row i = i+1

  let column i = (int_of_char 'A' + i) |> char_of_int |> String.make 1

  let of_file f =
    let f = open_in f in
    let c = of_channel f in
    let c = Rows.input_all c in
    Stdlib.close_in f;
    List.map Row.to_list c

  let to_string rows =
    let buf = Buffer.create 0 in
    let oc = to_buffer buf in
    output_all oc rows;
    Buffer.contents buf
end

module File = struct
  let contents fname =
    let f = open_in fname in
    let s = really_input_string f (in_channel_length f) in
    close_in f;
    s

  let rec find ?(recursive=true) ?extension dir =
    let l = Sys.readdir dir |> Array.to_list |> List.map (fun f -> if dir = "." then f else Filename.concat dir f) in
    let l =
      if recursive then
        List.map
          (fun f ->
             if Sys.is_directory f then
               find ~recursive f
             else [f]
          ) l |> List.flatten
      else
        List.filter (fun f -> not (Sys.is_directory f)) l
    in
    let l =
      match extension with
      | Some ext -> List.filter (String.ends_with ~suffix:ext ) l
      | None -> l
    in
    let l = List.sort compare l in
    l

  let write fname s =
    let oc = open_out fname in
    output_string oc s;
    close_out oc
end

module Str = struct
  include Str

  let string_match_forward regexp s off =
    try ignore (search_forward regexp s off); true
    with Not_found -> false
end
