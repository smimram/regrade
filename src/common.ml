let error fmt =
  Printf.ksprintf (fun s -> Printf.printf "[EE] %s\n%!" s; exit 1) fmt

let warning fmt =
  Printf.ksprintf (fun s -> Printf.printf "[WW] %s\n%!" s) fmt

let info fmt =
  Printf.ksprintf (fun s -> Printf.printf "[II] %s\n%!" s) fmt

let debug fmt =
  Printf.ksprintf (fun s -> Printf.printf "[DD] %s\n%!" s) fmt

module CSV = struct
  include Csv

  let row i = i+1

  let column i =
    let char i = (int_of_char 'A' + i) |> char_of_int |> String.make 1 in
    if i < 26 then char i
    else (char (i / 26 - 1))^(char (i mod 26))

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

  let rec find ?(recursive=true) dir =
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
    List.sort compare l

  let write fname s =
    let oc = open_out fname in
    output_string oc s;
    close_out oc
end

module List = struct
  include List

  let average l =
    List.fold_left (+.) 0. l /. float (List.length l)

  let rec map4 f l1 l2 l3 l4 =
    match (l1, l2, l3, l4) with
    | x1::l1, x2::l2, x3::l3, x4::l4 -> (f x1 x2 x3 x4)::(map4 f l1 l2 l3 l4)
    | [], [], [], [] -> []
    | _ -> assert false

  let pad n d l =
    List.append l (List.init (n - List.length l) (fun _ -> d))
end

module String = struct
  include String

  (* TODO: more efficient *)
  let replace_all l s =
    let drop n s = String.sub s n (String.length s - n) in
    let rec aux s =
      if s = "" then "" else
        match
          List.find_map
            (fun (src,tgt) ->
               if String.starts_with ~prefix:src s
               then Some (tgt ^ aux @@ drop (String.length src) s)
               else None
            ) l
        with
        | Some s -> s
        | None -> String.sub s 0 1 ^ aux @@ drop 1 s
    in
    aux s
end
