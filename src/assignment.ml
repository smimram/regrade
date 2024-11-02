(** An homework. *)

open Common

module Question = struct
  type t =
    {
      name : string;
      grep : string;
      points : float;
    }

  let fresh_name =
    let n = ref 0 in
    fun () ->
      incr n;
      "Q" ^ string_of_int !n

  let of_yaml yaml =
    let name = ref (fresh_name ()) in
    let grep = ref "" in
    let points = ref 1. in
    let yaml = Yaml.to_obj_exn yaml in
    List.iter (fun (k,v) ->
        match k with
        | "name" -> name := Yaml.to_string_exn v
        | "grep" -> grep := Yaml.to_string_exn v
        | "points" -> points := Yaml.to_float_exn v
        | k -> warning "Unhandled key in question: %s" k
      ) yaml;
    { name = !name; grep = !grep; points = !points }

  let points q = q.points
end

type t =
  {
    name : string;
    points : float;
    coefficient : float;
    questions : Question.t list;
  }

let of_yaml yaml =
  let name = ref "" in
  let points = ref 20. in
  let coefficient = ref None in
  let questions = ref [] in
  let yaml = Yaml.to_obj_exn yaml in
  List.iter (fun (k,v) ->
      match k with
      | "name" -> name := Yaml.to_string_exn v
      | "points" -> points := Yaml.to_float_exn v
      | "coefficient" -> coefficient := Some (Yaml.to_float_exn v)
      | "questions" -> questions := List.map Question.of_yaml (Yaml.to_list_exn v)
      | k -> warning "Unhandled key: %s" k
    ) yaml;
  let questions = !questions in
  let points = !points in
  let coefficient =
    match !coefficient with
    | Some c -> c
    | None ->
      let n = List.map Question.points questions |> List.fold_left (+.) 0. in
      points /. n
  in
  {
    name = !name;
    points;
    coefficient;
    questions;
  }
