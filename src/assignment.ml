(** An homework. *)

open Common

module Question = struct
  type t =
    {
      grep : string;
      points : float;
    }

  let of_yaml yaml =
    let grep = ref "" in
    let points = ref 1. in
    let yaml = Yaml.to_obj_exn yaml in
    List.iter (fun (k,v) ->
        match k with
        | "grep" -> grep := Yaml.to_string_exn v
        | "points" -> points := Yaml.to_float_exn v
        | k -> warning "Unhandled key in question: %s" k
      ) yaml;
    { grep = !grep; points = !points }
end

type t =
  {
    name : string;
    questions : Question.t list;
  }

let of_yaml yaml =
  let name = ref "" in
  let questions = ref [] in
  let yaml = Yaml.to_obj_exn yaml in
  List.iter (fun (k,v) ->
      match k with
      | "name" -> name := Yaml.to_string_exn v
      | "questions" -> questions := List.map Question.of_yaml (Yaml.to_list_exn v)
      | k -> warning "Unhandled key: %s" k
    ) yaml;
  { name = !name; questions = !questions }
