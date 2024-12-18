(** An homework. *)

open Common

module Question = struct
  type t =
    {
      name : string;
      regexp_string : string;
      regexp : Str.regexp;
      points : float;
    }

  let name q = q.name

  let regexp_string q = q.regexp_string
  
  let points q = q.points
end

module Q = Question

type t =
  {
    name : string;
    maximum : float;
    coefficient : float;
    questions : Q.t list;
  }

let of_csv ?(name="") rows =
  let name = ref name in
  let maximum = ref 20. in
  let coefficient = ref None in
  let question = ref [] in
  let regexp = ref [] in
  let points = ref [] in
  List.iter
    (function
      | [] -> ()
      | k::l ->
        match k with
        | "name" -> name := List.hd l
        | "maximum" -> maximum := float_of_string (List.hd l)
        | "coefficient" -> coefficient := Some (float_of_string (List.hd l))
        | "question" -> question := l
        | "regexp" -> regexp := l
        | "points" -> points := List.map (fun x -> if x = "" then 1. else float_of_string x) l
        | k -> warning "Unhandled row: %s" k
    ) rows;
  let name = !name in
  let n = List.length !question in
  let points = List.append !points (List.init (n - List.length !points) (fun _ -> 1.)) in
  let questions =
    let rec aux = function
      | q::q', g::g', p::p' -> { Q.name = q; regexp_string = g; regexp = Str.regexp g; points = p } :: aux (q', g', p')
      | [], [], [] -> []
      | _ -> assert false
    in
    aux (!question, !regexp, points)
  in
  let maximum = !maximum in
  let coefficient =
    match !coefficient with
    | Some c -> c
    | None ->
      let n = List.map Q.points questions |> List.fold_left (+.) 0. in
      maximum /. n
  in
  { name; maximum; coefficient; questions; }

let name a = a.name

let maximum a = a.maximum

let coefficient a = a.coefficient

let questions q = q.questions
