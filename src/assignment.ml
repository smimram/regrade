(** An homework. *)

open Common

module Question = struct
  type t =
    {
      name : string;
      grep : string;
      points : float;
    }

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

let of_csv rows =
  let name = ref "" in
  let maximum = ref 20. in
  let coefficient = ref None in
  let question = ref [] in
  let grep = ref [] in
  let points = ref [] in
  List.iter (function
      | ["name"; v] -> name := v
      | ["maximum"; v] -> maximum := float_of_string v
      | ["coefficient"; v] -> coefficient := Some (float_of_string v)
      | "question"::l -> question := l
      | "grep"::l -> grep := l
      | "points"::l -> points := List.map float_of_string l
      | k::_ -> warning "Unhandled row: %s" k
      | [] -> ()
    ) rows;
  let name = !name in
  let n = List.length !question in
  let points = List.append !points (List.init (n - List.length !points) (fun _ -> 1.)) in
  let questions =
    let rec aux = function
      | q::q', g::g', p::p' -> { Q.name = q; grep = g; points = p } :: aux (q', g', p')
      | [], [], [] -> []
      | _ -> assert false
    in
    aux (!question, !grep, points)
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
