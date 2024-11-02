(** An homework. *)

module Question = struct
  type t
end

type t =
  {
    mutable name : string;
    mutable questions : Question.t list;
  }

let create () = { name = ""; questions = [] }
