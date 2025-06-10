open Base

module Array = struct
  include Array

  let for_ length ~f =
    for i = 0 to length - 1 do
      f i
    done
  ;;
end

module Iarray = struct
  include Iarray

  let for_ = Array.for_
end

module Int = Int
module Interface = Hardcaml.Interface
module List = List

let concat = String.concat

let option_map2_exn a b ~f =
  match a, b with
  | None, None -> None
  | Some a, Some b -> Some (f a b)
  | _, _ ->
    raise_s [%message "Option.map2 expects either both to be Some, or both to be None!"]
;;

let option_iter2_exn a b ~f =
  match a, b with
  | None, None -> ()
  | Some a, Some b -> f a b
  | _, _ ->
    raise_s [%message "Option.map2 expects either both to be Some, or both to be None!"]
;;

module Derive_interface_from_map2 (X : sig
    type 'a t

    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
    val port_names_and_widths : (string * int) t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  end) =
Interface.Make (struct
    type 'a t = 'a X.t

    let sexp_of_t = X.sexp_of_t
    let port_names_and_widths = X.port_names_and_widths
    let map2 s t ~f = X.map2 s t ~f
    let map t ~f = map2 t t ~f:(fun t _ -> f t)
    let iter2 s t ~f = ignore (map2 s t ~f:(fun s t -> f s t) : unit t)
    let iter t ~f = ignore (map2 t t ~f:(fun t _ -> f t) : unit t)

    let to_list t =
      let x = ref [] in
      iter t ~f:(fun t -> x := t :: !x);
      List.rev !x
    ;;
  end)
