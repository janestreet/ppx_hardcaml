open Base
include Ppx_hardcaml_runtime0
module Interface = Hardcaml.Interface
module Wave_format = Hardcaml.Wave_format

module Derive_interface_from_map2 (X : sig
    type 'a t [@@deriving equal ~localize, compare ~localize]

    val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
    val port_names_and_widths : (string * int) t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
    val wave_formats : Wave_format.t t
  end) =
Interface.Make_with_wave_formats (struct
    type 'a t = 'a X.t [@@deriving equal ~localize, compare ~localize]

    let sexp_of_t = X.sexp_of_t
    let port_names_and_widths = X.port_names_and_widths
    let map2 s t ~(f @ local) = X.map2 s t ~f [@nontail]
    let map t ~(f @ local) = map2 t t ~f:(fun t _ -> f t) [@nontail]
    let iter2 s t ~(f @ local) = ignore (map2 s t ~f:(fun s t -> f s t) : unit t)
    let iter t ~(f @ local) = ignore (map2 t t ~f:(fun t _ -> f t) : unit t)

    let to_list t =
      let x = ref [] in
      iter t ~f:(fun t -> x := t :: !x);
      List.rev !x
    ;;

    let wave_formats = X.wave_formats
  end)
