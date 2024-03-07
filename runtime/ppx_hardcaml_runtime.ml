open Base

module Array = struct
  include Array

  let for_ length ~f =
    for i = 0 to length - 1 do
      f i
    done
  ;;
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
