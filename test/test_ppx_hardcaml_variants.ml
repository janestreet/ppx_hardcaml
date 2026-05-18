open! Import

module A = struct
  type 'a t =
    { a1 : 'a [@bits 8]
    ; a2 : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module B = struct
  type 'a t =
    { b1 : 'a [@bits 4]
    ; b2 : 'a [@bits 12]
    }
  [@@deriving hardcaml]
end

module C = struct
  type 'a t = { c1 : 'a [@bits 32] } [@@deriving hardcaml]
end

module V : sig
  type 'a t =
    | A of 'a A.t
    | B of 'a B.t
    | C of 'a C.t
  [@@deriving hardcaml_variants]
end = struct
  type 'a t =
    | A of 'a A.t
    | B of 'a B.t
    | C of 'a C.t
  [@@deriving hardcaml_variants]
end

module VA = V.Make (struct
    let kind = V.Kind.A
  end)

module VB = V.Make (struct
    let kind = V.Kind.B
  end)

module VC = V.Make (struct
    let kind = V.Kind.C
  end)

let%expect_test "coercion functions" =
  let a = V.A { a1 = 1; a2 = 2 } in
  let b = V.B { b1 = 3; b2 = 4 } in
  let c = V.C { c1 = 5 } in
  print_s [%message (V.a_exn a : int A.t)];
  print_s [%message (V.b_exn b : int B.t)];
  print_s [%message (V.c_exn c : int C.t)];
  [%expect
    {|
    ("V.a_exn a" (
      (a1 1)
      (a2 2)))
    ("V.b_exn b" (
      (b1 3)
      (b2 4)))
    ("V.c_exn c" ((c1 5)))
    |}];
  show_raise (fun () -> V.a_exn b);
  show_raise (fun () -> V.b_exn a);
  [%expect
    {|
    (raised (
      "expected A, got" (
        t (
          B (
            (b1 _)
            (b2 _))))))
    (raised (
      "expected B, got" (
        t (
          A (
            (a1 _)
            (a2 _))))))
    |}]
;;

let%expect_test "type equivalence" =
  let a : int V.t = A { a1 = 10; a2 = 20 } in
  let mapped : int VA.t = VA.map a ~f:(fun x -> x * 2) in
  print_s [%message (mapped : int V.t)];
  print_s [%message (V.a_exn mapped : int A.t)];
  [%expect
    {|
    (mapped (
      A (
        (a1 20)
        (a2 40))))
    ("V.a_exn mapped" (
      (a1 20)
      (a2 40)))
    |}]
;;

let%expect_test "port_names_and_widths" =
  print_s [%message (VA.to_list VA.port_names_and_widths : (string * int) list)];
  print_s [%message (VB.to_list VB.port_names_and_widths : (string * int) list)];
  print_s [%message (VC.to_list VC.port_names_and_widths : (string * int) list)];
  [%expect
    {|
    ("VA.to_list VA.port_names_and_widths" (
      (a1 8)
      (a2 16)))
    ("VB.to_list VB.port_names_and_widths" (
      (b1 4)
      (b2 12)))
    ("VC.to_list VC.port_names_and_widths" ((c1 32)))
    |}]
;;

let%expect_test "operations with correct kind" =
  let a : int VA.t = A { a1 = 5; a2 = 10 } in
  print_s [%message "map" ~_:(VA.map a ~f:(( + ) 1) : int VA.t)];
  [%expect
    {|
    (map (
      A (
        (a1 6)
        (a2 11))))
    |}];
  VA.iter a ~f:(fun x -> printf "%d " x);
  [%expect {| 5 10 |}];
  let a1 : int VA.t = A { a1 = 1; a2 = 2 } in
  let a2 : int VA.t = A { a1 = 10; a2 = 20 } in
  print_s [%message "map2" ~_:(VA.map2 a1 a2 ~f:( + ) : int VA.t)];
  [%expect
    {|
    (map2 (
      A (
        (a1 11)
        (a2 22))))
    |}];
  VA.iter2 a1 a2 ~f:(fun x y -> printf "(%d,%d) " x y);
  [%expect {| (1,10) (2,20) |}]
;;

let%expect_test "operations with mismatched kind" =
  let a : int V.t = A { a1 = 1; a2 = 2 } in
  let b : int V.t = B { b1 = 1; b2 = 2 } in
  let c : int V.t = C { c1 = 42 } in
  show_raise (fun () -> VA.iter b ~f:(fun _ -> ()));
  show_raise (fun () -> VB.map c ~f:(fun x -> x * 2));
  show_raise (fun () -> VB.to_list a);
  [%expect
    {|
    (raised (
      "mismatched tag"
      (kind A)
      (t (
        B (
          (b1 _)
          (b2 _))))))
    (raised ("mismatched tag" (kind B) (t (C ((c1 _))))))
    (raised (
      "mismatched tag"
      (kind B)
      (t (
        A (
          (a1 _)
          (a2 _))))))
    |}];
  show_raise (fun () -> VA.iter2 a b ~f:(fun _ _ -> ()));
  show_raise (fun () -> VA.map2 a c ~f:( + ));
  show_raise (fun () -> VA.map2 b b ~f:( + ));
  [%expect
    {|
    (raised (
      "mismatched tag"
      (kind A)
      (t (
        B (
          (b1 _)
          (b2 _))))))
    (raised ("mismatched tag" (kind A) (t (C ((c1 _))))))
    (raised (
      "mismatched tag"
      (kind A)
      (t (
        B (
          (b1 _)
          (b2 _))))))
    |}]
;;

let%expect_test "validate" =
  let a : int V.t = A { a1 = 1; a2 = 2 } in
  let b : int V.t = B { b1 = 3; b2 = 4 } in
  VA.validate a;
  VB.validate b;
  print_endline "valid cases ok";
  [%expect {| valid cases ok |}];
  show_raise (fun () -> VA.validate b);
  show_raise (fun () -> VB.validate a);
  [%expect
    {|
    (raised (
      "mismatched tag"
      (kind A)
      (t (
        B (
          (b1 _)
          (b2 _))))))
    (raised (
      "mismatched tag"
      (kind B)
      (t (
        A (
          (a1 _)
          (a2 _))))))
    |}]
;;

let%expect_test "module reexports inside Make" =
  let a_inner : int VA.A.t = { a1 = 1; a2 = 2 } in
  print_s [%message (a_inner : int A.t)];
  print_s [%message (VA.A.port_names_and_widths : (string * int) A.t)];
  [%expect
    {|
    (a_inner (
      (a1 1)
      (a2 2)))
    (VA.A.port_names_and_widths (
      (a1 (a1 8))
      (a2 (a2 16))))
    |}]
;;

let%expect_test "map_variants" =
  let a : int V.t = A { a1 = 1; a2 = 2 } in
  let b : int V.t = B { b1 = 3; b2 = 4 } in
  let c : int V.t = C { c1 = 5 } in
  let double_a (x : int A.t) : int A.t = A.map x ~f:(fun v -> v * 2) in
  let triple_b (x : int B.t) : int B.t = B.map x ~f:(fun v -> v * 3) in
  let negate_c (x : int C.t) : int C.t = C.map x ~f:(fun v -> -v) in
  print_s [%message (V.map_variants a ~a:double_a ~b:triple_b ~c:negate_c : int V.t)];
  print_s [%message (V.map_variants b ~a:double_a ~b:triple_b ~c:negate_c : int V.t)];
  print_s [%message (V.map_variants c ~a:double_a ~b:triple_b ~c:negate_c : int V.t)];
  [%expect
    {|
    ("V.map_variants a ~a:double_a ~b:triple_b ~c:negate_c"
     (A (
       (a1 2)
       (a2 4))))
    ("V.map_variants b ~a:double_a ~b:triple_b ~c:negate_c"
     (B (
       (b1 9)
       (b2 12))))
    ("V.map_variants c ~a:double_a ~b:triple_b ~c:negate_c" (C ((c1 -5))))
    |}]
;;

let%expect_test "sexp_of_t" =
  let a = V.A { a1 = 1; a2 = 2 } in
  let b = V.B { b1 = 3; b2 = 4 } in
  print_s [%message (a : int V.t) (b : int V.t)];
  [%expect
    {|
    ((a (
       A (
         (a1 1)
         (a2 2))))
     (b (
       B (
         (b1 3)
         (b2 4)))))
    |}]
;;
