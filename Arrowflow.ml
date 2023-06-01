module Arrowflow = struct
  type _ arrowflow = Op : ('a -> 'b) -> ('a -> 'b) arrowflow

  let push_compose a b = match (a, b) with Op f, g -> Op (fun x -> g (f x))

  let compose_with a b = match (a, b) with Op f, Op g -> Op (fun x -> g (f x))

  let merge_with a b =
    match (a, b) with Op f, Op g -> Op (fun (x, y) -> (f x, g y))

  let apply wkflow x = match wkflow with Op f -> f x

  let start f = Op f

  module Operators = struct
    let ( >>> ) a b = push_compose a b

    let ( >>= ) a b = compose_with a b

    let ( |>> ) a b = push_compose (start a) b

    let ( >*> ) a b = merge_with a b

    let ( >$ ) wkflow x = apply wkflow x
  end

  let test i =
    let open Operators in
    let int_flow = (fun i -> i * 5) |>> string_of_int in
    let float_flow =
      float_of_int |>> (fun f -> f /. 2.0) >>> string_of_float
    in
    let wk_flow =
      start (fun s -> (s, s))
      >>= (int_flow >*> float_flow)
      >>> fun (a, b) -> String.concat ":" [a; b]
    in
    wk_flow >$ i

  let () =
    assert (
      List.map test [1; 2; 3; 4; 5]
      = ["5:0.5"; "10:1."; "15:1.5"; "20:2."; "25:2.5"] )
end
