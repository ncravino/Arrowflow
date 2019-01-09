type _ arrowflow = Op : ('a -> 'b) -> ('a -> 'b) arrowflow

let compose_with a b = match (a, b) with Op f, g -> Op (fun x -> g (f x))

let merge_with a b =
  match (a, b) with Op f, g -> Op (fun (x, y) -> (f x, g y))

let apply wkflow x = match wkflow with Op f -> f x

let start f = Op f

module Operators = struct
  let ( >>> ) a b = compose_with a b

  let ( >*> ) a b = merge_with a b

  let ( >$ ) wkflow x = apply wkflow x
end
