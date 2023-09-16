let extract (xs: 'a list) : ('a list * 'a * 'a list) list =
  let rec aux (acc: 'a list) (xs: 'a list) =
    match xs with
    | []    -> []
    | x::xs ->
       let r1 = (acc, x, xs) in
       r1 :: aux (x :: acc) xs
  in
  aux [] xs


let extract_pairs (xs: 'a list): ('a * 'a * 'a list) list =
  let aux (pre, x, post) =
    List.map (fun (pre', x', post') -> (x, x', List.concat [pre; pre'; post'])) (extract post)
  in
  List.concat_map aux (extract xs)


let string_of_list (ns: int list) =
  String.concat ", " (List.map string_of_int ns)


type operator =
  | Add
  | Sub
  | Mul
  | Div


type ast =
  | BinOp of ast * operator * ast
  | Lit of int


let solve (goal: int) (ns: int list) : ast list =
  let rec value_of ast =
    match ast with
    | Lit n -> n
    | BinOp (left, Add, right) -> value_of left + value_of right
    | BinOp (left, Sub, right) -> value_of left - value_of right
    | BinOp (left, Mul, right) -> value_of left * value_of right
    | BinOp (left, Div, right) -> value_of left / value_of right
  in
  let rec aux (nodes : ast list) =
    let add (x, y, rest) =
      aux (BinOp (x, Add, y) :: rest)
    in
    let sub (x, y, rest) =
      if x > y then
        aux (BinOp (x, Sub, y) :: rest)
      else if y > x then
        aux (BinOp (y, Sub, x) :: rest)
      else
        []
    in
    let mul (x, y, rest) =
      aux (BinOp (x, Mul, y) :: rest)
    in
    let div (x, y, rest) =
      if value_of y != 0 && value_of x mod value_of y == 0 then
        aux (BinOp (x, Div, y) :: rest)
      else if value_of x != 0 && value_of y mod value_of x == 0 then
        aux (BinOp (y, Div, x) :: rest)
      else
        []
    in
    let solved, unsolved = List.partition (fun ast -> value_of ast == goal) nodes
    in
    let extra_solved = List.concat_map (fun triple -> List.concat [add triple; sub triple; mul triple; div triple]) (extract_pairs unsolved)
    in
    List.append solved extra_solved
  in
  aux (List.map (fun x -> Lit x) ns)


let show (ast: ast) =
  let rec aux ast =
    match ast with
    | Lit n                    -> string_of_int n
    | BinOp (left, Add, right) -> Printf.sprintf "%s + %s" (aux left) (aux right)
    | BinOp (left, Sub, right) -> Printf.sprintf "%s - %s" (aux left) (aux right)
    | _                        -> aux' ast
  and aux' ast =
    match ast with
    | Lit n                    -> string_of_int n
    | BinOp (left, Add, right) -> Printf.sprintf "(%s)" (aux ast)
    | BinOp (left, Sub, right) -> Printf.sprintf "(%s)" (aux ast)
    | BinOp (left, Mul, right) -> Printf.sprintf "%s * %s" (aux' left) (aux' right)
    | BinOp (left, Div, right) -> Printf.sprintf "%s / %s" (aux' left) (aux' right)
  in
  aux ast


let main () =
  match List.map int_of_string (List.tl (Array.to_list Sys.argv)) with
  | goal :: ns ->
     let solutions = solve goal ns
     in
     List.iter (fun ast -> print_endline (show ast)) solutions
  | _ ->
     print_endline "No goal specified"


let _ = main ()
