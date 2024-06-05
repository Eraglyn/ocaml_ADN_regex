open Regex_base

let rec repeat n l =
  if n=0 then [] else 
  let rec aux n w = 
    if n>1 then aux (n-1) (w@l) else w
  in
  aux n l
  

let rec expr_repeat n e =
  if n=0 then Eps else 
    let rec aux n w = 
      if n>1 then Concat(e, (aux (n-1) w)) else w
    in
    aux n e
  

let rec is_empty e =
  match e with 
  | Eps -> true
  | Base(a) -> false
  | Joker -> false
  | Concat(a,b) -> is_empty a && is_empty b
  | Alt(a,b) -> is_empty a && is_empty b
  | Star(a) -> is_empty a 

let rec null e =
  match e with 
  | Eps -> true
  | Base(a) -> false
  | Joker -> false
  | Concat(a,b) ->  null a && null b
  | Alt(a,b) -> null a || null b
  | Star(a) -> true
  

let rec is_finite e =
  match e with 
  | Eps -> true
  | Base(a) -> true
  | Joker -> true
  | Concat(a,b) ->  is_finite a && is_finite b
  | Alt(a,b) -> is_finite a && is_finite b 
  | Star(a) -> if is_empty a then true else false

let product l1 l2 =
  let rec build_lan li1 li2 acc = 
    match li1 with 
    |[] -> acc
    |h::t-> build_lan t l2 ((List.map (fun x -> h@x) l2)@acc)
    in
    build_lan l1 l2 []

let rec transform l = 
  match l with 
  |[]-> []
  |h::t-> [h]::(transform t)
let rec enumerate alphabet e =
  match e with 
  | Eps -> Some [[]]
  | Base(a) -> if (List.mem a alphabet) then Some[[a]] else Some [[]]
  | Joker -> Some (transform alphabet)
  | Concat(a,b) -> 
  let e1 = (enumerate alphabet a) in 
  let e2 = (enumerate alphabet b) in 
  if e1 = None ||e2=None then None else Some (product(Option.get e1) (Option.get e2))

  | Alt(a,b) -> let e1 = (enumerate alphabet a) in 
  let e2 = (enumerate alphabet b) in 
  if e1 = None ||e2=None then None else  Some ((Option.get e1)@(Option.get e2))
  | Star(a) -> None


let rec alphabet_expr e =
  let rec alphabet_expr_aux e = 
    match e with 
    | Eps -> []
    | Base(a) -> [a] 
    | Joker -> []
    | Concat(a,b) -> (alphabet_expr a)@(alphabet_expr b)  
    | Alt(a,b) -> (alphabet_expr a)@(alphabet_expr b)  
    | Star(a) -> (alphabet_expr a)
  in
  sort_uniq (alphabet_expr_aux e)

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  let alphabet = union_sorted (alphabet_expr e) w in
  let language = enumerate alphabet e in 
  if language = None then Infinite else
  if List.mem w (Option.get language) then Accept else Reject

