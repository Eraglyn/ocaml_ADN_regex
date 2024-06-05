type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec explode_Aux num acc =
    if num < 0 then acc
    else explode_Aux (num - 1) (str.[num] :: acc)
  in
  explode_Aux (String.length str - 1) []

(* conversions *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'T' -> T
  | 'G' -> G
  | 'C' -> C
  | _ -> WC

let () = (* methode test pour base_of_char*)
  Printf.printf "%s\n" (match base_of_char 'G' with
                        | A -> "A"
                        | T -> "T"
                        | G -> "G"
                        | C -> "C"
                        | WC -> "WC");  (* Cela devrait imprimer "G" *)

  Printf.printf "%s\n" (match base_of_char '.' with
                        | A -> "A"
                        | T -> "T"
                        | G -> "G"
                        | C -> "C"
                        | WC -> "WC");;  (* Cela devrait imprimer "WC" *)


let dna_of_string (s : string) : base list =
  let rec aux acc index =
    if index < 0 then acc
    else
    aux ((base_of_char s.[index]) :: acc) (index - 1)
  in
  (aux [] (String.length s - 1))


  (* methode auxilliaire de string_of_dna*)
let base_to_char (b : base) : char =
  match b with
  | A -> 'A'
  | T -> 'T'
  | G -> 'G'
  | C -> 'C'
  | WC -> '.'

let string_of_dna (seq : dna) : string =
  match seq with 
  |[]-> ""
  |_->
  String.concat "" (List.map (fun base -> String.make 1 (base_to_char base)) seq)

  let () = (* pour tester la methode string_of_dna*)
  let sequence = [G; T; A; A; WC; WC; C; T] in
  let chaine = string_of_dna sequence in
  Printf.printf "%s\n" chaine  




(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)

(* methode recursive car plus simple pour répondre a cet question *)
let cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  let rec auxiliaire slice list =
    match (slice, list) with
    | ([], suf) -> Some suf
    | (x :: slice2, y :: list2) when x = y -> auxiliaire slice2 list2
    | _ -> None
  in
  auxiliaire slice list


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)

   (* Sublist permet d'obtenir la sous-liste de l entre les indices b et e *)
let rec sublist b e l =  
if b>=e then [] else 
match l with
  [] -> []
| hd :: tl -> 
    (* Si e <= 0 on a atteinds la fin de ce que l'on veut récupérer donc on finit par une liste vide, 
       sinon on rappel la fonction sur tl en décrémentant b et e *)
    let tail = if e<= 0 then [] else (sublist (b-1) (e-1) tl )in 
    if b>0 then tail else (hd :: tail)


let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  
  if slice = [] then Some ([], list) else
  if List.length slice > List.length list then None else
    (* L'idée de est_sous_list est la suivante : 
   On vérifie si slice est sous-liste de list, si non on renvoit -1, si oui on renvoie l'indice du début de slice dans list *)
  let rec est_sous_list s l index parcours= 
    match (s,l) with 
    | ([], _) -> index
    | (s_hd::s_tl, l_hd::l_tl) -> 
      (* Ici, on test les concordance entre slice et là où on se trouve dans list en gardant en mémoire l'indice du début de slice tant que les valeurs concordent, et on reprenant à l'indice parcours en cas de problème*)
      if s_hd=l_hd then est_sous_list s_tl l_tl (index) (parcours+1)
      else
        if index<>parcours then  
        est_sous_list slice l (parcours) (parcours)
      else est_sous_list slice l_tl (parcours+1) (parcours+1)
    |_ -> -1
  in
  let i = est_sous_list slice list 0 0 in
  (* Si i = -1, cela veut dire que l'on n'a pas trouvé l'existance de slice dans list 
     Si i != -1 alors on renvoie le tupple de ce qu'il se trouve avant slice et ce qui se trouve après slice *)
  if i= -1 then None else 
    Some ((sublist 0 (i-1) list),(sublist (List.length slice +i) (List.length list -1) list))
 
  
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let after = first_occ start list in
  if after = None || (snd (Option.get after)) = []then [] else 
  let between = first_occ stop (snd (Option.get after)) in
  if between = None then [] else
  (fst (Option.get between))::(slices_between start stop (snd (Option.get between))) 
(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A;T;G] [T;A;A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus =
  let rec find_elem e l =
     match l with 
     |[]-> e
     |hd::tl-> if (List.find_opt (fun (a,x)-> a=hd ) e) = None then find_elem ((hd,1)::e) tl
              else  find_elem (List.map (fun (a,x) -> if a=hd then (a,x+1) else (a,x)) e) tl
  in 
  let rec max e m = 
    match e with
    |[]-> m 
    |hd::tl-> if (snd hd )>(snd m) then max tl hd else max tl m
  in
  let element = (find_elem [] list) in 
  let size= List.length element in
  if size <= 0 then No_consensus 
  else if size = 1 then Full(fst (List.hd element)) 
  else let resp = max element (List.hd element) in 
  if (List.find_opt (fun (a,x)-> a<> (fst resp) && x = (snd resp)) element)<> None then No_consensus else
  Partial(fst resp, snd resp)

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)
 let rec take_first tab = 
  match tab with 
  |[] -> []
  | h::t -> if h=[] then take_first t else (List.hd h)::(take_first t)
let rec remove_first tab = 
  match tab with 
  |[]-> []
  |h::t-> if List.length h = 1 then [] else 
    if h =[] then remove_first t else(List.tl h)::remove_first t


let consensus_sequence (ll : 'a list list) : 'a consensus list =
  let rec build_pos tab  = 
    match tab with 
    |[] -> []
    |[[]]-> []
    |h::t-> (take_first tab)::build_pos ( remove_first tab)
  in
  let pos = build_pos ll in
  if pos = [] then [] else 
  List.map consensus pos
(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
