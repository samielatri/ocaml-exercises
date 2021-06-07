(* *)
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree

(* Exo I *)

(* Q1 *)
let rec cons_all (x:'a) (xss:('a list) list) : ('a list) list =
  match xss with
      [] -> []
    | xs::xss -> (x::xs)::(cons_all x xss)
	
let cons_all (x:'a) (xss:('a list) list) : ('a list) list =
  List.map (fun xs -> x::xs) xss

(* Q2 *)
let rec branch_list (bt:'a btree) : ('a list) list =
  match bt with
      Empty -> [[]]
    | Node(x,bt1,bt2) ->
	(cons_all x ((branch_list bt1)@(branch_list bt2)))

(* Q3 *)
let rec is_branch (xs:'a list) (bt:' btree) : bool =
  match xs, bt with
      [], Empty -> true
    | x::xs, Node(y,bt1,bt2) ->
	(x=y) && ((is_branch xs bt1) || (is_branch xs bt2))
    | _ -> false

(* étu: moins bonne car construit la liste des branches *)	
let is_branch (xs:'a list) (bt:' btree) : bool =
  List.mem xs (branch_list bt)


(* tests: non demandés
let bt =
  Node(3, Node(5,Empty,Empty),
          Node(4,Empty,
               Node(2,Empty,Empty)))

let _ = assert (is_branch [] Empty)
let _ = assert (not (is_branch [3] bt))
let _ = assert (is_branch [3;4] bt)
let _ = assert (not (is_branch [3;2] bt))
let _ = assert (is_branch [3;4;2] bt)
let _ = assert (not (is_branch [3;2;4] bt))
let _ = assert (not (is_branch [3;4;2;1] bt))
*)
	
(* Exo II *)

(* Q1 *)
let rec sublist_tree (xs:'a list) : 'a btree =
  match xs with
      [] -> Empty
    | x::xs ->
	let bt = sublist_tree xs in
	  Node (x, bt, bt)

(* Q2 *)
let rec sublist_list (bt:'a btree) : ('a list) list =
  match bt with
      Empty -> [[]]
    | Node(x,bt1,bt2) ->
	(cons_all x (sublist_list bt1))@(sublist_list bt2)

(* Q3 *)
let rec is_sublist (xs:'a list) (bt:' btree) : bool =
  match xs, bt with
      [], _ -> true
    | x::xs', Node(y,bt1,bt2) ->
	if (x=y) then (is_sublist xs' bt1)
	else (is_sublist xs bt2)
    | _ -> false


(* Exo III *)
type dist_list = (string * string * float) list

(* Q1 *)
let rec dist (v1:string) (v2:string) (ds:dist_list) : float =
  match ds with
      [] -> raise Not_found
    | (w1,w2,d)::ds ->
	if ((w1=v1) && (w2=v2)) || ((w1=v2) && (w2=v1)) then d
	else (dist v1 v2 ds)

type dist_tab = (string * (string * float) list) list

(* Q2 *)
let dist (v1:string) (v2:string) (tds:dist_tab) : float =
  List.assoc v2 (List.assoc v1 tds)

(* Q3 *)
let rec add (k:'a) (v:'b) (kvss:('a * 'b list) list) : ('a *'b list) list =
  match kvss with
      (k',vs)::kvss' ->
	if (k=k') then (k, v::vs)::kvss'
	else (k',vs)::(add k v kvss')
    | [] -> [k,[v]]

(* Q4 *)
let add_dist (v1:string) (v2:string) (d:float) (tds:dist_tab) : dist_tab =
  (add v1 (v2,d) (add v2 (v1,d) tds))

(* Q5 *)
let rec build (ds: dist_list) : dist_tab =
  match ds with
      [] -> []
    | (v1,v2,d)::ds -> (add_dist v1 v2 d (build ds))

(* récurence terminale *)
let build (ds: dist_list) : dist_tab =
  let rec loop (ds:dist_list) (r:dist_tab) : dist_tab =
    match ds with
	[] -> r
      | (v1,v2,d)::ds -> (loop ds (add_dist v1 v2 d r))
  in
    (loop ds [])

(* itérateur *)
let build (ds: dist_list) : dist_tab =
  List.fold_left (fun r (v1,v2,d) -> (add_dist v1 v2 d r)) [] ds
    
(* Exo IV *)

(* Q1 *)
let rec list_remove (x:'a) (xs:'a list) : 'a list =
  match xs with
      [] -> []
    | x'::xs ->
	if (x=x') then xs
	else x'::(list_remove x xs)

(* Q2 *)
let rec sub_assoc (xs:'a list) (xvs:('a * 'b) list) : ('a * 'b) list =
  match xvs with
      [] -> []
    | (x,v)::xvs ->
	if (List.mem x xs) then (x,v)::(sub_assoc xs xvs)
	else (sub_assoc xs xvs)

(* itérateur *)
let sub_assoc (xs:'a list) (xvs:('a * 'b) list) : ('a * 'b) list =
  List.filter (fun (x,_) -> List.mem x xs) xvs

(* Q3 *)
let min_val_key (vds : ('a * 'b) list) : 'a =
  let rec loop (v':'a) (d:'b) (vds:('a * 'b) list) : 'a =
    match vds with
	[] -> v'
      | (v'',d')::vds ->
	  if (d' < d) then (loop v'' d' vds)
	  else (loop v' d vds)
  in
    match vds with
	[] -> invalid_arg "min_val_key"
      | (v',d)::vds -> (loop v' d vds)

(* Q4 *)
let rec sort_dist (v:string) (vs:string list)
              (map : (string * (string * int) list) list)
    : string list =
  match vs with
      [] -> [v]
    | vs ->
	let v' = min_val_key (sub_assoc vs (List.assoc v map)) in
	  v::(sort_dist v' (list_remove v' vs) map)
	    
