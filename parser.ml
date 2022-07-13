
type parseobj = Leaf of  string* string * bool* int * parseobj option * string list option |
 Constructor of string * string * parseobj * parseobj list * bool  | None;;

let newleaf n t = Leaf ( n, t,  true,  -1, None, None);;

let newconstructor op t child = if List.length child = 2 then Constructor ("constructor", t, op, child,true)
else Constructor ("constructor", t, op, child, false);;

let getType x = match x with
 Leaf (_,t,_,_,_,_) -> t
 |_-> "" ;;

type parser =  string list -> parseobj;;

let rec sublist list i j =
  if i > j then
    []
  else
    (List.nth list i) :: (sublist list (i+1) j);;
	
let rec intersect a b = match a with
	 c::d -> if List.mem c b then true else intersect d b
	 |_ -> false;;

let binaryn par1 par2 op exp n = let l = List.length exp in
if n < 2 || n > l-3 then None else
if l < 5 then None else let o = op [List.nth exp n] in
	
	if o	= None then None else
if not ((List.nth exp 0) = "(") || not 	((List.nth exp (l-1) ) = ")") then None else
let p1 =  par1 (sublist exp 1 n)  in let p2 = 	par2 (sublist exp n (l-1)) in
if p1 = None || p2 = None then None else
	newconstructor o (getType o)  [p1;p2];; 
	
	
let rec prebinary par1 par2 op exp n 	= if n > List.length exp-3 then None else
	let test =  binaryn par1 par2 op exp n in
	if not ( test = None ) then test else prebinary par1 par2 op exp (n+1);;	 		

	let binary par1 par2 op exp = prebinary par1 par2 op exp 0;;
	
let rec findleftaux par exp n = if n > List.length exp - 2 then 0 else
	let test = par (sublist exp 0 n) in
	if not (test = None) then n else
	  	findleftaux par exp (n + 1);;
		
let findleft par exp = findleftaux par exp 0;;




		
		
		
		
	
	


 
