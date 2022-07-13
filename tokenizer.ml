let ssub s n m = String.sub s n (m-n);;
let suf s n = let l = String.length s in ssub s n l;;
let  stringincaux s sub n = let l = String.length sub in if  n + l < String.length s then if ssub s n (l + n) = sub then true
else false else false;;
let rec stringin s sub n = if n < String.length s then
	if stringincaux s sub n then n else
		stringin s sub (n+1)
	else -1;;
let rec loop f arg = if not ((f arg) = arg) then loop f (f arg) else arg;;
let rec spaceParenthesis form = if form = "" then "" else if List.mem form.[0] ['(';')';',';'&';'.';':';'='; '+'; '*'] 
	then String.concat "" [" " ; String.make 1 form.[0]; " ";spaceParenthesis (suf form 1) ] 
else String.concat "" [ String.make 1 form.[0]; spaceParenthesis (suf form 1) ];;

let rec imp form = if form ="" then "" else if (String.length form) < 2 then form
 else if ssub form 0 2 = "->" || ssub form 0 2 = "<-" then 
String.concat "" [" " ; ssub form 0 2;  " "; imp (suf form 2)] else
	String.concat "" [String.make 1 form.[0]; imp (suf form 1)];;
	
let cleanaux form = if form.[0] = ' ' then suf form 1 else
	let l = String.length form -1 in if form.[l] = ' ' then ssub form 0 l
else  let p = stringin form "  " 0 in if not (p = -1) then String.concat "" [ssub form 0 p; suf form (p+1) ]  else form;;

let clean form = loop cleanaux form;;

let fix form = clean (imp (spaceParenthesis form));;

let tokenize form = String.split_on_char ' ' (fix form);;

let alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";
"v";"w";"x";"y";"z"];;
let predAlphabet = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";
"W";"X";"Y";"Z"];;

let fresh state alphabet = if List.exists (fun x -> not (List.mem x state)) alphabet then
	List.find (fun x -> not (List.mem x state)) alphabet 
else
	let deja = List.filter (fun x -> String.length x > 1 &&  x.[1] = '_') state in let nums = List.map (fun x -> suf x 3) deja
in let sorted = List.sort (fun x y -> if x = y then 0 else if x > y then 1 else -1)(List.map (fun x -> int_of_string x) nums ) in let l = List.length sorted in String.concat "" ["x_"; string_of_int (List.nth sorted (l - 1))] ;;
