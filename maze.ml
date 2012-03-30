let x = ref 0
let y = ref 0
let start = ref 0
let finish = ref 0
type case = {posx : int; posy : int; index : int; grpid : int}
type door = {is_open : bool; index1 : int; index2 : int}
let lcase = ref []
let ldoor = ref []

let rec fill_case a b =
  if (b != 0)
  then (if (a != 0)
	then {posx = a; posy = b; index = ((!x * (b - 1)) + a); grpid = ((!x * (b - 1)) + a)}::fill_case (a - 1) b
	else fill_case !x (b - 1))
  else []

let rec fill_vdoor a b =
  if (b != 0)
  then (if (a > 1)
	then {is_open = false; index1 = ((!x * (b - 1)) + a - 1); index2 = ((!x * (b - 1)) + a)}::fill_vdoor (a - 1) b
	else fill_vdoor !x (b - 1))
  else []

let rec fill_hdoor a b =
  if (b > 1)
  then (if (a != 0)
	then {is_open = false; index1 = ((!x * (b - 1)) + a - !x); index2 = ((!x * (b - 1)) + a)}::fill_hdoor (a - 1) b
	else fill_hdoor !x (b - 1))
  else []

let rec fill_door a b = (fill_hdoor a b)@(fill_vdoor a b)

let rec is_finish list = match list with
  | h1::h2::list -> if (h1.grpid = h2.grpid) then (is_finish (h2::list)) else false
  | h1::list -> true
  | [] -> true

let rec getgrpid list id = match list with
    | head::list -> if (head.index = id) then head.grpid else (getgrpid list id)
    | [] -> -1

let rec setgrpid list src dst = match list with
  | head::list ->
      if (head.grpid = dst)
      then {posx = head.posx; posy = head.posy; index = head.index; grpid = src}::(setgrpid list src dst)
      else head::(setgrpid list src dst)
  | [] -> []

let rec setdoor list elem = match list with
  | head::list ->
      if (head.index1 = elem.index1 && head.index2 = elem.index2)
      then {is_open = true; index1 = head.index1; index2 = head.index2}::(setdoor list elem)
      else head::(setdoor list elem)
  | [] -> []

let rec open_door () =
  if ((is_finish !lcase) = false)
  then (let rand = List.nth !ldoor (Random.int (List.length !ldoor)) in
	  if (rand.is_open = false && (getgrpid !lcase rand.index1) != (getgrpid !lcase rand.index2))
	  then
	    begin
	      lcase := setgrpid !lcase (getgrpid !lcase rand.index1) (getgrpid !lcase rand.index2);
	      ldoor := setdoor !ldoor rand;
	      open_door ();
	    end
	  else open_door ())
  else ()

let getcase index = List.nth !lcase (index - 1)

let rec display_door list = match list with
  | head::list ->
      if head.is_open = false
      then
	begin
	  Dispmaze.pos_wall (getcase head.index1).posx (getcase head.index1).posy (getcase head.index2).posx (getcase head.index2).posy;
	  display_door list;
	end
      else display_door list
  | [] -> ()

let getindex1 c = c.index1

let getindex2 c = c.index2

let rec display_resolve list = match list with
  | head::list ->
      begin
	Dispmaze.aff_resolve (getcase head).posx (getcase head).posy;
	display_resolve list;
      end
  | [] -> ()

let rec init_start () = let i = List.nth !lcase (Random.int (List.length !lcase)) in
  if ((i.index mod !x = 0) || (i.index mod !x = 1) || (i.index <= !x) || (i.index > ((!x - 1) * !y)))
  then i.index
  else init_start ()

let rec init_finish () = let rand = List.nth !lcase (Random.int (List.length !lcase)) in
  match !start with
    | i when (i mod !x = 0) -> if ((rand.index mod !x) = 1) then rand.index else init_finish ()
    | i when (i mod !x = 1) -> if ((rand.index mod !x) = 0) then rand.index else init_finish ()
    | i when (i <= !x) -> if (rand.index > ((!x - 1) * !y)) then rand.index else init_finish ()
    | i when (i > ((!x - 1) * !y)) -> if (rand.index <= !x) then rand.index else init_finish ()
    | _ -> -1

let rec getadj index list = match list with
  | head::list ->
      if ((head.index1 = index) && (head.is_open = true))
      then head.index2::(getadj index list)
      else
	if ((head.index2 = index) && (head.is_open = true))
	then head.index1::(getadj index list)
	else getadj index list
  | [] -> []

let rec remove_elem a list = match list with
  | head::list -> if (head = a) then (remove_elem a list) else head::(remove_elem a list)
  | [] -> []

let rec solve index list =
  if (index = !finish)
  then [!finish]
  else
    match list with
      | head::list -> let a = (solve head (remove_elem index (getadj head !ldoor))) in
	  if (List.mem !finish a = true)
	  then index::a
	  else
	    begin
	      match list with
		| head::list -> index::(solve head (remove_elem index (getadj head !ldoor)))
		| [] -> []
	    end
      | [] -> []

let display () =
  begin
    ignore (Dispmaze.start_sdl !x !y);
    Dispmaze.printf_wall !x !y;
    display_resolve (solve !start (getadj !start !ldoor));
    Dispmaze.display_wall (getcase !start).posx (getcase !start).posy (getcase !finish).posx (getcase !finish).posy !x !y;
    display_door !ldoor;
    Dispmaze.wait_for_escape ()
  end

let init_maze () =
  begin
    Random.self_init ();
    x := int_of_string(Sys.argv.(1));
    y := int_of_string(Sys.argv.(2));
    lcase := fill_case !x !y;
    ldoor := fill_door !x !y;
    start := init_start ();
    finish := init_finish ();
   end

