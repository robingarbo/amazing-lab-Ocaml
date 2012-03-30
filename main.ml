let main () =
  if (int_of_string Sys.argv.(1) > 1 && int_of_string Sys.argv.(2) > 1)
  then
    begin
  Maze.init_maze ();
  Maze.open_door ();
  Maze.display ()
    end
  else
    print_endline "USAGE ./step2 x y > 1"

let _ = main ()
