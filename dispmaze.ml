open Sdlevent
open Sdlkey

let wall1 = "wall1.jpg"
let wall2 = "wall2.jpg"

let floor = "floor.jpg"
let resolve = "resolve.jpg"

let start_sdl y x =
  Sdl.init [`VIDEO];
  Sdlvideo.set_video_mode (50 * y) (50 * x) []

let screen = start_sdl (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2)))

let rec wait_for_escape () =
  let ev = wait_event () in
    match ev with
      | KEYDOWN {keysym = KEY_ESCAPE} ->
          at_exit Sdl.quit
      | event ->
          wait_for_escape ()

let aff_resolve y x =
  let image = Sdlloader.load_image resolve in
  let position_of_image = Sdlvideo.rect (y * 50 - 50) (x * 50 - 50) y x in
    begin
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
	Sdlvideo.flip screen;
    end

let aff_image y x =
  let image = Sdlloader.load_image floor in
  let position_of_image = Sdlvideo.rect (y * 50 - 50) (x * 50 - 50) y x in
    begin
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
	Sdlvideo.flip screen;
    end

let aff_wallv i j =
  begin
      let image = Sdlloader.load_image wall1 in
  let position_of_image = Sdlvideo.rect (i * 50 - 55) (j * 50 - 50)  i j in
    begin
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
	Sdlvideo.flip screen;
    end
  end

let aff_wallh i j =
  begin
      let image = Sdlloader.load_image wall2 in
  let position_of_image = Sdlvideo.rect (i * 50 - 50) (j * 50 - 50) i j in
    begin
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
	Sdlvideo.flip screen;
    end
  end

let aff_wallh2 i j =
  begin
      let image = Sdlloader.load_image wall2 in
  let position_of_image = Sdlvideo.rect (i * 50 - 50) (j * 50 - 5) i j in
    begin
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
	Sdlvideo.flip screen;
    end
  end


let aff_wallv2 i j =
  begin
      let image = Sdlloader.load_image wall1 in
  let position_of_image = Sdlvideo.rect (i * 50 - 5) (j * 50 - 50)  i j in
    begin
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
	Sdlvideo.flip screen;
    end
  end


let pos_wall x y i j =
  begin
    if x = i then
      if j < y then
	aff_wallh x y
      else
	aff_wallh x j
    else if y = j then
      if x > i then
	aff_wallv x j
      else
	aff_wallv i j
  end

let rec aff_all_wall_sud x1 y1 x2 y2 maxx maxy =
  begin
    if (maxx = x1 && maxy = y1) || (maxx = x2 && maxy = y2)
    then
      begin
	aff_all_wall_sud x1 y1 x2 y2 (maxx - 1) maxy
      end
    else if maxx > 0 then
      begin
	aff_all_wall_sud x1 y1 x2 y2 (maxx - 1) maxy;
	aff_wallh2 maxx maxy
      end
  end

let rec aff_all_wall_nord x1 y1 x2 y2 maxx maxy =
  begin
    if (maxx = x1 && maxy = y1) || (maxx = x2 && maxy = y2)
    then
      begin
	aff_all_wall_nord x1 y1 x2 y2 (maxx - 1) maxy
      end
    else if maxx >= 0 then
      begin
	aff_all_wall_nord x1 y1 x2 y2 (maxx - 1) maxy;
	aff_wallh2 maxx maxy
      end
  end

let rec aff_all_wall_droite x1 y1 x2 y2 maxx maxy =
  begin
    if (maxx = x1 && maxy = y1) || (maxx = x2 && maxy = y2)
    then
      aff_all_wall_droite x1 y1 x2 y2 maxx (maxy - 1)
    else if maxy >= 0 then
      begin
	aff_wallv2 maxx maxy;
	aff_all_wall_droite x1 y1 x2 y2 maxx (maxy - 1)
      end
  end

let rec aff_all_wall_gauche x1 y1 x2 y2 maxx maxy =
  begin
    if (maxx = x1 && maxy = y1) || (maxx = x2 && maxy = y2)
    then
      aff_all_wall_gauche x1 y1 x2 y2 maxx (maxy - 1)
    else if maxy >= 0 then
      begin
	aff_wallv maxx maxy;
	aff_all_wall_gauche x1 y1 x2 y2 maxx (maxy - 1)
      end
  end

let display_wall x1 y1 x2 y2 i j =
  begin
    aff_all_wall_sud x1 y1 x2 y2 i j;
    aff_all_wall_nord x1 (y1 - 1) x2 (y2 - 1) i 0;
    aff_all_wall_droite x1 y1 x2 y2 i j;
    aff_all_wall_gauche x1 y1 x2 y2 1 j
  end

let rec aff_all_img i j =
  begin
    aff_image (i) (j);
  end

let rec printf_wall x y =
  if (x != 0 || y != 0)
  then
    if (x != 0)
    then
      begin
	aff_all_img x y;
	printf_wall (x - 1) y;
      end
    else printf_wall (int_of_string (Sys.argv.(1))) (y - 1)
  else
    ();
