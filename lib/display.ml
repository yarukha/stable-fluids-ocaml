open Variables
module Grid = Bigarray.Array2
module Field = Bigarray.Array3

let pre_display () = 
  GlDraw.viewport  ~x:0 ~y:0 ~w:!Variables.win_x ~h:!Variables.win_y;
  GlMat.mode `projection ;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0.,1.) ~y:(0.,1.);
  GlClear.color ~alpha:1. (0.,0.,0.);
  GlClear.clear [`color]

let post_display () = 
  Glut.swapBuffers ()


let draw_density dens = 
  GlDraw.begins `quads;
  let h = 1. /. (float_of_int n) in 
  for i =  1 to n do 
    let x = (float_of_int i -. 0.5) *. h in 
    for j = 1 to n do 
      let y  = (float_of_int j -. 0.5) *. h in 
      let d00 = Grid.get dens i j in 
      let d01 = Grid.get dens i (j+1) in 
      let d10 = Grid.get dens (i+1) j in 
      let d11 = Grid.get dens (i+1) (j+1) in
      GlDraw.color (d00,d00,d00); GlDraw.vertex2 (x,y);
      GlDraw.color (d10,d10,d10); GlDraw.vertex2 (x+.h,y);
      GlDraw.color (d11,d11,d11); GlDraw.vertex2 (x+.h,y+.h);
      GlDraw.color (d01,d01,d01); GlDraw.vertex2 (x,y+.h);
    done;
  done;
  GlDraw.ends ()

let draw_velocity vel = 
  GlDraw.color (1.,1.,1.);
  GlDraw.line_width 1.;
  GlDraw.begins `lines ;
  let h = 1. /. (float_of_int n) in 
  for i =  1 to n do 
    let x = (float_of_int i -. 0.5) *. h in 
    for j = 1 to n do 
      let y  = (float_of_int j -. 0.5) *. h in 
      GlDraw.vertex2 (x,y);
      GlDraw.vertex2 (x +. Field.get vel i j 0,y+. Field.get vel i j 1);
    done;
  done;
  GlDraw.ends ()

let update_left_click i j = 
  if Variables.mouse.(0) then
    Grid.set Iterator2.dens i j Variables.def_density_source

let update_right_click i j = 
  if Variables.mouse.(2) then 
    Field.set Iterator2.vel i j 0 Variables.(def_force_source *. float_of_int (!m_x - !om_x));
    Field.set Iterator2.vel i j 1 Variables.(def_force_source *. float_of_int (!m_y - !om_y))

let update_middle_click i j = 
  if Variables.mouse.(1) then 
    for i' = max 1 (i-3) to min n (i+3) do 
      for j' = max 1 (j-3) to min n (j+3) do 
        if Variables.find_obstacle i' j' then 
          Variables.remove_obstacle i' j' 
        else 
          Variables.add_obstacle i' j'  
        done;
      done


let update_source () = 
  if Variables.mouse.(0)||Variables.mouse.(2)||Variables.mouse.(1) then
    let i = int_of_float @@ ((float_of_int !Variables.m_x) /. (float_of_int !Variables.win_x)) *. float_of_int((n+1)) in 
    let j = int_of_float @@ ((float_of_int (!Variables.win_y - !Variables.m_y)) /. (float_of_int !Variables.win_y)) *. float_of_int((n+1)) in
    if (i<1 || i>n || j< 1 || j > n) then () else 
    update_left_click i j ;
    update_middle_click i j ;
    update_right_click i j

    



let my_init () = 
  GlClear.color ~alpha:1. (0.,0.,0.);
  GlClear.clear [`color];
  Glut.swapBuffers ();
  GlClear.clear [`color];
  Glut.swapBuffers ()





let key_func = fun ~key ~x ~y ->
  let _ =x,y in match key with
      |100->Variables.display_dens:= not !Variables.display_dens
      |99->Grid.fill Iterator2.dens 0.; Field.fill Iterator2.vel 0.;Variables.reset_obstacles ()
      |112->Variables.periodic := not !Variables.periodic
      |27(*esc*) -> exit 0
      | _ -> ()


let mouse_func = 
  fun ~button ~state ~x ~y -> 
    Variables.om_x := x;
    Variables.om_y := y;
    Variables.m_x := x;
    Variables.m_y := y;
    let i = match button with  
      |Glut.LEFT_BUTTON->0
      |Glut.MIDDLE_BUTTON->1
      |Glut.RIGHT_BUTTON->2
      |Glut.OTHER_BUTTON(_)->0 in 
    let b = match state with 
      |Glut.DOWN->true
      |Glut.UP->false in 
      flush stdout;
    Variables.mouse.(i)<-b
  
let motion_func = 
  fun ~x ~y -> 
    Variables.m_x:=x;
    Variables.m_y:=y


let reshape_func = 
  fun ~w ~h ->
    Glut.reshapeWindow ~w:w ~h:h;
    Variables.win_x:=w;
    Variables.win_y:=h

let idle_func = 
  Some (fun () -> 
    update_source ();Iterator2.step ();
  Glut.postRedisplay ();
  flush stdout
  )

let display_func = 
  fun () -> 
    pre_display ();
    begin
    if !Variables.display_dens then 
      draw_density Iterator2.dens 
    else
      draw_velocity Iterator2.vel;
    end;
    post_display ()

let rec time_func = 
  fun ~value -> 
    let _ = value in 
    update_source ();
    Iterator2.step ();
    Glut.postRedisplay ();
    incr Variables.image_count;
    let new_t = Unix.gettimeofday () in 
    if new_t -. !Variables.t > 1. then begin
      Format.printf "FPS:%i\n%!" !Variables.image_count;
      Variables.image_count:=0;
      Variables.t:=new_t
    end else ();
    Glut.timerFunc ~ms:Variables.t_update ~cb:time_func ~value:0




let main () =
  ignore(Glut.init ~argv:Sys.argv);
  Glut.initDisplayMode ~double_buffer:true ~alpha:true ();
  Glut.initWindowPosition ~x:0 ~y:0;
  Glut.initWindowSize ~w:!Variables.win_x ~h:!Variables.win_y ;
  ignore(let _ = Glut.createWindow ~title:"Stable Fluids" in ());
  my_init ();

  Variables.t := Unix.gettimeofday ();
  Variables.image_count := 0;
  Format.printf "\n-Esc to quit\n-Left click to add density\n-Right click to add force\n-d to change display\n-pto set periodicity\n-c to reset\n%!";
  Glut.keyboardFunc ~cb:key_func;
  Glut.mouseFunc ~cb:mouse_func;
  Glut.motionFunc ~cb:motion_func;
  Glut.reshapeFunc ~cb:reshape_func;
  Glut.timerFunc ~ms:Variables.t_update ~cb:time_func ~value:0;
  Glut.displayFunc ~cb:display_func;
  Glut.mainLoop();



