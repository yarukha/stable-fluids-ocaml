open Bigarray



let n = Variables.n
let m = n+2
let dt = Variables.dt
let a = Variables.a
let k = Variables.k
let h = Variables.h

let dt0 = float_of_int n *. dt
let f_n = float_of_int n



let kind = Float32
let layout = c_layout
type grid = (float,float32_elt,c_layout) Array2.t
type field = (float,float32_elt,c_layout) Array3.t


let init_grid = Array2.init kind layout
let init_field = Array3.init kind layout


let density_source = init_grid m m (fun _ _ -> 0.)
let force_source = init_field m m 2 (fun _ _ _ -> 0.) 

let dens = init_grid m m (fun _ _ -> 0.)
let vel = init_field m m 2 (fun _ _ _-> 0.)


let d = init_grid m m (fun _ _ -> 0.)
let tmp_grid = init_grid m m (fun _ _ -> 0.)
let tmp_field = init_field m m 2 (fun _ _ _-> 0.)


let restrain_index i = 
  if i< 0.5 then 0.5 else if i > f_n +. 0.5 then f_n +. 0.5 else i


let set_bound_grid f = 
  begin
  if !Variables.periodic then 
  for i = 1 to n do 
    f.{0,i}<- f.{n,i};
    f.{i,0}<- f.{i,n};
    f.{n+1,i}<-f.{1,i};
    f.{i,n+1}<-f.{i,1};
  done
  else
  for i = 1 to n do 
    f.{0,i}<- f.{1,i};
    f.{i,0}<- f.{i,1};
    f.{n+1,i}<-f.{n,i};
    f.{i,n+1}<-f.{i,n};
  done end;
  Variables.iter_obstacles (fun (i,j)-> if (i>=1)&&(i<=n)&&(j>=1)&&(j<=n) then f.{i,j}<-0.);
  f.{0,0}<-0.5 *. (f.{1,0}+. f.{0,1});
  f.{0,n+1}<-0.5 *. (f.{1,n+1}+.f.{0,n});
  f.{n+1,0}<-0.5 *. (f.{n,0}+.f.{n+1,1}) ;
  f.{n+1,n+1}<-0.5 *. (f.{n,n+1}+.f.{n+1,n})



let set_bound_field w = 
  if !Variables.periodic then 
    for i = 1 to n do 
      w.{0,i,0}<- w.{n,i,0} ;      w.{0,i,1}<- w.{n,i,1};
      w.{i,0,0}<- w.{i,n,0};    w.{i,0,1}<- w.{i,n,1};
      w.{n+1,i,0}<- w.{1,i,0};    w.{n+1,i,1}<- w.{1,i,1};
      w.{i,n+1,0}<- w.{i,1,0};    w.{i,n+1,1}<- w.{i,1,1};
    done
  else 
  for i = 1 to n do 
    w.{0,i,0}<- -.w.{1,i,0} ;      w.{0,i,1}<- -.w.{1,i,0};
    w.{i,0,0}<- w.{i,1,0};    w.{i,0,1}<- -.w.{i,1,1};
    w.{n+1,i,0}<- -.w.{n,i,0};    w.{n+1,i,1}<- w.{n,i,1};
    w.{i,n+1,0}<- w.{i,n,0};    w.{i,n+1,1}<- -.w.{i,n,1};
  done;
  Variables.iter_obstacles (fun (i,j)->if ((i>=1)&&(i<=n)&&(j>=1)&&(j<=n)) then (w.{i,j,0}<-0.;w.{i,j,1}<-0.) else ());
  w.{0,0,0}<-0.5 *. (w.{1,0,0}+. w.{0,1,0});      w.{0,0,1}<-0.5 *. (w.{1,0,1}+. w.{0,1,1});
  w.{0,n+1,0}<-0.5 *. (w.{1,n+1,0}+.w.{0,n,0});  w.{0,n+1,1}<-0.5 *. (w.{1,n+1,1}+.w.{0,n,1});
  w.{n+1,0,0}<-0.5 *. (w.{n,0,0}+.w.{n+1,1,0}) ;  w.{n+1,0,1}<-0.5 *. (w.{n,0,1}+.w.{n+1,1,1}) ;
  w.{n+1,n+1,0}<-0.5 *. (w.{n,n+1,0}+.w.{n+1,n,0})  ;  w.{n+1,n+1,1}<-0.5 *. (w.{n,n+1,1}+.w.{n+1,n,1})


let add_source () = 
  for i = 0 to n+1 do 
    for j = 0 to +1 do 
      dens.{i,j}<-dens.{i,j} +. (dt *. density_source.{i,j})
    done;
  done

let add_force () = 
  for i = 0 to n+1 do 
    for j = 0 to n+1 do 
      vel.{i,j,0}<-vel.{i,j,0}+. (dt *. force_source.{i,j,0});
      vel.{i,j,1}<-vel.{i,j,1}+. (dt *. force_source.{i,j,1}) 
    done;
  done

let add_h_force () = 
  for i = 0 to n+1 do 
    for j = 0 to n+1 do 
      vel.{i,j,0}<-vel.{i,j,0} -. 0.1;
    done;
  done


let solver x0 x= 
  for _ = 0 to k-1 do 
    for i = 1 to n do 
      for j = 1 to n do 
        x.{i,j}<-h *.(x0.{i,j}+. a*. (x.{i-1,j} +. x.{i+1,j} +. x.{i,j-1} +. x.{i,j+1}))
      done;
    done;
  done;
  set_bound_grid x

let diffuse_dens () = 
  Array2.blit dens tmp_grid;
  solver tmp_grid dens;
  set_bound_grid dens

let diffuse_vel () = 
  Array3.blit vel tmp_field; 
  for _ = 0 to k-1 do 
    for i = 1 to n do 
      for j = 1 to n do 
        vel.{i,j,0}<-h *.(tmp_field.{i,j,0}+. a*. (vel.{i-1,j,0} +. vel.{i+1,j,0} +. vel.{i,j-1,0} +. vel.{i,j+1,0}));
        vel.{i,j,1}<-h *.(tmp_field.{i,j,1}+. a*. (vel.{i-1,j,1} +. vel.{i+1,j,1} +. vel.{i,j-1,1} +. vel.{i,j+1,1}))
      done;
    done;
  done;
  set_bound_field vel



let advect_dens () =
  Array2.blit dens tmp_grid; 
  for i = 1 to n do 
    for j = 1 to n do 
      let x = float_of_int i -. (dt0 *. vel.{i,j,0}) and y = float_of_int j -. (dt0 *. vel.{i,j,1}) in 
      let x=restrain_index x and y = restrain_index y in 
      let i0,j0=int_of_float x,int_of_float y in 
      let i1,j1= i0+ 1,j0+1 in
      let s1= x-.Owl.Maths.trunc x and t1 =y-. Owl.Maths.trunc y in 
      let s0 = 1. -. s1 and t0 = 1. -. t1 in 
      dens.{i,j}<- s0 *. (t0 *. tmp_grid.{i0,j0} +. t1 *. tmp_grid.{i0,j1}) +. 
                    s1 *. (t0 *. tmp_grid.{i1,j0} +. t1 *. tmp_grid.{i1,j1})
    done;
  done;
  set_bound_grid dens

let advect_vel () = 
  Array3.blit vel tmp_field;
  for i = 1 to n do 
    for j = 1 to n do 
      let x = float_of_int i -. (dt0 *. vel.{i,j,0}) and y = float_of_int j -. (dt0 *. vel.{i,j,1}) in 
      let x=restrain_index x and y = restrain_index y in 
      let i0,j0=int_of_float x,int_of_float y in 
      let i1,j1= i0+ 1,j0+1 in
      let s1= x -. Owl.Maths.trunc x and t1 = y-. Owl.Maths.trunc y in 
      let s0 = 1. -. s1 and t0 = 1. -. t1 in 
      vel.{i,j,0}<- s0 *. (t0 *. tmp_field.{i0,j0,0} +. t1 *. tmp_field.{i0,j1,0}) +. 
                    s1 *. (t0 *. tmp_field.{i1,j0,0} +. t1 *. tmp_field.{i1,j1,0});
      vel.{i,j,1}<- s0 *. (t0 *. tmp_field.{i0,j0,1} +. t1 *. tmp_field.{i0,j1,1}) +. 
                    s1 *. (t0 *. tmp_field.{i1,j0,1} +. t1 *. tmp_field.{i1,j1,1})
    done;
  done;
  set_bound_field vel


let compute_div ()= 
  for i = 1 to n do 
    for j = 1 to n do 
      d.{i,j}<-0.5*.(vel.{i+1,j,0}-.vel.{i-1,j,0}+.vel.{i,j+1,1}-.vel.{i,j-1,1})
    done;
  done;
  set_bound_grid d

let project_vel () = 
  compute_div ();
  Array2.fill tmp_grid 0.;
  for _ = 0 to k-1 do 
    for i = 1 to n do 
      for j = 1 to n do 
        tmp_grid.{i,j}<-0.25 *. (tmp_grid.{i+1,j}+.tmp_grid.{i-1,j}+.tmp_grid.{i,j+1}+.tmp_grid.{i,j-1}-.d.{i,j})
      done;
    done;
  set_bound_grid tmp_grid;
  done;
  for i = 1 to n do 
    for j = 1 to n do 
      vel.{i,j,0}<-vel.{i,j,0}-.(0.25*.(tmp_grid.{i+1,j}-.tmp_grid.{i-1,j}));
      vel.{i,j,1}<-vel.{i,j,1}-.(0.25*.(tmp_grid.{i,j+1}-.tmp_grid.{i,j-1}))
    done;
  done;
  set_bound_field vel

let step_density () = 
  add_source ();
  diffuse_dens ();
  advect_dens ()

let step_velocity () = 
  add_force ();
  diffuse_vel ();
  project_vel ();
  advect_vel ();
  project_vel ()


let print_grid g = 
  for i = 1 to n do 
    for j = 1 to n do 
      Format.printf "%f; " g.{i,j};
    done;
    print_newline ();
  done

let print_field f = 
  for i = 1 to n do 
    for j = 1 to n do 
      Format.printf "(%f,%f); " f.{i,j,0} f.{i,j,1};
    done;
    print_newline ();
  done
let step () = 
  step_velocity ();
  step_density ()


