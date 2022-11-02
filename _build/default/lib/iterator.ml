open Owl
open Variables

module Grid = Arr
type grid = Grid.arr
type field = grid * grid
type system ={mutable velocity:field;mutable density:grid}


let s_shape = [|n+2;n+2|]

let density_source = let zsg = Grid.zeros s_shape in Grid.set_slice [[1;n];[n/2;n/2]] zsg (Grid.create [|n;1|] 10.); zsg
let force_source = Grid.zeros s_shape,Grid.zeros s_shape
let dens0 = Grid.zeros s_shape 
let w0 = Grid.zeros s_shape, Grid.zeros s_shape
let system= {velocity = w0;density=dens0}

let border_conditions = 0.

let rec for_ t0 i f = if i<=0 then t0 else f @@ for_ t0 (i-1) f


let print_shape s = 
  Printf.printf "[|"; (Array.iter (Printf.printf "%i;") s) ; Printf.printf "]\n"

(*modified get_slice to account for the border*)
type direction = |Left |Right |Down|Up
let slice_of_d = function |Left -> [[0;n+1];[0;n]] |Right->[[0;n+1];[1;n+1]]|Up->[[1;n+1];[0;n+1]]|Down->[[0;n];[0;n+1]]
let g_slice d g = 
  let g' = Grid.get_slice (slice_of_d d) g in 
  match d with 
  |Left -> Grid.concat_horizontal g' (Arr.zeros [|n+2;1|])
  |Right -> Grid.concat_horizontal (Arr.zeros [|n+2;1|]) g'
  |Up -> Grid.concat_vertical g' (Arr.zeros [|1;n+2|])
  |Down -> Grid.concat_vertical (Arr.zeros [|1;n+2|]) g'




let add_source : grid -> grid -> grid=
  fun s g ->   
  Grid.(g + (dt $* s))


let diffuse_step (f0:grid) f :grid= 
  let m_1 = g_slice Left f in 
  let m_2 = g_slice Right f in 
  let m_3 = g_slice Down f in 
  let m_4 = g_slice Up f in
  let h = (4. +. a) in 
  Grid.(h $/ (f0 + (a $* (m_1 + m_2 + m_3 + m_4))))



let div (w : field) :grid=
  let w_x,w_y =w in 
  let m_1 = g_slice Right w_x in 
  let m_2 = g_slice Left w_x in 
  let m_3 = g_slice Up w_y in 
  let m_4 = g_slice Down w_y in
  Grid.(2. $/ (m_1 - m_2 + m_3 - m_4)) 

(*q and d are of size (n+2)*(n+2) *)
let q_step (d:grid) q :grid= 
  let m_1 = g_slice Right q in 
  let m_2 = g_slice Left q in 
  let m_3 = g_slice Up q in 
  let m_4 = g_slice Down q in
  Grid.(4. $/ (m_1+m_2+m_3+m_4 - d))




let diffuse f0 = 
  for_ f0 k (diffuse_step f0)

let advect_grid (u:field) g= 
  let pred i  = Grid.mapi_nd (fun t x -> (float_of_int t.(i)) -. dt *. x |> min (float_of_int n +. 0.5) |> max 0.5) in
  let pred_x = pred 0 (fst u) and pred_y = pred 1 (snd u) in 
  Grid.map2 (
  fun x y -> let x0 = int_of_float x and y0 = int_of_float y in 
  let x1 = 1+ x0 and y1 = 1 + y0 in 
  let s1 = Maths.trunc x and t1 = Maths.trunc y in 
  let s0 = 1. -. s1 and t0 = 1. -. t1 in
  s0 *. (t0 *. Grid.get g [|x0;y0|] +. t1 *. Grid.get g [|x0;y1|]) +. 
  s1 *. (t0 *. Grid.get g [|x1;y0|] +. t1 *. Grid.get g [|x1;y1|]) 
  ) pred_x pred_y
  
let advect_field u :field= 
  advect_grid u (fst u),advect_grid u (snd u)
let project w= 
  let d = div w in 
  let q0 = Grid.zeros @@ Grid.shape d in 
  let q = for_ q0 k (q_step d)  in 
  let w_x,w_y = w in 
  let m_1 = g_slice Right q in 
  let m_2 = g_slice Left q in 
  let m_3 = g_slice Up q in 
  let m_4 = g_slice Down q in 
  Grid.(w_x -= (2. $/ (m_1 - m_2)));
  Grid.(w_y -= (2. $/ (m_3 - m_4)))

let evolve_grid s w (g:grid) = 
  add_source s g |> diffuse |> advect_grid w |>
  Grid.copy_ ~out:g

let evolve_field : field -> field -> unit=
  fun s w ->  
  let s_x,s_y = s in 
  let w_x,w_y = w in 
  let w'_x = add_source s_x w_x |> diffuse in 
  let w'_y = add_source s_y w_y |> diffuse in 
  project (w'_x,w'_y);
  advect_field (w'_x,w'_y)|> project 

let evolve_system dens_s f_s = 
  evolve_field f_s system.velocity;
  evolve_grid dens_s system.velocity system.density

