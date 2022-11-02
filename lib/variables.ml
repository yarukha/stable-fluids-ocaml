let win_x = ref 800
let win_y = ref 600

let m_x = ref 0
let m_y = ref 0 
let om_x = ref 0 
let om_y = ref 0
let mouse = Array.make 3 false

let visc = 0.00001
let dt =0.1 
let a = visc *. dt
let h = 1./.(1. +. (4.*. a))
let k = 15
let n = 128


let fps = 60
let t_update = 1000 / 60

let def_density_source = 1.
let def_force_source = 2.
let periodic = ref false
let display_dens = ref true
let horizontal_force = ref false



let t= ref 0.
let image_count = ref 0
module P = struct type t = int * int let compare = compare end
module Point_set = Set.Make(P)

let obstacles = ref Point_set.empty

let iter_obstacles foo =
  Point_set.iter foo !obstacles 

let find_obstacle i j = 
  Point_set.mem (i,j) !obstacles 

let remove_obstacle i j =
  obstacles := Point_set.remove (i,j) !obstacles

let add_obstacle i j = 
  obstacles := Point_set.add (i,j) !obstacles

let reset_obstacles  ()= 
  obstacles := Point_set.empty