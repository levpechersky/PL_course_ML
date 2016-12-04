local (* Part 1 helpers *)
  (* Count occurences of element in list *)
  fun occurences (element, lst) = length(List.filter (fn x => element=x) lst);

  fun common_aux (common_x, max_occurences, []) = common_x
  | common_aux (common_x, max_occurences, x::xs) = let
      val x_occurences = occurences(common_x, x::xs)
    in
      if x_occurences > max_occurences then common_aux(x, x_occurences, xs)
      else common_aux(common_x, max_occurences, xs)
    end;

  (**)
  (* TODO may exctract x2=x1+1 *)
  fun first_seq [] = []
  | first_seq [x] = [x]
  | first_seq (x1::x2::xs) = if x2=x1+1 then x1::first_seq(x2::xs) else [x1];

  (* Gets a list and splits it into list of sequences *)
  (* Example:break_to_seq [1,2,3,1,2,2,3,4,5] = [[1,2,3],[1,2],[2,3,4,4]] *)
  fun break_to_seq [] = []
  |break_to_seq lst = let
      val seq = first_seq lst
    in
      seq::break_to_seq(List.drop(lst, length(seq)))
    end;

  (* Gets some compare function, e.g. op< and a list *)
  (* Returns maximal element according to definition of less *)
  (* For example: max(op>, lst) will return minimal element *)
  fun max(_, [x]) = x
  | max(less, x::xs) = let
    val tail_largest = max(less,xs)
  in
    if less(x, tail_largest) then tail_largest else x
  end;
in
  (* For list, returns most common element in that list
     Example: common [“PL”, “PL”, “is”, “fun”, “PL”] = “PL” *)
  fun common (x::xs) = common_aux(x, 1, x::xs);

  (* For list of integers returns longest increasing by 1 sequence
     Example: sequence[1,2,3,4,1,5,5] = [1,2,3,4] *)
  fun sequence [] = []
  | sequence lst = max((fn(x,y) => length(x)<length(y)), break_to_seq(lst));

  (* For list of elements returns list of all-unique pairs
     of (element, occurences of element)
     Example: thin [3,3,2,7,5,7,5] = [(3,2), (2,1), (7,2), (5, 2)] *)
  fun thin [] = []
  | thin(x::xs) = (x, occurences(x,x::xs))::thin(List.filter (fn y => y<>x) xs);
end;


local (* All maze functions helpers *)

  fun transp ([]::_) = []
  | transp rows =
  (map hd rows) :: transp (map tl rows);

  (*
  x - line index, y - column index. Example:
  enumerate2d_row(0,0,["a","b","c"]);
  val it = [((0,0),"a"),((0,1),"b"),((0,2),"c")] : ((int * int) * string) list
  *)
  fun enumerate2d_row(x,y,[]) = []
  | enumerate2d_row(x,y,l::ls) = ((x,y),l)::enumerate2d_row(x,y+1,ls);

  (*
  x, y - starting indices
  Takes 2d array, returns 1d array of tuple (row,col)*(data)
  enumerate2d([["a","b","c"],["d","e","f"]]);
  val it =
    [((0,0),"a"),((0,1),"b"),((0,2),"c"),((1,0),"d"),((1,1),"e"),((1,2),"f")]
    : ((int * int) * string) list
  *)
  fun enumerate2d(_,_, []) = []
  | enumerate2d(x,y,row::rows) = enumerate2d_row(x, 0, row) @
    (enumerate2d(x+1,y,rows))

  (* l t r b = left top right bottom
  coords_room_to_node((4,5), (1,0,0,1));
  val it = ((4,5),[(4,4),(5,5)]) : (int * int) * (int * int) list
   *)
  fun coords_room_to_node((x,y), (l,t,r,b)) = ((x,y),
   (if l=1 then [(x, y-1)] else []) @
   (if t=1 then [(x-1, y)] else []) @
   (if r=1 then [(x, y+1)] else []) @
   (if b=1 then [(x+1, y)] else []));

  fun is_valid width height x y = x>=0 andalso y>=0 andalso
    x<width andalso y<height;

  (* valid - function is_valid with width height in closure already
  second argument - list of coords (int * int) list with coords of neighbors
  val v = is_valid 3 3; (* 3*3 board *)
  val v = fn : int -> int -> bool
  has_way_out(v, [(0,~1),(1,0)]); (* one of neighbors is outside - has way out *)
  val it = true : bool
  - has_way_out(v, [(0,2),(1,0)]); (* all neighbors inside - no way out *)
  val it = false : bool
  *)
  fun has_way_out(_, []) = false
  | has_way_out(valid, (x,y)::neighbors) = (not (valid x y))
    orelse has_way_out(valid, neighbors);

  (* As node consists of tuple (coords)*(neighbors coords list), this function
     returns neighbors coords list *)
  fun node_neighbors(_, x) = x;

  (* fun node_coords(x, _) = x; *)

  (* Given associated list and a key returns value of that key, or empty list if
     key doesn't exist.
  val l =
    [((0,0),[(~1,0),(0,1)]),((0,1),[(0,0),(1,1)]),((0,2),[(0,3)]),((1,0),[])]
    : ((int * int) * (int * int) list) list
  - assoc(l, (0,1));
  val it = [(0,0),(1,1)] : (int * int) list
  *)
  fun assoc([], _) = []
  | assoc((x,y)::pairs, k) = if x=k then y else assoc(pairs,k);

  (* Remove item from list *)
  fun remove(list, x) = List.filter (fn y => y<>x) list;

  (* Given associated list and list of keys returns list of pairs with those keys
  1st - associated list, i.e. list of pairs. 2-nd - list of keys to look for
  Returns pairs of (key, value), not only values
  val l =
    [((0,0),[(~1,0),(0,1)]),((0,1),[(0,0),(1,1)]),((0,2),[(0,3)]),((1,0),[])]
    : ((int * int) * (int * int) list) list
  assoc_mult(l, [(0,0), (0,2)]);
  val it = [((0,0),[(~1,0),(0,1)]),((0,2),[(0,3)])]
   : ((int * int) * (int * int) list) list
  *)
  fun assoc_mult([], _) = []
  | assoc_mult(_, []) = []
  | assoc_mult(pairs, k::ks) = let
    val k_values = assoc(pairs,k)
  in
      if null(k_values) then assoc_mult(pairs, ks) else
      (k, k_values)::assoc_mult(pairs, ks)
   end;

  (* difference of lists, works exactly as difference of sets, but slower *)
  infix \;
   fun [] \ _ = []
   | lst \ [] = lst
   | lst \ (x::xs) = remove(lst,x) \ xs;

  (* Gets a maze and returns 1d list of (x,y)*(list of neighbors' coords)
  build_graph [[(0,1,1,0),(1,0,0,1),(0,0,1,0)],[(0,0,0,0),(0,1,1,1),(1,0,0,1)],
  [(0,0,0,0),(0,1,0,1),(0,1,1,1)]];
  val it =
    [((0,0),[(~1,0),(0,1)]),((0,1),[(0,0),(1,1)]),((0,2),[(0,3)]),((1,0),[]),
     ((1,1),[(0,1),(1,2),(2,1)]),((1,2),[(1,1),(2,2)]),((2,0),[]),
     ((2,1),[(1,1),(3,1)]),((2,2),[(1,2),(2,3),(3,2)])]
    : ((int * int) * (int * int) list) list
  *)
  fun build_graph matrix = (map coords_room_to_node)(enumerate2d(0,0,matrix));

  (* Given node returns all it's neighbor nodes in maze *)
  fun neighbors_of(node, maze) = assoc_mult(maze, node_neighbors(node));

  fun neighbors_of_mult([], _) = []
  | neighbors_of_mult(_, []) = []
  | neighbors_of_mult(node::nodes, maze) = neighbors_of(node, maze)@neighbors_of_mult(nodes, maze);

  (* Check if exists path between two nodes
  Initializing:
  dest - destination node
  unvisited - all but source node
  node::edge - list of source node only
  Example:
  path(dest, remove(graph, src), [src]);
  Returns 1 if there's path between 2 points, 0 otherwise
  *)
  fun path(_, _, []) = 0
  | path(dest, unvisited, node::edge) = if node=dest then 1 else let
      val neighbors_of_node = neighbors_of(node, unvisited)
    in
      path(dest, (unvisited \ neighbors_of_node), edge @ neighbors_of_node)
    end;

  fun tuple_prefix(_, []) = []
  | tuple_prefix(prefix, x::xs) = (prefix, x)::tuple_prefix(prefix,xs);

  fun all_pairs_of [] = []
  | all_pairs_of(x::xs) = tuple_prefix(x, xs) @ all_pairs_of(xs);

  fun path_2_points(src, dest, maze) = path(dest, remove(maze, src), [src]);

  fun exit_maze_aux(_, []) = false
  | exit_maze_aux([], _) = false
  | exit_maze_aux(graph, (src, dest)::src_dest_pairs) =
    path_2_points(src, dest,graph)>0 orelse exit_maze_aux(graph, src_dest_pairs);

  fun neighbors_aux ((_,_,1,_),(0,_,_,_)) = false
  | neighbors_aux ((_,_,0,_),(1,_,_,_)) = false
  | neighbors_aux ((_,_,_,1),(_,0,_,_)) = false
  | neighbors_aux ((_,_,_,0),(_,1,_,_)) = false
  | neighbors_aux _ = true;

in
  fun are_nighbours(a,b) = neighbors_aux(a,b) andalso neighbors_aux(b,a);

local
  fun row_is_valid [] = true
  | row_is_valid [x] = true
  | row_is_valid(x1::x2::xs) = are_nighbours(x1,x2) andalso row_is_valid(x2::xs);

  fun rows_valid [] = true
  | rows_valid(x::xs) = row_is_valid x andalso rows_valid xs;
in
  fun is_valid_maze maze = rows_valid maze andalso rows_valid(transp(maze))
end;

  fun exit_maze maze = let
    val graph = build_graph maze
    val pred = is_valid (length(hd(maze))) (length(maze))
    val exits = List.filter (fn x => has_way_out(pred, node_neighbors(x))) graph
  in
  (*TODO if maze invalid - false *)
    exit_maze_aux(graph, all_pairs_of(exits))
  end;

end;
