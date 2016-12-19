(* Lev Pechersky 333815546 levpechersky@campus.technion.ac.il Kfir Taizi 208044610 kfirtaizi@campus.technion.ac.il *)

local (* Part 1 helpers *)
  (* Count occurences of element in list *)
  fun occurences (element, lst) = length(List.filter (fn x => element=x) lst);

  fun common_aux (common_x, max_occurences, []) = common_x
  | common_aux (common_x, max_occurences, x::xs) = let
      val x_occurences = occurences(x, x::xs)
    in
      if x_occurences > max_occurences then common_aux(x, x_occurences, xs)
      else common_aux(common_x, max_occurences, xs)
    end;

  (* Returns first continuous sequence (e.g. 5,6,7,8,9) in list
     Example: first_seq [1,3,4,5] = [1];
              first_seq [1,2,3,1] = [1,2,3];  *)
  fun first_seq [] = []
  | first_seq [x] = [x]
  | first_seq (x1::x2::xs) = if x2=x1+1 then x1::first_seq(x2::xs) else [x1];

  (* Gets a list and splits it into list of sequences
     Example:break_to_seq [1,2,3,1,2,2,3,4,5] = [[1,2,3],[1,2],[2,3,4,5]] *)
  fun break_to_seq [] = []
  | break_to_seq lst = let
      val seq = first_seq lst
    in
      seq::break_to_seq(List.drop(lst, length(seq)))
    end;

  (* Gets some compare function, e.g. op< and a list
     Returns maximal element according to definition of less
     For example: max(op>, lst) will return minimal element *)
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
  fun sequence lst = max((fn(x,y) => length(x)<length(y)), break_to_seq(lst));

  (* For list of elements returns list of all-unique pairs
     of (element, occurences of element)
     Example: thin [3,3,2,7,5,7,5] = [(3,2), (2,1), (7,2), (5, 2)] *)
  fun thin [] = []
  | thin(x::xs) = (x, occurences(x,x::xs))::thin(List.filter (fn y => y<>x) xs);
end;



local (* All maze functions helpers *)
  (* Transpose 2d list *)
  fun transp ([]::_) = []
  | transp rows =
  (map hd rows) :: transp (map tl rows)

  local
    fun enumerate2d_row(x,y,[]) = []
    | enumerate2d_row(x,y,l::ls) = ((x,y),l)::enumerate2d_row(x,y+1,ls)
  in
    (* x, y - starting indices
    Takes 2d array, returns 1d array of tuple (row,col)*(data)
    Example: enumerate2d([["a","b","c"],["d","e","f"]]) =
    [((0,0),"a"),((0,1),"b"),((0,2),"c"),((1,0),"d"),((1,1),"e"),((1,2),"f")] *)
    fun enumerate2d(_, _, []) = []
    | enumerate2d(x,y,row::rows) = enumerate2d_row(x, 0, row) @ (enumerate2d(x+1,y,rows))
  end;

  (* Gets tuple of coords (x,y) and room (l,t,r,b) and converts it to node
  l t r b = left top right bottom
  coords_room_to_node((4,5), (1,0,0,1));
  val it = ((4,5),[(4,4),(5,5)]) : (int * int) * (int * int) list   *)
  fun coords_room_to_node((x,y), (l,t,r,b)) = ((x,y),
   (if l=1 then [(x, y-1)] else []) @
   (if t=1 then [(x-1, y)] else []) @
   (if r=1 then [(x, y+1)] else []) @
   (if b=1 then [(x+1, y)] else []));

   (* Given width and height, checks if x and y are in width * height bounds *)
  fun test_bounds width height x y = x>=0 andalso y>=0 andalso x<width andalso y<height;

  (* valid - function test_bounds with width height in closure already
  neighbors - list of coords (x:int * y:int) of neighbors
  val v = test_bounds 3 3; (* 3*3 board *)
  val v = fn : int -> int -> bool
  num_ways_out(v, [(0,~1),(1,0)]); (* one of neighbors is outside - has way out *)
  val it = 1 : int
  - num_ways_out(v, [(0,2),(1,0)]); (* all neighbors inside - no way out *)
  val it = 0 : int  *)
  fun num_ways_out(valid, neighbors) = length(List.filter (fn (x,y) => not (valid x y)) neighbors);

  (* As node consists of tuple (coords)*(neighbors coords list), this function
     returns neighbors coords list *)
  fun node_neighbors(_, x) = x;

  (* Given associated list and a key returns value of that key, or empty list if
     key doesn't exist.
  val l = [((0,0),[(~1,0),(0,1)]),((0,1),[(0,0),(1,1)]),((0,2),[(0,3)]),((1,0),[])]
  assoc(l, (0,1)) => [(0,0),(1,1)]  *)
  fun assoc([], _) = []
  | assoc((x,y)::pairs, k) = if x=k then y else assoc(pairs,k);

  (* Remove item from list *)
  fun remove(list, x) = List.filter (fn y => y<>x) list;

  (* Given associated list and list of keys returns list of pairs with those keys
  1st - associated list, i.e. list of pairs. 2-nd - list of keys to look for
  Returns pairs of (key, value), not only values
  val l = [((0,0),[(~1,0),(0,1)]),((0,1),[(0,0),(1,1)]),((0,2),[(0,3)]),((1,0),[])]
  assoc_mult(l, [(0,0), (0,2)]) => [((0,0),[(~1,0),(0,1)]),((0,2),[(0,3)])]  *)
  fun assoc_mult([], _) = []
  | assoc_mult(_, []) = []
  | assoc_mult(pairs, k::ks) = let
      val k_values = assoc(pairs,k)
    in
      if null(k_values) then assoc_mult(pairs, ks) else (k, k_values)::assoc_mult(pairs, ks)
    end;

  (* difference of lists, works exactly as difference of sets, but slower *)
  infix \
   fun [] \ _ = []
   | lst \ [] = lst
   | lst \ (x::xs) = remove(lst,x) \ xs;

  (* Gets a maze and returns 1d list of (x,y)*(list of neighbors' coords)
  build_graph [[(0,1,1,0),(1,0,0,1),(0,0,1,0)],[(0,0,0,0),(0,1,1,1),(1,0,0,1)],
  [(0,0,0,0),(0,1,0,1),(0,1,1,1)]];
  val it = [((0,0),[(~1,0),(0,1)]),((0,1),[(0,0),(1,1)]),((0,2),[(0,3)]),((1,0),[]),
     ((1,1),[(0,1),(1,2),(2,1)]),((1,2),[(1,1),(2,2)]),((2,0),[]),
     ((2,1),[(1,1),(3,1)]),((2,2),[(1,2),(2,3),(3,2)])]  *)
  fun build_graph maze = (map coords_room_to_node)(enumerate2d(0,0,maze));

  (* Given node returns all it's neighbor nodes in maze *)
  fun neighbors_of(node, maze) = assoc_mult(maze, node_neighbors(node));

  (* Returns all neighbors of all nodes in node::nodes. *)
  fun neighbors_of_mult([], _) = []
  | neighbors_of_mult(_, []) = []
  | neighbors_of_mult(node::nodes, maze) = let
    val neighbors_of_node = neighbors_of(node, maze)
  in
   neighbors_of_node @ neighbors_of_mult(nodes, maze \ neighbors_of_node)
  end;

  local
    fun BFS(_, _, []) = false
    | BFS(dest, unvisited, open_list) =  let
        val next_open_list = neighbors_of_mult(open_list, unvisited)
    in
      List.exists (fn x => x=dest) open_list orelse BFS(dest, (unvisited \ next_open_list), next_open_list)
    end
  in
    (* Returns true if dest is reachable from source within given graph *)
    fun path_exists(src, dest, graph) = BFS(dest, remove(graph, src), [src])
  end;

  (* Given list of unique items returns list of all unique pairs (unlike cross-product).
  Example:  all_pairs_of [1,2,3,4] => [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] *)
  fun all_pairs_of [] = []
  | all_pairs_of(x::xs) = (map (fn y => (x, y)) xs) @ all_pairs_of(xs);

  (* Counts number of paths in graph between all pairs of nodes
  (source, destination) in src_dest_pairs list. Number of paths stored in acc. *)
  fun total_paths(_, [], acc) = acc
  | total_paths([], _, _) = 0
  | total_paths(graph, (src, dest)::src_dest_pairs, acc) = total_paths(graph, src_dest_pairs, acc
    + (if path_exists(src, dest, graph) then 1 else 0));

  fun one_room_paths pred exits = length(List.filter (fn x => num_ways_out(pred, node_neighbors(x))>1) exits);

  (* neighb_lr(x,y) checks whether room x is a valid neighbor of y from the left *)
  fun neighb_lr ((_,_,1,_),(0,_,_,_)) = false
  | neighb_lr ((_,_,0,_),(1,_,_,_)) = false
  | neighb_lr _ = true;

  (* neighb_tb(x,y) checks whether room x is a valid neighbor of y from the top *)
  fun neighb_tb ((_,_,_,1),(_,0,_,_)) = false
  | neighb_tb ((_,_,_,0),(_,1,_,_)) = false
  | neighb_tb _ = true;

  local
    fun row_is_valid(_, []) = true
    | row_is_valid (_, [x]) = true
    | row_is_valid(neighb_test, x1::x2::xs) = neighb_test(x1,x2) andalso row_is_valid(neighb_test, x2::xs)
  in
    (* neighb_test is a function receiveing two rooms and returning true if those rooms are valid neighbors *)
    fun rows_valid neighb_test maze = List.all (fn x => row_is_valid(neighb_test, x)) maze
  end;

in
  fun are_nighbours(a,b) = neighb_lr(a,b) orelse neighb_lr(b,a) orelse neighb_tb(a,b) orelse neighb_tb(b,a);

  fun is_valid_maze maze = rows_valid neighb_lr maze andalso rows_valid neighb_tb (transp(maze));

  local
    fun check_paths_num check maze = let
      val graph = build_graph maze
      val pred = test_bounds (length maze) (length(hd maze))
      val exits = List.filter (fn x => num_ways_out(pred, node_neighbors(x))>0) graph
    in
      is_valid_maze maze andalso check(total_paths(graph, all_pairs_of exits, 0) + (one_room_paths pred exits))
    end
  in
    fun exit_maze maze = check_paths_num (fn x => x>0) maze
    fun one_path_maze maze = check_paths_num (fn x => x=1) maze
  end;

end;
