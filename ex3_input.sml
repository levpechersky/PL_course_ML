(* Default tests *)

(* “PL” *)
common ["PL", "PL", "is", "fun", "PL"];

(* [1,2,3,4] *)
sequence[1,2,3,4,1,5,5];

(* [(3,2), (2,1), (7,2), (5, 2)] *)
thin [3,3,2,7,5,7,5];

(* true  *)
are_nighbours((0,1,1,0), (1,0,0,0));

(* false  *)
are_nighbours((0,0,1,0), (0,1,1,1));

(* true  *)
is_valid_maze [[(0,0,0,0), (0,0,0,0),(0,0,0,0)],[(0,0,0,0), (0,0,0,0),(0,0,0,0)],[(0,0,0,0), (0,0,0,0),(0,0,0,0)],[(0,0,0,0), (0,0,0,0),(0,0,0,0)]];

(* true  *)
is_valid_maze [[(0,1,1,0),(1,0,0,1),(0,0,1,0)],[(0,0,0,0),(0,1,1,0),(1,0,0,1)],[(0,0,0,0),(0,0,0,1),(0,1,0,0)]];

(* false  *)
is_valid_maze [[(0,1,1,0),(0,1,0,0),(0,0,1,0)],[(0,0,0,0),(0,1,1,0),(1,0,0,1)],[(0,0,0,0),(0,0,0,1),(0,0,0,0)]];

(* true  *)
exit_maze [[(0,1,1,0),(1,0,0,1),(0,0,1,0)],[(0,0,0,0),(0,1,1,0),(1,0,0,1)],[(0,0,0,0),(0,0,0,1),(0,1,1,1)]];

(* false *)
exit_maze [[(0,1,1,0),(1,0,0,0),(0,0,0,0)],[(0,0,0,1),(0,0,0,1),(0,0,0,0)],[(0,1,0,0),(0,1,0,0),(0,0,0,0)]];

(* true  *)
one_path_maze[[(0,1,1,0), (1,0,1,0),(1,0,1,0)],[(0,0,0,0), (0,0,0,0),(0,0,0,0)],[(0,0,0,0), (0,0,0,0),(0,0,0,0)],[(0,0,0,0), (0,0,0,0),(0,0,0,0)]];

(* false  *)
one_path_maze [[(0,1,1,0),(1,0,0,1),(0,0,1,0)],[(0,0,0,0),(0,1,1,1),(1,0,0,1)],[(0,0,0,0),(0,1,0,1),(0,1,1,1)]];



common ["forever alone"]; (* "forever alone" *)
common ["hodor","hodor","hodor","hodor","hodor","hodor","hodor"]; (* guess what? *)
common [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]; (* 5 *)

sequence [1,1,1,1,1,1,1]; (* [1] *)
sequence [1,3,4,6,7,8,10,11,12,13]; (* [10,11,12,13] *)
sequence [1,2,3,6,7,8]; (* ? *)
sequence []; (* [] *)

thin ["hodor","hodor","hodor","hodor","hodor","hodor","hodor"]; (* [("hodor", 7)] *)
thin []; (* [] *)
thin [1]; (* [(1,1)] *)



(* Super-legal neighbors (for any direction) *)
(* Return true for each pair *)
are_nighbours((0,0,0,0), (0,0,0,0));
are_nighbours((0,0,0,1), (0,1,0,0));
are_nighbours((0,0,1,0), (1,0,0,0));
are_nighbours((0,0,1,1), (1,1,0,0));
are_nighbours((0,1,0,0), (0,0,0,1));
are_nighbours((0,1,0,1), (0,1,0,1));
are_nighbours((0,1,1,0), (1,0,0,1));
are_nighbours((0,1,1,1), (1,1,0,1));
are_nighbours((1,0,0,0), (0,0,1,0));
are_nighbours((1,0,0,1), (0,1,1,0));
are_nighbours((1,0,1,0), (1,0,1,0));
are_nighbours((1,0,1,1), (1,1,1,0));
are_nighbours((1,1,0,0), (0,0,1,1));
are_nighbours((1,1,0,1), (0,1,1,1));
are_nighbours((1,1,1,0), (1,0,1,1));
are_nighbours((1,1,1,1), (1,1,1,1));

(* Only illegal neighbors (for any direction) *)
(* Return false  for each pair *)
are_nighbours((0,0,0,0),(1,1,1,1));
are_nighbours((0,0,0,1),(1,0,1,1));
are_nighbours((0,0,1,0),(0,1,1,1));
are_nighbours((0,0,1,1),(0,0,1,1));
are_nighbours((0,1,0,0),(1,1,1,0));
are_nighbours((0,1,0,1),(1,0,1,0));
are_nighbours((0,1,1,0),(0,1,1,0));
are_nighbours((0,1,1,1),(0,0,1,0));
are_nighbours((1,0,0,0),(1,1,0,1));
are_nighbours((1,0,0,1),(1,0,0,1));
are_nighbours((1,0,1,0),(0,1,0,1));
are_nighbours((1,0,1,1),(0,0,0,1));
are_nighbours((1,1,0,0),(1,1,0,0));
are_nighbours((1,1,0,1),(1,0,0,0));
are_nighbours((1,1,1,0),(0,1,0,0));
are_nighbours((1,1,1,1),(0,0,0,0));

(* All rooms. Room is 4 walls in order left top right bottom *)
(* If wall is 1 - can pass that way, otherwise - can't *)
val all_rooms = [(0,0,0,0),
(0,0,0,1),
(0,0,1,0),
(0,0,1,1),
(0,1,0,0),
(0,1,0,1),
(0,1,1,0),
(0,1,1,1),
(1,0,0,0),
(1,0,0,1),
(1,0,1,0),
(1,0,1,1),
(1,1,0,0),
(1,1,0,1),
(1,1,1,0),
(1,1,1,1) ];

(* Cross-product of 2 lists *)
fun cartesian (_,[]) = []
 | cartesian ([],_) = []
 | cartesian (x::xs, ys) = (map (fn y => (x,y)) ys) @ (cartesian (xs,ys));
val cross = cartesian(all_rooms, all_rooms);
(* Should be 240 - number of all possible neighbors *)
length(List.filter (fn x => are_nighbours x) cross);

(* Valid mazes *)
is_valid_maze [[(0,0,0,0)]];
is_valid_maze [[(1,1,1,1)]];
is_valid_maze [[(0,1,1,0),(1,1,0,0)],[(1,0,0,1),(0,0,1,1)],[(0,1,1,1),(1,1,0,0)],[(0,1,0,0),(0,0,0,1)]]; (* 4x2 *)
is_valid_maze [[(0,1,1,0),(1,0,1,0),(1,1,0,0)],[(1,0,1,0),(1,0,1,1),(1,0,1,0)]]; (* 2x3 *)
is_valid_maze [[(0,1,1,0),(1,1,0,0)],[(1,0,0,1),(0,0,1,1)]]; (* 4x1 *)
is_valid_maze [[(0,1,1,0),(1,0,1,0),(1,1,0,0)]]; (* 1x3 *)

(* Invalid mazes *)
is_valid_maze [[(0,1,1,0),(1,1,0,0)],[(1,0,0,1),(1,1,0,0)],[(0,1,1,1),(1,1,0,0)],[(0,1,0,0),(0,0,0,1)]]; (* 4x2 *)
is_valid_maze [[(0,1,1,0),(1,0,1,0),(1,1,0,0)],[(1,0,1,0),(1,0,1,1),(0,1,0,1)]]; (* 2x3 *)
is_valid_maze [[(0,1,1,0)],[(1,0,0,0)],[(1,0,0,1)],[(0,0,0,1)]]; (* 4x1 *)
is_valid_maze [[(0,1,0,0),(1,1,1,0),(0,1,0,0)]]; (* 1x3 *)

(* Mazes with exit *)
exit_maze [[(1,1,1,1)]];
(* All corners are 1-room in and out paths *)
exit_maze [[(1,1,0,0),(0,0,0,0),(0,1,1,0)],
    		   [(0,0,0,0),(0,0,0,0),(0,0,0,0)],
    		   [(1,0,0,1),(0,0,0,0),(0,0,1,1)]];

(* Snake like through all rooms *)
exit_maze [[(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)]];

print "Wait for it...\n";
(* If you want your computer to die - uncomment *)
(*
exit_maze [[(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)]];
*)

(* Simpler case. Took me 4-5 secons *)
exit_maze [[(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)],
    		   [(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1),(1,1,1,1)]];

(* Mazes with no exit *)
exit_maze [[(0,0,0,0)]];

(* Snake like through all rooms *)
exit_maze [[(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,1),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)],
    		   [(0,1,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,0,1)],
    		   [(0,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,0,1,0),(1,1,0,0)]];
(*                 ^ this zero ruins it *)