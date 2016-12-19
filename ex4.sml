(* Lev Pechersky 333815546 levpechersky@campus.technion.ac.il Kfir Taizi 208044610 kfirtaizi@campus.technion.ac.il *)

datatype color = BLACK | RED;
datatype 'a RBTree = Nil
|  Br of (int * 'a * color) * 'a RBTree * 'a RBTree;
datatype Balance = RR | LR | LL | RL;
exception NotFound;

fun cmp a b = if a<b then LESS else (if a>b then GREATER else EQUAL);

fun max (a, b) = if a>b then a else b;

fun root_is_black Nil = true
| root_is_black (Br((_,_,c), _, _)) = c=BLACK;

fun all_leaves_black Nil = true
| all_leaves_black (Br((_,_,RED), Nil, Nil)) = false
| all_leaves_black (Br((_,_,_), l, r)) = all_leaves_black l andalso all_leaves_black r;

fun red's_children_black Nil = true
| red's_children_black (Br((_,_,RED), l, r)) =
    root_is_black l andalso root_is_black r andalso
    red's_children_black l andalso red's_children_black r
| red's_children_black (Br((_,_,BLACK), l, r)) =
    red's_children_black l andalso red's_children_black r;

fun black_height Nil = 0
| black_height (Br((_,_,BLACK), l, r)) = 1 + max(black_height l, black_height r)
| black_height (Br((_,_,RED), l, r)) = max(black_height l, black_height r);

fun black_height_equal Nil = true
| black_height_equal (Br(_, l, r)) = black_height l = black_height r;

fun is_rbtree Nil = true
| is_rbtree tree = root_is_black tree andalso
    all_leaves_black tree andalso
    red's_children_black tree andalso
    black_height_equal tree;

fun size Nil = 0
| size (Br(_, l, r)) = 1 + size l + size r;

fun blacks Nil = 0
| blacks (Br((_,_,BLACK), l, r)) = 1 + blacks l + blacks r
| blacks (Br((_,_,RED), l, r)) = blacks l + blacks r;

fun postorder Nil = []
| postorder (Br((_,v,_),l,r)) = (postorder l) @ (postorder r) @ [v];

fun get (Nil, _) = raise NotFound
| get (Br((k,v,c),l,r), key) = case cmp key k of
    EQUAL => (v,c) | GREATER => get (r, key) | LESS => get (l, key)


(*
Serialization:
We build a list, containing full BFS traversal of tree as it were full
binary tree. Then we encode each non-Nil node in following format:
Tag, key, value (exact format is described below).
Each sequence of Nil nodes is compressed to pair of tag and number of
consequtive Nil nodes.
Tag is a single char representing a bit array (exact format is described below).
Every other value may be encoded in 1-4 characters. Tag contains information
about encoded values' sizes.

Serialization format (per characters):
[0]           tag
[1...length]  (length depends on tag) for regular nodes: node key and value
                               for Nil nodes: number of consequtive Nil nodes
[length+1]    next tag
[length+2...] next node (if any)

Tag (per bits):
[0-1] 0: empty (in this case it's 1st and last char in stream,
         and all other data is irrelevant)
      1: BLACK
      2: RED
      3: Nil
[2-3]: ((number of bytes)-1) for key / for number of consequtive Nil nodes
[4-5]: ((number of bytes)-1) for value, e.g 01(binary) means that value
       takes 2 bytes
[6]: key sign (1 is negative)
[7]: value sign (1 is negative)
*)
(*local*)
  exception LSerialError;
  datatype tag = EMPTY | NIL_TAG | NODE_TAG;

  fun substr(s, from, to) = if from>to then ""
      else str(String.sub(s,from)) ^ substr(s, from+1, to);

  (* Checks how many bytes needed to store x, with no respect to sign *)
  fun bytes_needed x = if (abs x)<256 then 1 else 1 + bytes_needed (x div 256);

  fun tag_type c = case (ord c) mod 4 of
      0 => EMPTY
    | 3 => NIL_TAG
    | _ => NODE_TAG;

  (* Convert number of bytes to tag format *)
  fun bytes_num_tag n = if n>=0 andalso n<=4 then n-1 else raise LSerialError;

  (* Gets a tag - single char containing all node metadata *)
  fun unpack_tag c = let
    val tag = ord c
    val color = case tag mod 4 of 1 => BLACK | 2 => RED | _ => raise LSerialError
    val k_bytes = ((tag div 4) mod 4) + 1
    val v_bytes = ((tag div 16) mod 4) + 1
    val k_sign = if ((tag div 64) mod 2)=0 then 1 else ~1
    val v_sign = if ((tag div 128) mod 2)=0 then 1 else ~1
  in
    (color, k_bytes, v_bytes, k_sign, v_sign)
  end;

  fun pack_tag (color, k_bytes, v_bytes, k_sign, v_sign) = let
    val t_color = case color of BLACK => 1 | RED => 2
    val t_k_bytes = bytes_num_tag k_bytes
    val t_v_bytes = bytes_num_tag v_bytes
    val t_k_sign = if k_sign<0 then 1 else 0
    val t_v_sign = if v_sign<0 then 1 else 0
  in
    chr(t_color + 4*t_k_bytes + 16*t_v_bytes + 64*t_k_sign + 128*t_v_sign)
  end;

  (* Gets a tag - single char containing all node metadata
     Returns number of bytes where number of consequtive Nil nodes stored *)
  fun unpack_nil_tag c = (((ord c) div 16) mod 4) + 1;

  (* As explained before, 3 is tag of Nil node *)
  fun pack_nil_tag len_bytes = chr(3 + (bytes_num_tag len_bytes)*4);

  fun fill_list(_, 0) = []
  | fill_list(init, n) = init :: fill_list(init, n-1);

  (* Works with positive values only, sign lost when zipping *)
  local
    fun zip 0 _ s = s
    | zip bytes x s = zip (bytes-1) (x div 256) (str(chr(x mod 256))^s);
  in
    fun zip_int bytes x = zip bytes (abs x) "";
  end

  (* Works with positive values only *)
  local
    fun unzip index s acc = if index>=String.size(s) then acc else
      unzip (index+1) s (acc*256 + ord(String.sub(s, index)));
  in
    fun unzip_int bytes s = if bytes<>String.size(s) then
      raise LSerialError else unzip 0 s 0;
  end

  local
    fun deeper_level [] = []
    | deeper_level (Nil::trees) = [Nil, Nil] @ deeper_level trees
    | deeper_level ((Br(node,l,r))::trees) = [l, r] @ deeper_level trees

    fun c_bfs_aux trees acc = let
      val next = deeper_level trees
    in
      if List.all (fn x => x=Nil) next then acc else (c_bfs_aux next (acc @ next))
    end
  in
    fun complete_bfs tree = c_bfs_aux [tree] [tree]
  end;

  fun tree2node Nil = NONE
  | tree2node (Br(node,_,_)) = SOME node;

  fun pack_node (k,v,c) = let
    val k_bytes = bytes_needed k
    val v_bytes = bytes_needed v
    val tag = pack_tag (c, k_bytes, v_bytes, k, v)
  in
    (str tag) ^ (zip_int k_bytes k) ^ (zip_int v_bytes v)
  end;

  fun unpack_node stream = let
    val (c, k_bytes, v_bytes, k_sign, v_sign) = unpack_tag (String.sub(stream, 0))
    val k = unzip_int k_bytes (substr(stream, 1, k_bytes))
    val v = unzip_int v_bytes (substr(stream, k_bytes+1, k_bytes + v_bytes))
  in
    (k*k_sign, v*v_sign, c)
  end;

  fun unpack_nils stream = let
    val bytes = unpack_nil_tag (String.sub(stream, 0))
    val nils_count = unzip_int bytes (substr(stream, 1, bytes))
  in
    fill_list(NONE, nils_count)
  end;

  local
    (* Last Nil nodes are dropped *)
    fun pack_all ([], acc, _) = acc
    | pack_all (NONE::nodes, acc, nil_count) = pack_all(nodes, acc, nil_count+1)
    | pack_all (SOME(node)::nodes, acc, 0) = pack_all(nodes, acc@[pack_node node], 0)
    | pack_all (SOME(node)::nodes, acc, nil_count) = let
      val bytes = bytes_needed nil_count
      val consequtive_nils = str(pack_nil_tag nil_count) ^ (zip_int bytes nil_count)
    in
      pack_all(nodes, acc@[consequtive_nils]@[pack_node node], 0)
    end
  in
    fun encode Nil = str(chr 0)
    | encode tree = foldl op^ "" (pack_all(map tree2node (complete_bfs tree), [], 0))
  end;

    fun unpack_all("", acc) = acc
    | unpack_all(stream, acc) = let
      val tag = String.sub(stream, 0)
    in
      case tag_type tag of
        NODE_TAG => let
          val (_, k_bytes, v_bytes, _, _) = unpack_tag tag
          val stream_tail = substr(stream, 1+k_bytes+v_bytes, String.size(stream)-1)
        in
          unpack_all(stream_tail , SOME(unpack_node stream)::acc)
        end
      | NIL_TAG => let
          val bytes = unpack_nil_tag tag
          val stream_tail = substr(stream, 1+bytes, String.size(stream)-1)
        in
          unpack_all (stream_tail, (unpack_nils stream) @ acc)
        end
      | EMPTY => [NONE]
    end;

    fun break_to_levels bfs_list 

(*in*)


(*end*)

(*
use "ex4.sml";
use "ex4_test.sml";
map tree2node (complete_bfs rb_test_int);
encode rb_test_int;
decode it;
*)


(*OLD

fun encode Nil = "\^@"
| encode tree = let
  val k_bytes = bytes_needed (abs_max_key tree)
  val v_bytes = bytes_needed (abs_max_value tree)
  val zip_n = zip_node k_bytes v_bytes
  val stream = map zip_n (map tree2node (complete_bfs tree))
  (* TODO compress nulls, drop last nulls *)
in
  str(chr k_bytes) ^ str(chr v_bytes) ^ (foldl op^ "" stream)
end;

fun decode "\^@" = Nil
| decode s = let
  val k_bytes = ord(String.sub(s,0))
  val v_bytes = ord(String.sub(s,1))
  val unzip_n = unzip_node k_bytes v_bytes
  val first_node_of = take_1st_node k_bytes v_bytes
  val stream = substr(s, 2, String.size(s)-1)
in
  Br((unzip_n (first_node_of stream)), Nil, Nil) (* TODO now decodes only root*)
end;


fun zip_node node = case node of NONE => "N" |
  SOME((k,v,c)) => let
  val tag = pack_tag(k div (abs k), v div (abs v), c)
in
  str(tag) ^ zip_int k_bytes (abs k) ^ zip_int v_bytes (abs v)
end;

fun unzip_node k_bytes v_bytes s = let
  val (k_sign, v_sign, color) = unpack_tag(String.sub(s, 0))
  val key = (unzip_int k_bytes (substr(s, 1, k_bytes))) * k_sign
  val value = (unzip_int v_bytes (substr(s, k_bytes+1, k_bytes+v_bytes))) * v_sign
in
  (key, value, color)
end;

fun zip_nils [] = ""
| zip_nils lst = let
  val len = length lst
  val bytes = bytes_needed len
in
  str(pack_nil_tag bytes) ^ (zip_int bytes len)
end;

fun unzip_nils "" = []
| unzip_nils s = let
  val bytes = unpack_nil_tag(String.sub(s, 0))
  val len = unzip_int bytes (substr(s, 1, bytes))
in
  fill_list(Nil, len)
end;

fun abs_max_key Nil = 0
| abs_max_key (Br((k,_,_),l,r)) = max(abs k, max(abs_max_key l, abs_max_key r));

(* Only for int RB-trees *)
fun abs_max_value Nil = 0
| abs_max_value (Br((_,v,_),l,r)) = max(abs v, max(abs_max_value l, abs_max_value r));

(* Credit to Snir Cohen. Returns true if (f arg) throws exception exp *)
fun test f arg exp =
let
  val res = SOME (f arg) handle exp => NONE
in
  case res of
       NONE => true
     | SOME _ => false
end;

fun is_node_tag c = not (test unpack_tag c LSerialError);
fun is_nil_tag c = not (test unpack_nil_tag c LSerialError);

(* TODO handle all nil cases *)
fun take_1st_node k_bytes v_bytes stream = let
    val c = String.sub(stream, 0)
  in
    if is_node_tag c then substr(stream, 0, k_bytes + v_bytes) else
    (if is_nil_tag c then substr(stream, 0, unpack_nil_tag(c)) else raise LSerialError)
  end;

*)
