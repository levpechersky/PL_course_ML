local
  val l_6  = Br(( 6, "6", RED), Nil, Nil);
  val l_22 = Br((22,"22", RED), Nil, Nil);
  val l_27 = Br((27,"27", RED), Nil, Nil);
  val l_15 = Br((15,"15", BLACK), Nil, Nil);
  val l_11 = Br((11,"11", BLACK), Nil, Nil);
  val l_1  = Br(( 1, "1", BLACK), Nil, l_6);
  val l_25 = Br((25,"25", BLACK), l_22, l_27);
  val l_8  = Br(( 8, "8", RED), l_1, l_11);
  val l_17 = Br((17,"17", RED), l_15, l_25);
in
  val rb_test = Br((13,"13",BLACK), l_8, l_17)
end;

local
val l_6  = Br(( 6, ~ 6 *  6, RED), Nil, Nil);
val l_22 = Br((22, ~22 * 22, RED), Nil, Nil);
val l_27 = Br((27, ~27 * 27, RED), Nil, Nil);
val l_15 = Br((15, ~15 * 15, BLACK), Nil, Nil);
val l_11 = Br((11, ~11 * 11, BLACK), Nil, Nil);
val l_1  = Br(( 1, ~ 1 *  1, BLACK), Nil, l_6);
val l_25 = Br((25, ~25 * 25, BLACK), l_22, l_27);
val l_8  = Br(( 8, ~ 8 *  8, RED), l_1, l_11);
val l_17 = Br((17, ~17 * 17, RED), l_15, l_25);
in
  val rb_test_int = Br((13, ~13 * 13 ,BLACK), l_8, l_17)
end;



val _ = print "----------------------------------------------\n";
val _ = print "    Testing. Everything should return true    \n";
val _ = print "----------------------------------------------\n";

val _ = print "\nTests on tree from assignment (and an empty tree):\n";

val _ = print "\ntest of size:\n";
size Nil = 0;
size rb_test = 10;

val _ = print "\ntest of blacks:\n";
blacks Nil = 0;
blacks rb_test = 5;

val _ = print "\ntest of black_height :\n";
black_height Nil = 0;
black_height rb_test = 2;

val _ = print "\ntest of get:\n";
get (rb_test, 13) = ("13", BLACK);
get (rb_test, 1) = ("1", BLACK);
get (rb_test, 11) = ("11", BLACK);
get (rb_test, 15) = ("15", BLACK);
get (rb_test, 25) = ("25", BLACK);
get (rb_test, 6) = ("6", RED);
get (rb_test, 8) = ("8", RED);
get (rb_test, 17) = ("17", RED);
get (rb_test, 22) = ("22", RED);
get (rb_test, 27) = ("27", RED);

val _ = print "\ntest of postorder:\n";
postorder rb_test = ["6","1","11","8","15","22","27","25","17","13"];

val _ = print "\nDone.\n";
