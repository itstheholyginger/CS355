(*
    Programmer: Amariah Del Mar
    Date: 2/15/18
    Class: CptS 355
    Assignment: HW2
 *)

(* use "/Users/AmariahDelMar/Documents/Homework/CptS355/HW2.sml"; *)

fun countInList [] v = 0
|  countInList (x::xs) v = (1 * (if x = v then 1 else 0)) + (countInList xs v);


(* zipTail function takes two lists, pairs up the corresponding elements from two lists,
 and returns a merged list of tuples *)
fun zipTail L1 L2 =
  let
      fun reverse L =
        let
            fun rev [] L = L
              |   rev (x::rest) L = rev rest (x::L)
        in
            rev L []
        end;
      fun zip [] [] s = s
      |   zip [] r s = s
      |   zip q [] s = s
      |   zip (x::xs) (y::ys) z = zip xs ys ((x,y)::z)
      val V = zip L1 L2 []
  in
      reverse V
  end;



  fun fold f base [] = base
  |   fold f base (x::rest) = f x (fold f base rest);


(* Helper function: uses function f on each element of list *)
fun map f [] = []
 | map f (x::rest) = (f x) :: (map f rest);

 (* histogram function takes a list as input and returns a list of tuples
  where the first elements in the tuples are the unique elements from the
  input list and the second elements are the number of
  occurrences of those elements in the tuple.  *)
fun histogram L =
  let
      (* remove helper function: removes a all occurances of specific element in list *)
      fun remove(value, []) = []
      |   remove(value, x::rest) = if (value = x) then remove(x, rest)
                                   else x::(remove(value, rest))

      (* removes all duplicates of elements in a list *)
      fun removeDuplicates [] = []
      |   removeDuplicates (x::rest) =
          x::(removeDuplicates (remove(x, rest)))
  in
      removeDuplicates (zipTail L (map (countInList L) L))
  end;


(* Function deepSum is given a list of int lists and it returns the sum of all numbers
 in all sublists of the input list. *)
fun deepSum L =
  let
      fun sumList L = fold (fn x => fn y => x+y) 0 L
  in
      sumList (map sumList L)
  end;

(* Function deepSumOption is given a list of int option lists and it returns
 the sum of all int option values in all sublists of the input list. *)
fun deepSumOption L =
  let
      fun sumOpt (NONE) (NONE) = NONE
      |   sumOpt (SOME(x)) (NONE) = SOME(x)
      |   sumOpt (NONE) (SOME(y)) = SOME(y)
      |   sumOpt (SOME(x)) (SOME(y)) = SOME(x+y)
      fun sumList L = (fold sumOpt NONE L)
  in
      sumList (map sumList L)
  end;

(* takes a list of tuples as input and produces a list including two lists as output. *)
fun unzip L =
  let
      fun first (x,y) = x;
      fun second (x,y) = y;
  in
      [map first L, map second L]
  end;

(* Custom data type of string and int *)
datatype either = ImAString of string | ImAnInt of int;

(* Custom data type of tree *)
datatype eitherTree = Empty
        |             eLEAF of either
        |             eINTERIOR of (either*eitherTree*eitherTree);


(* binary trees containing values of type either where
  data may be held at both interior and leaf nodes of the tree *)
fun eitherSearch (eLEAF(ImAnInt v)) x = (x=v)
|   eitherSearch (eLEAF(ImAString v)) x = false
|   eitherSearch (eINTERIOR((ImAnInt v), left, right)) x =
    if (x=v) then true
    else (eitherSearch left x) orelse (eitherSearch right x);


datatype 'a Tree = LEAF of 'a | NODE of ('a Tree) * ('a Tree);
datatype 'a myTree = myLEAF of 'a | myNODE of 'a*'a*('a myTree)*('a myTree);

(* Return minimum of a binary tree *)
fun findMin (LEAF(v)) = v
|   findMin (NODE(left, right)) = if findMin (left) < findMin (right) then findMin (left)
                                  else findMin (right);


(* Return maximum of a binary tree *)
fun findMax (LEAF(v)) = v
|   findMax (NODE(left, right)) = if findMax (left) > findMax (right) then findMax (left)
                                  else findMax (right);

  (* Function that returns tree in which each node includes the min and max values that
  appear in the leaves of the subtree rooted at that node. *)
fun minmaxTree (LEAF(v)) = myLEAF(v)
|   minmaxTree (NODE (left, right)) = myNODE((findMin (NODE(left, right))), (findMax (NODE(left, right))), minmaxTree left, minmaxTree right);


(* ----TEST FUNCTIONS---- *)

(* countInList test functions *)
fun countInListTest() =
  let
      val T1 = (countInList [5, 3, 77, 3, 8, 1, 2, 2, 5, 6] 3 = 2)
      val T2 = (countInList ["hello", "I", "am", "tired"] "am" = 1)
      val T3 = (countInList [[1, 3], [5, 2], [1, 1], [1, 1], []] [1, 1] = 2)
      val T4 = (countInList [] "wazzup" = 0)
  in
      print ("\n\n------------- \n countInListTest:\n" ^
      "test1: " ^ Bool.toString(T1) ^ "\n" ^
      "test2: " ^ Bool.toString(T2) ^ "\n" ^
      "test3: " ^ Bool.toString(T3) ^ "\n" ^
      "test4: " ^ Bool.toString(T4) ^ "\n")
  end;

(* zipTail test functions *)

fun zipTailTest() =
  let
      val T1 = (zipTail [1, 2, 3, 4] ["one", "two", "three"] = [(1, "one"), (2, "two"), (3, "three")])
      val T2 = (zipTail [3] [1, 4, 32] = [(3, 1)])
      val T3 = (zipTail [] [1, 2, 3] = [])
  in
      print ("\n\n------------- \n zipTailTest:\n" ^
      "test1: " ^ Bool.toString(T1) ^ "\n" ^
      "test2: " ^ Bool.toString(T2) ^ "\n" ^
      "test3: " ^ Bool.toString(T3) ^ "\n")
  end;
val _ = zipTailTest();

(* histogram test function *)
fun histogramTest() =
  let
      val T1 = (histogram [1, 2, 3, 4, 2, 3, 12, 2] = [(1, 1), (2, 3), (3, 2), (4, 1), (12, 1)])
      val T2 = (histogram [[1, 2], [1], [], [], [2], [2]] = [([1,2], 1), ([1], 1), ([], 2), ([2], 2)])
      val T3 = (histogram [] = []);
  in
      print ("\n\n------------- \n histogramTest:\n" ^
      "test1: " ^ Bool.toString(T1) ^ "\n" ^
      "test2: " ^ Bool.toString(T2) ^ "\n" ^
      "test3: " ^ Bool.toString(T3) ^ "\n")
  end;
val _ = histogramTest();

(* deepSum test function *)
fun deepSumTest() =
  let
      val T1 = (deepSum [[1, 1, 1], [23, 0], [9, 1]] = 36)
      val T2 = (deepSum [[~78, 10], [12, 8, 1, 1], [5, 3]] = ~38)
      val T3 = (deepSum [] = 0)
      val T4 = (deepSum [[]] = 0)
  in
      print ("\n\n------------- \n deepSumTest:\n" ^
      "test1: " ^ Bool.toString(T1) ^ "\n" ^
      "test2: " ^ Bool.toString(T2) ^ "\n" ^
      "test3: " ^ Bool.toString(T3) ^ "\n")
  end;
val _ = deepSumTest();

(* deepSumOption test functions *)

fun deepSumOptionTest() =
  let
      val T1 = (deepSumOption [[SOME(1), SOME(1), SOME(2)], [SOME(~1), SOME(5)], [SOME(3), NONE], [], [NONE]] = SOME(11))
      val T2 = (deepSumOption [[SOME(~78), SOME(10)], [SOME(12), SOME(8), SOME(1), SOME(1)], [SOME(5), SOME(3)]] = SOME(~38))
      val T3 = (deepSumOption [] = NONE)
      val T4 = (deepSumOption [[]] = NONE)
  in
      print ("\n\n------------- \n deepSumOptionTest:\n" ^
      "test1: " ^ Bool.toString(T1) ^ "\n" ^
      "test2: " ^ Bool.toString(T2) ^ "\n" ^
      "test3: " ^ Bool.toString(T3) ^ "\n")
  end;
val _ = deepSumOptionTest();
(* unzip test functions *)

fun unzipTest () =
  let
      val T1 = (unzip [(1,2), (2,3), (3,4)] = [[1,2,3], [2,3,4]])
      val T2 = (unzip  [("1","ab"),("5","bc"),("8","cd")] = [["1","5","8"],["ab","bc","cd"]])
      val T3 = (unzip [] = [])

  in
      print ("\n\n------------- \n unzipTest:\n" ^
          "test1: " ^ Bool.toString(T1) ^ "\n" ^
          "test2: " ^ Bool.toString(T2) ^ "\n" ^
          "test3: " ^ Bool.toString(T3) ^ "\n")
  end;

val _ = unzipTest();

(* eitherSearch test functions *)
fun eitherTest () =
  let
      val int1 = eLEAF(ImAnInt 1)
      val int2 = eLEAF(ImAnInt 2)
      val int3 = eLEAF(ImAnInt 3)
      val int4 = eLEAF(ImAnInt 4)
      val int5 = eLEAF(ImAnInt 5)
      val int6 = eLEAF(ImAnInt 6)
      val str1 = eLEAF(ImAString "ONE")
      val str2 = eLEAF(ImAString "TWO")
      val str3 = eLEAF(ImAString "THREE")
      val str4 = eLEAF(ImAString "FOUR")
      val str5 = eLEAF(ImAString "FIVE")
      val str6 = eLEAF(ImAString "SIX")
      val leaf1 = eINTERIOR((ImAnInt 54), str3, int4)
      val leaf2 = eINTERIOR((ImAnInt 32), int1, leaf1)
      val leaf3 = eINTERIOR((ImAnInt 6), leaf2, str6)
      val leaf4 = eINTERIOR((ImAnInt 78), leaf3, int2)
      val leaf5 = eINTERIOR((ImAnInt 12), str4, leaf4)
      val leaf6 = eINTERIOR((ImAnInt 21), leaf5, int6)

      val T1 = (eitherSearch leaf6 32 = true)
      val T2 = (eitherSearch leaf6 12 = true)
  in
      print ("\n\n------------- \n eitherTest:\n" ^
            "test1: " ^ Bool.toString(T1) ^ "\n" ^
            "test2: " ^ Bool.toString(T2) ^ "\n")
  end;
val _ = eitherTest();

(* findMin test functions *)
fun findMinTest() =
  let
      val n1 = (NODE(NODE(LEAF(2),NODE(LEAF(7),LEAF(10))),LEAF(4)))
      val n2 = (NODE(NODE(NODE(LEAF(0),LEAF(~11)),LEAF(6)),NODE(LEAF(9),LEAF(10))))
      val n3 = (LEAF(0))

      val T1 = (findMin n1 = 2)
      val T2 = (findMin n2 = ~11)
      val T3 = (findMin n3 = 0)
  in
      print ("\n\n------------- \n findMinTest:\n" ^
            "test1: " ^ Bool.toString(T1) ^ "\n" ^
            "test2: " ^ Bool.toString(T2) ^ "\n" ^
            "test3: " ^ Bool.toString(T3) ^ "\n")
  end;
  val _ = findMinTest();

(* findMax test functions *)
fun findMaxTest() =
  let
      val p1 = (NODE(NODE(LEAF(2),NODE(LEAF(7),LEAF(10))),LEAF(4)))
      val p2 = (NODE(NODE(NODE(LEAF(0),LEAF(~11)),LEAF(6)),NODE(LEAF(9),LEAF(10))))
      val p3 = (LEAF(0))

      val T1 = (findMax p1 = 10)
      val T2 = (findMax p2 = 10)
      val T3 = (findMax p3 = 0)
  in
      print ("\n\n------------- \n findMaxTest:\n" ^
            "test1: " ^ Bool.toString(T1) ^ "\n" ^
            "test2: " ^ Bool.toString(T2) ^ "\n" ^
            "test3: " ^ Bool.toString(T3) ^ "\n")
  end;
val _ = findMaxTest();

(* minmaxTree test functions *)
fun minmaxTreeTest() =
  let
      val x = (NODE(NODE(NODE(LEAF(1),LEAF(8)),LEAF(29)),NODE(LEAF(2),LEAF(5))))
      val y = (NODE(NODE(NODE(LEAF(11),LEAF(29)),LEAF(70)),NODE(LEAF(31),LEAF(98))))

      val T1 = (minmaxTree x = (myNODE(1,29,myNODE(1,29,myNODE(1,8,myLEAF(1),
                                            myLEAF(8)),myLEAF(29)),
                                            myNODE(2,5,myLEAF(2),myLEAF(5)))))

      val T2 = (minmaxTree y = (myNODE(11,98,myNODE(11,70,myNODE(11,29,myLEAF(11),
                                            myLEAF(29)),myLEAF(70)),
                                            myNODE(31,98,myLEAF(31),myLEAF(98)))))
  in
      print ("\n\n------------- \n minmaxTreeTest:\n" ^
            "test1: " ^ Bool.toString(T1) ^ "\n" ^
            "test2: " ^ Bool.toString(T2) ^ "\n")
  end;
val _ = minmaxTreeTest();
