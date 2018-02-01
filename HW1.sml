(*  Homework Assignment #1
    Programmer: Amariah Del Mar
    Date: 1/29/18
    Course: CptS 355 *)

(* Problems *)

(* 1. inList - 10% *)

(* Will find if element is in list *)
fun inList (value, []) = false
  | inList(value, (x::rest)) = (value=x) orelse inList(value, rest);

(*
    b. The reason the function type is (''a * ''a list) -> bool instead of
       ('a * 'a list) -> bool is because ''a Comparable while 'a is not.  *)


(* 2. removeDuplicates - 10% *)

(* check to see if first value is in rest of list. If not, add it to new list *)

(* remove helper function: removes a all occurances of specific element in list *)
fun remove(value, []) = []
  | remove(value, x::rest) = if (value = x) then remove(x, rest)
                             else x::(remove(value, rest));

(* removes all duplicates of elements in a list *)
fun removeDuplicates [] = []
  | removeDuplicates (x::rest) =
    x::(removeDuplicates (remove(x, rest)));


(* 3. listIntersect - 10% *)

(* outputs a list of elements that are in both the input lists *)
fun listIntersect([], y) = []                          (* vvv ignores if duplicates occur later in the list *)
  | listIntersect(x::rest, y) = if inList(x, y) andalso inList(x, rest) = false then x::(listIntersect(rest, y))
                                else listIntersect(rest, y);


(* 4. range - 15% *)

(* outputs a list of all elements between x and z (not including z) by a step of y *)
fun range x y z =
  if (y = 0) then []
  else if (x = z) then []  (* BASE: if min = max then empty list *)
  else if ((x > z) andalso (y > 0)) then []    (* BASE: if x > z and z will never be reached because the step is increasing *)
  else if ((x < z) andalso (y < 0)) then []    (* BASE: if x < z and z will never be reached because the step is decreasing *)
  else x::(range (x+y) y z);


(* 5. numbersToSum - 15% *)

(* Helper function: adds elements of list together *)
fun sumList [] = 0
  | sumList (x::rest) = x + (sumList rest);

(* Helper function: outputs length of list *)
fun length [] = 0
  | length (x::rest) = 1 + (length rest);

(* Helper function: removes last element of a list *)
fun delLastElem [] = []
  | delLastElem [x] = []
  | delLastElem (x::rest) = if length(rest) = 0 then delLastElem [x]
                            else x::(delLastElem rest);

(* Finds the list of number in list whose sum < "sum" *)

fun numbersToSum sum [] = []
  | numbersToSum sum (L) =
    if (sumList (L)) < sum then (L)
    else numbersToSum sum (delLastElem L);


(* 6. replace - 15% *)

(*
  My first function that used List library functions.
  Was scared that Sam would take points away. Didn't use.

fun replace n v [] = []
  | replace n v (x::rest) =
  if n > (length (x::rest)) then (x::rest)
  else if n < 0 then (x::rest)
  else if n = 0 then (v::rest)
  else (List.take((x::rest), n)) @ (v::(List.drop((x::rest), (n+1)))); *)


(* Replaces element at index n of list with element v *)
fun replace n v [] = []
  | replace 0 v (x::rest) = (v::rest)   (* BASE: if replaced element is first in list *)
  | replace n v (x::rest) = if n < 0 then (x::rest)   (* BASE: if index < 0 then return nil *)
                            else x::(replace (n-1) v rest);

(* 7. groupNleft and groupNright - 25% *)

(* Helper function: appends two lists together *)
fun append ([], L) = L
  | append ((x::rest), L) = x::(append(rest, L));

(* Helper function: reverses list *)
fun reverse [] = []
  | reverse (x::rest) = append(reverse(rest), x::[]);

(* Helper function: turns list of elements into lists of lists of elements of length n *)
fun listSplitter [] temp n = [temp]
  | listSplitter (x::rest) temp n = if length(temp) = n then temp::(listSplitter rest [x] n)
                                  else (listSplitter rest (x::temp) n);

(* Helper function: uses function f on each element of list *)
fun map f [] = []
  | map f (x::rest) = (f x) :: (map f rest);


(* Groups elements in a list to lists of length n in the lists with the leftovers on the right *)
fun groupNright n [] = [[]]
  | groupNright n L = if n > 0 then (map reverse (listSplitter L [] n))
                      else [[]];


(* Groups elements in a list to lists of length n in the lists with the leftovers on the left *)
fun groupNleft n [] = [[]]
  | groupNleft n (L) = if n > 0 then reverse(listSplitter (reverse L) [] n)
                       else [[]];


(* TEST CASES *)

fun mytest_inList (n, L, output:bool) =
  if inList(n, L) = output then true else false;

fun mytest_removeDuplicates (L, output) =
  if removeDuplicates L = output then true else false;

fun mytest_listIntersect(L, R, output) =
  if listIntersect(L, R) = output then true else false;

fun mytest_range(min, step, max, output) =
  if (range min step max) = output then true else false;

fun mytest_numToSum(sum, L, output) =
  if (numbersToSum sum L) = output then true else false;

fun mytest_replace(n, v, L, output) =
  if (replace n v L) = output then true else false;

fun mytest_groupNleft(n, L, output) =
  if (groupNleft n L) = output then true else false;

fun mytest_groupNright(n, L, output) =
  if (groupNright n L) = output then true else false;


val inList_test1 = mytest_inList(2, [], false);
val inList_test2 = mytest_inList([4, 5], [[2, 3, 4], [1], [4, 5], []], true);
val inList_test3 = mytest_inList("help", ["please", "help", "I", "need", "sleep"], true);

val removeDuplicates_test1 = mytest_removeDuplicates([2, 2, 2, 2, 2, 2, 2, 2, 2, 2], [2]);
val removeDuplicates_test2 = mytest_removeDuplicates([], []);
val removeDuplicates_test3 = mytest_removeDuplicates(["m", "y", "n", "a", "m", "e", "i", "s", "a", "m", "a", "r", "i", "a", "h"], ["m","y","n","a","e","i","s","r","h"]);
val removeDuplicates_test4= mytest_removeDuplicates([[2], [32], [1], [2], [2], [6], [6]], [[2], [32], [1], [6]]);


val listIntersect_test1 = mytest_listIntersect([1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1], [1]);
val listIntersect_test2 = mytest_listIntersect([], [4444], []);
val listIntersect_test3 = mytest_listIntersect([[2], [1], []], [[1], [3]], [[1]]);
val listIntersect_test4 = mytest_listIntersect(["I", "say", "hi"], ["you", "say", "hi"], ["say", "hi"]);

val range_test1 = mytest_range(2, ~3, 2, []);
val range_test2 = mytest_range(5, ~2, 0, [5, 3, 1]);
val range_test3 = mytest_range(1, 0, 8, []);
val range_test4 = mytest_range(10, 5, 2, []);
val range_test5 = mytest_range(1, ~1, 8, []);

val numToSum_test1 = mytest_numToSum(0, [100, 4], []);
val numToSum_test2 = mytest_numToSum(3, [3, 5], []);
val numToSum_test3 = mytest_numToSum(1000, [39, 23, 0, 1], [39, 23, 0, 1]);

val replace_test1 = mytest_replace(0, "a", ["m", "o", "m"], ["a", "o", "m"]);
val replace_test2 = mytest_replace(4, ~485, [3, 5, 1, 3, 2, 4, 3], [3, 5, 1, 3, ~485, 4, 3]);
val replace_test3 = mytest_replace(3, 0, [0, 1, 2, 3], [0, 1, 2, 0]);
val replace_test4 = mytest_replace(~3, 0, [0, 1, 2, 3], [0, 1, 2, 3]);
val replace_test5 = mytest_replace(9, 0, [0, 1, 2, 3], [0, 1, 2, 3]);

val groupNleft_test1 = mytest_groupNleft(30, [1, 2, 3], [[1, 2, 3]]);
val groupNleft_test2 = mytest_groupNleft(~4, ["a", "b", "c"], [[]]);
val groupNleft_test3 = mytest_groupNleft(2, [1, 2, 3, 4, 5], [[1], [2, 3], [4, 5]]);

val groupNright_test1 = mytest_groupNright(3, [1, 2, 3, 4, 5], [[1, 2, 3], [4, 5]]);
val groupNright_test2 = mytest_groupNright(~4, ["a", "b", "c"], [[]]);
val groupNright_test3 = mytest_groupNright(100, ["do", "re", "me"], [["do", "re", "me"]]);
