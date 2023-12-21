(***************************************************************************************************
Homework 1
==========
CSCI 599, Fall 2023: An Introduction to Programming Languages
Mukund Raghothaman
Due: 10pm PT on 18 September, 2023
***************************************************************************************************)

(***************************************************************************************************
- Your name: Aaron Dardik
- Your partner's name (if any): N/A
***************************************************************************************************)

(***************************************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in the cheatsheet and
   on the course website.
2. Please feel free to collaborate with a partner, but make sure to write your submission
   individually.
3. Rename this file to HW1-yourname.ml, where yourname is your name, before submitting it on
   Blackboard.
4. Make sure that we can cleanly import this file into the OCaml toplevel when I call
   #use "HW1-yourname.ml". Comment out any questions you were unable to solve / have left
   unanswered.
***************************************************************************************************)



(** Distribution of points:
    5 + (2 + 5) + (4 + 4) + (5 + 2 + 3) + (2 + 3) + 5 + 10 + (5 + 5) + (4 + 6 + 10) + (4 + 6) =
      90 points *)

(**************************************************************************************************)
(* Question 1: Starting Off (5 points)

   Predict the types of the following expressions and their results upon evaluation. Explain your
   reasoning for each expression in a sentence or two. *)

let _ = 5 + 3 - 2
(* - : 6 = int *)
(* Explanation: All three inputs (5, 3, 2) are ints, and the operation is integer addition. *)

let _ = 2. ** 0.5
(* - : approx sqrt(2), which is approx 1.41 = float *)
(* Explanation: Both 2. and 0.5 are floats due to the '.' *)

let _ = [1; 2; 3] @ [4; 5; 6]
(* - : int list = [1; 2; 3; 4; 5; 6] *)
(* Explanation: This is due to all the elements of the lists being integers.
   Changing the last element, for example, from 6 to 6. will cause an error. *)

let _ = ("abc" ^ "def").[3]
(* - : char = d *)
(* Explanation: Being zero-indexed finding the character with index
3 will give the fourth character, d.*)

let _ = fun x -> x + 1
(* - : int -> int = <fun> *)
(* Explanation: The above is a map, so it should be of the form something -> something.
  Due to both the + being integer addition and 1 being an int, it is a function from
  int to int. Had we had let _ = fun x -> x +. 1. we would have a function that is float to
  float. While if we had writen let _ = fun x -> x + 1. we would have an error.*)

let _ = (fun x -> x + 1) 5
(* - : int = 6 *)
(* Explanation: The result of applying the previous function to the int 5 gives an
integer whose value is 6.*)

let _ = (fun x y -> x + (3 - y)) 4
(* - : int -> int = <fun> *)
(* Explanation: The function of two variables, evaluated at one of them creates a function
  of one variable. As the + and - are integer +, integer - and the 3 is an int the function is int -> int. Changing the
  + to +. the - to -. and the 3 to 3. (as well as 4 to 4.) would make it a function float -> float.
  Changing only some of these would resul in an error.*)

let _ = Float.of_int
(* - : int -> float = <fun> *)
(* Explanation: This function takes an integer and converts it to the corresponding float. Ex.
   this function maps 3 to 3. (<-- that is a decimal sign) *)

let _ = List.map Float.of_int [1; 2; 3; 4; 5; 6]
(* - : float list = [1.;2.;3.;4.;5.;6.] *)
(* Explanation: List.map applies Float.of_int to the original int list elementwise which
then makes it a list of floats.*)

let _ = (fun f x y -> f y x) ( - ) 2 3
(* - : int = 1*)
(* fun takes f, x, and y and 'reorders' the expression to f, y, x. So -, 2, 3 becomes
  -, 3, 2. Given "outfix" notation this means 3-2, or 1.*)

(**************************************************************************************************)
(* Question 2: Writing Simple Expressions (2 + 5 points) *)

(* 2a. Recall the formula for the solutions of a quadratic equation a * x ** 2 + b * x + c = 0:

       (-b +/- sqrt(b ** 2 - 4 * a * c)) / (2 * a).

       The quantity b ** 2 - 4 * a * c is called the discriminant. When a, b and c are real numbers,
       the roots are real-valued iff the discriminant is non-negative.

       Write a function solveQuad : float -> float -> float -> (float * float) option which returns
       the real-valued roots of a quadratic equation, if they exist.

       In the following declaration, replace the call to raise with your implementation. *)

let solveQuad : float -> float -> float -> (float * float) option =
  fun a b c -> 
    let discriminant = b ** 2.0 -. 4.0 *. a *. c in
    if discriminant >= 0.0 then
      let root1 = (-.b +. sqrt(discriminant)) /. (2.0 *. a) in
      let root2 = (-.b -. sqrt(discriminant)) /. (2.0 *. a) in
      Some (root1, root2)
    else
      None
  


(****)
(* 2b. For a given initial positive integer value n1, the Collatz sequence n1, n2, n3, ... is
       defined as follows:

       n{i + 1} = ni / 2, if ni is even, and
                  3 * ni + 1, otherwise.

       For example, for n1 = 3, the sequence is 3, 10, 5, 16, 8, 4, 2, 1, 4, 2, 1, 4, 2, 1, ..., and
                    for n1 = 12, the sequence is 12, 6, 3, 10, 5, 16, 8, 4, 2, 1, ...

       The famous Collatz conjecture states that regardless of the initial value n1, the sequence
       always eventually reaches 1.

       Write a function collatzLen : int -> int such that collatzLen n1 is the number of elements in
       the Collatz sequence before attaining the value 1. For example, collatzLen 3 = 7,
       collatzLen 12 = 9, and collatzLen 1 = 0.

       For full credit, make sure that your implementation is tail-recursive. *)

let collatzLen : int -> int =
  fun n -> 
    let rec aux n count =
      if n = 1 then count
      else if n mod 2 = 0 then aux (n / 2) (count + 1)
      else aux (3 * n + 1) (count + 1)
    in
    aux n 0
  ;;

(* We take in an integer n and define the recursive function aux which has three cases:
   Either n=1, n is even or n is odd and not 1. In the first case, which is the base case we
   return count (if we input 1 this is 0) and in the other two cases we add 1 and recurse.*)

(**************************************************************************************************)
(* Question 3: Type Inhabitation (4 + 4 points)

   3a. Provide examples of expressions with the following types. Uncomment your responses after
       filling them in.*)

(*3.1*) let e3a1 : int = 42;;
(*3.2*) let e3a2 : int = [1;2;3];;
(*3.3*) let f (x : int list) = List.length x;;
(*3.4*) let apply_function arg f = f arg;; 
(*3.5*) let apply f(arg1, arg2) = f arg1 arg2;;
(*3.6*) let apply f(x) y = f(x, y);;
(*3.7*) open Either;;
        let apply_function_to_either either f =
          match either with
            | Left a -> f a
            | Right b -> b;;
(*3.8*) open Either;;
        let sep (a, b) = Right b;;

(* 3b. Provide examples of expressions e so that the following expressions are well-typed:*)

(*e3b1*):
  let e = 5;;
  3 + e = 8;;

(*e3b2*): 
  let e = int_of_float( )
  3 + (int_of_float(3.2));;

(*e3b3*): 
  let e = List.length
  (List.length [1; 2; 3]) + 2 = 5

(*e3b4*): 
  let e = (fun y -> y+1)
  (fun x -> x 0)(fun y -> y+1) = 1 



(**************************************************************************************************)
(* Question 4: Type Design (5 + 2 + 3 points)

   Your instructor, M, recently adopted a pair of cats, Eddie and Bergin, who are best friends with
   each other. See https://r-mukund.github.io/images/eb.jpg for a picture of them. Unfortunately,
   these are also his first pets. He doesn't know much about cats, what food and how much they eat,
   and what they should play with. He visits his neighborhood pet store, and spots several items on
   the shelves. The items are either food stuffs (each of these has a calorie value and a price) or
   toys (each of these only has a price).

   4a. Design an algebraic data type to describe each item in the store's inventory: *)




type item = Food of int*int | Toy of int;;


(*     Write two functions, makeFood and makeToy, which make a corresponding value of type item: *)
let makeFood calories price = Food(calories, price);;
let makeToy price = Toy(price);;


(****)
(* 4b. M, of course, wants to give his new pet cats the happiest lives possible. Write a function
       that calculates the total price of the store's inventory. *)
let rec totalPrice lst = match lst with 
| [] -> 0
|hd::tl ->
  (match hd with
    |Food (e1, e2) -> e2
    |Toy(e) -> e )
  + totalPrice tl;; 


    

(*     Explain your solution: Here we have a type item which can either be a Food or a Toy. We 
   use a separate constructor to make a Food item and a Toy item. Then, totalPrice looks at a list
   of items and if its empty returns 0. Otherwise it iterates over the list, tallying up the price 
    of the food or toy (whichever the item is) and returning the total. *)

(****)
(* 4c. M also wants his cats to be healthy, and sadly suddenly realizes that faculty salaries are
       not comparable to those in the industry. Given a daily calorie target, find the smallest
       price he can expect to pay each month to keep his cats well-fed? *)

(* SOLVE THIS ONE, I DON'T KNOW HOW YET...
let cheapestDiet : item list -> int -> int option =
  fun l target -> raise NotImplemented *)

(*     Assume that there is only one of each item in the inventory. If there is insufficient food to
       adequately feed Eddie and Bergin, report None.

       Explain your solution: ____________ *)

(**************************************************************************************************)
(* Question 5: Evaluation Order (2 + 3 points)

   5a. Dr. Nefario is building the device that will allow him and Gru to steal the Moon. He needs to
       do lots of calculations, but is tired of divide-by-zero causing OCaml to complain. He decides
       to settle the matter once and for all, and declares: "When you divide by zero, the result is
       zero!" He writes the following function: *)

let safeDivideIncorrect =
  fun a b -> let ans = a / b in if b <> 0 then ans else 0

(*     but to his horror, safeDivideIncorrect 3 0 continues to raise an error.

       Explain why: Here, the expression let ans = a / b is calculated whether b
       is zero or not because it precedes the phrase "in if b <> 0." In order to fix
      this we should put the if clause first. 

       Help Dr. Nefario correctly implement safeDivide: *)

let safeDivideCorrect =
  fun a b -> if b <> 0 then a / b else 0;;

(****)
(* 5b. What gets printed to the screen when evaluating the following expressions? *)

let e5b1 =
  if (print_endline "condition"; true)
  then (print_endline "then"; 0)
  else (print_endline "else"; 1)

(*     Answer: condition
               then
               val eb51 : int = 0
       Explain: The expression will print "conidition" no matter what, then because
       of the boolean true we will go into the first branch of the if / else which prints "then"
      and sets eb51 to 0, so the final line printed will be val eb51 : int = 0 *)

let e5b2 =
  (print_endline "left"; 3 < 4) && (print_endline "right"; 2 < 3)

(*     Answer: left
               right
               val eb52 : bool = true
       Explain: We first print "left" then due to 3<4 being true we move on to print "right"
       That gives the top line being "left" then on the nextline "right." Because 3<4 and 2<3 the value
       of the bool is true. Note that if we changed the expression on the right to 3>2 it would print
        left \n right \n false because it had to print both statements before realizing the && is false*)

let e5b3 =
  (print_endline "left"; 4 < 3) && (print_endline "right"; 2 < 3)

(*     Answer: left
              val e5b3 : bool = false
       Explain: We begin by printing left. But, as the left expression is false we don't
       need to evaluate the right expression so that one never prints.*)

(**************************************************************************************************)
(* Question 6: Higher-Order Functions (5 points)

   The integral of a function f between two points a and b is defined as (f a + f (a + epsilon) +
   f (a + 2 * epsilon) + ... + f (a + k * epsilon)) * epsilon, where epsilon > 0 is a small real
   number, and k is the largest integer such that a + k * epsilon <= b.

   Write an expression (integral f a b epsilon) which computes the integral of f between a and b
   with interval size epsilon: *)

let integral f a b epsilon = 
  (let num_steps = int_of_float(floor((b -. a) /. epsilon)) in 
  List.init num_steps (fun i -> f(a +. float_of_int i *. epsilon)) 
  |> List.fold_left (fun acc x -> acc +. x) 0.0) *. epsilon;;


(* What is its type? val integral: (float -> float) -> float -> float -> float -> float = <fun> *)

(* Explain your implementation: We begin by calculating the number of steps / subintervals
   that our interval is divided into. From here, we create a list of that size whose elements are f 
   evaluated at a +. i *. epsilon where i ranges over [num_steps]. Then we take the elements of the list,
   add them together and multiply the total by epsilon. *)

(****)
(* BONUS: Solve Exercise 2.6 of the SICP textbook. This exercise will introduce you to Church
   numerals, and is an excellent example of the power of higher-order functions. *)

(**************************************************************************************************)
(* Question 7: One Nontrivial Algorithm (10 points)

   A binary search tree is defined as follows: *)

type tree = Leaf
          | Node of int * tree * tree
          [@@deriving show];;

(* with the invariant that all numbers in the left subtree are strictly less than the value at the
   root, and all numbers in the right subtree are strictly greater than the value at the root: *)

let is_bst : tree -> bool =
  fun t ->
    let rec bounds l t h =
      match t with
      | Leaf -> true
      | Node(n, t1, t2) -> (l < n) &&
        bounds l t1 n &&
        bounds n t2 h &&
        (n < h)
    in bounds min_int t max_int;;

(* Write a function merge : tree -> tree -> tree which merges two binary search trees into a single
   tree containing all numbers in either of the input trees: *)
   let rec inorder_list t =
    match t with
    | Leaf -> []
    | Node (n, t1, t2) -> (inorder_list t1) @ [n] @ (inorder_list t2);;

  let sorted_list_merge lst1 lst2 =
    let rec merge acc lst1 lst2 =
      match (lst1, lst2) with
      | ([], lst) | (lst, []) -> List.rev_append acc lst
      | (hd1 :: tl1, hd2 :: tl2) ->
          if hd1 <= hd2 then
            merge (hd1 :: acc) tl1 lst2
          else
            merge (hd2 :: acc) lst1 tl2
    in
    merge [] lst1 lst2;;

    let sorted_list_to_bst lst =
      let rec aux lst =
        match lst with
        | [] -> Leaf
        | [x] -> Node (x, Leaf, Leaf)
        | _ ->
            let len = List.length lst in
            let mid = len / 2 in
            let left_half = List.take mid lst in
            let right_half = List.drop mid lst in
            let left_tree = aux left_half in
            let right_tree = aux right_half in
            match (left_tree, right_tree) with
            | (Leaf, _) -> Leaf
            | (_, Leaf) -> Leaf
            | (Node (x, _, _), Node (y, _, _)) ->
                Node (x, left_tree, right_tree)
      in
      aux lst;;
    
    let merge bst1 bst2 =
      let sorted_lst1 = inorder_list bst1 in
      let sorted_lst2 = inorder_list bst2 in
      let merged_sorted_lst = sorted_list_merge sorted_lst1 sorted_lst2 in
      sorted_list_to_bst merged_sorted_lst;;
    

(* Explain your implementation: Here, we first convert a BST to a sorted list,
   then merge two sorted lists and after that we convert the newly merged sorted list 
   into a binary search tree. *)

(**************************************************************************************************)
(* Question 8: Run-Length Encoding and Decoding (5 + 5 points)

   The run-length encoding is a simple lossless algorithm for data compression, and was sometimes
   used in fax machines. The idea is to encode a sequence of repeated data items by its length. For
   example, the sequence ['a'; 'a'; 'b'; 'c'; 'c'; 'c'; 'a'] as the sequence [('a', 2); ('b', 1);
   ('c', 3); ('a', 1)], indicating that the element 'a' occurs thrice, followed by a single
   occurrence of 'b', and then by three occurrences of 'c' and one occurrence of 'a', in that order.

   Write two functions, encode : 'a list -> ('a * int) list, and decode : ('a * int) list ->
   'a list, which apply run-length compression to a sequence of data items and recover them
   respectively. Among other properties, ensure that for all lists l, decode (encode l) = l. *)

let rec encode lst =
  match lst with
  | [] -> []
  | hd :: tl ->
    let rec count_prefix acc x lst =
      match lst with
      | [] -> (x, acc) :: encode tl
      | y :: rest when x = y -> count_prefix (acc + 1) x rest
      | _ -> (x, acc) :: encode lst
    in
    count_prefix 1 hd tl
;;

(* Explain: The function takes in a list lst, and has a helper function which keeps
   track of how many times the current element has repeated and attaches that to the front
   of our encoding. Whenever a new element appears the counter resets to 1. *)

let rec decode lst =
  match lst with
  | [] -> []
  | (x, count) :: tl -> 
    let rec replicate n x acc =
      if n <= 0 then acc
      else replicate (n - 1) x (x :: acc)
    in
    replicate count x [] @ decode tl
;;

(* Explain: This function uses a helper replicate, so that whenever it sees x with a 
   number count = n following it, it copies that number n times into a list, thereby decoding it. *)

(**************************************************************************************************)
(* Question 9: Implementing Tail Recursive Functions (4 + 6 + 10 points) *)

(* 9a. Students are commonly introduced to recursion using the Fibonacci function: *)

let rec fib1 : int -> int =
  fun n -> if n = 0 then 0
           else if n = 1 then 1
           else fib1 (n - 1) + fib1 (n - 2)

(*     This function is not tail recursive. Why?
       A function is tail recursive if the recursive call is the last thing the function does. Here,
        as there are two recursive calls, they cannot both be the last call and therefore the function isn't
       tail recursive. *)

(*     Write an alternative tail recursive implementation of the Fibonacci function: *)

let fib2 : int -> int =
  fun n -> let rec fibonacci_helper a b count =
    if count = n then a
    else fibonacci_helper b (a + b) (count + 1)
    in
      if n <= 0 
        then None 
      else 
        Some (fibonacci_helper 0 1 0)
  ;;

(*     Explain: The only recursive call in the helper is the last line of the helper,
   therefore it is tail recursive. *)

(*     Evaluate fib1 50 and fib2 50. What do you observe? Why?
       fib1 50 can't even evaluate, my computer can't handle it...this is due to the huge number
       of recursive calls. While fib2 is tail recursive so there are fewer calls and it can stil evaluate. *)

(****)
(* 9b. (fold_right) Recall the definitions of fold_left and fold_right: *)

let rec fold_left =
  fun f acc l ->
    match l with
    | [] -> acc
    | hd :: tl -> fold_left f (f acc hd) tl

let rec fold_right =
  fun f l acc ->
    match l with
    | [] -> acc
    | hd :: tl -> f hd (fold_right f tl acc)

(*     As we will discuss in class, fold_left induces a left-to-right flow of data over the list,
       while fold_right induces the right-to-left flow of data.

       Now, observe that the reference implementation of fold_left is tail-recursive, while
       fold_right is not tail-recursive. Why?
       When one looks at fold_left one sees that fold_left is the last thing being called - making
       it tail recursive. On the other hand, in fold_right the last thing being called is f. Parentheses 
       will make this illustration easier. The last call in fold_left is fold_left(f, (f acc hd), tl), whereas
  in fold_right the last call in the function is f(hd, fold_right(f tl acc)), and therefore
  fold_right is evaluated before f and isn't tail recursive.  *)

(*     Write a tail-recursive implementation of the fold_right function. Ensure that your
       implementation works as a drop-in replacement for the traditional fold_right implementation
       shown above. *)

(*Here is the tail-recursive form of reverse.*)       
let reverse lst =
  let rec reverse_aux acc lst =
    match lst with
    | [] -> acc
    | hd :: tl -> reverse_aux (hd :: acc) tl
  in
  reverse_aux [] lst
;;

(*Using the same strategy, we make a tail-recursive version of fold-right*)
let tail_recursive_fold_right f lst init =
  let rec fold_right_aux acc lst =
    match lst with
    | [] -> acc
    | hd :: tl -> fold_right_aux (f hd acc) tl
  in
  fold_right_aux init lst
;;


(*     Explain your solution: This is analogous to the reverse fold_right method
   where we use an auxiliary function and pattern matching and since the recursive
   call is the last thing called in the function it is tail-recursive. *)

(*     HINT: The main difference between fold_left and fold_right is in the direction of data flow.
       The list reversal function (rev : 'a list -> 'a list) is another function which similarly
       swaps the order of data.
       1. Can you write an implementation of rev which is tail-recursive?
       2. Can you use rev as a building block for a tail-recursive fold_right? *)

(****)
(* 9c. (CHALLENGE PROBLEM) Recall the traditional approach to perform an in-order traversal of a
       tree t: *)

let rec inorder1 : tree -> int list =
  fun t ->
    match t with
    | Leaf -> []
    | Node(a, tl, tr) -> (inorder1 tl) @ [ a ] @ (inorder1 tr);;

(*     Observe that inorder1 is not tail-recursive. Why?
       As there are two calls to inorder1, they cannot both be the last call within the function,
       therefore one of the recursive calls (the first one) is not last, and thus the function
       isn't tail recursive. *)

(*     Implement an equivalent tail-recursive version inorder2 : tree -> int list. *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree;;

let in_order_traversal tree =
  let rec in_order_traversal_aux acc stack current =
    match current with
    | Empty ->
      (match stack with
      | [] -> List.rev acc  
      | (node, left, right) :: rest ->
        in_order_traversal_aux (node :: acc) rest right)
    | Node (value, left, right) ->
      in_order_traversal_aux acc ((value, left, right) :: stack) left
  in
  in_order_traversal_aux [] [] tree
;;

(*     Explain your solution: This is a similar strategy to the previous examples, we use 
   an auxiliary function and in addition to that we basically use a stack to keep track of the nodes
   that are "visited" and recursively go down the left branch of the tree. This problem was really hard.*)

(*     This question might be the most difficult problem in this homework assignment, but captures
       the essence of why _any_ computation can be expressed using tail recursive calls.

       HINT: How would you implement in-order traversal in your favorite imperative language,
       without recursion? *)

(**************************************************************************************************)
(* Question 10: Expression Evaluator (4 + 6 points)

   In this question, we will implement our first interpreters for an extremely simple language of
   arithmetic expressions. Consider the type expr1 of arithmetic expressions defined as follows: *)

type expr1 = Int1 of int
           | Plus1 of expr1 * expr1
           | Minus1 of expr1 * expr1
           | Mult1 of expr1 * expr1
           [@@deriving show];;

(* For example, the value Plus1(Int1 3, Int1 8) represents the expression (3 + 8) which, when
   evaluated, produces the result 11. The base constructor Int1 constructs an expression out of an
   integer value, while the Plus1, Minus1 and Mult1 constructors encode the addition, subtraction
   and multiplication operations respectively.

   10a. Write a function eval1 : expr1 -> int which produces the result of evaluating an
        expression. *)

let rec eval1 : expr1 -> int = 
  fun expr1 -> match expr1 with
  | Int1 n -> n
  | Plus1 (e1 * e2) -> eval1 e1 + eval1 e2
  | Minus1 (e1 * e2) -> eval1 e1 - eval1 e2
  | Mult1 (e1 * e2) -> eval1 e1 * eval1 e2
;;

(*      Next, we extend the type expr1 of expressions with constructors that encode variables and
        let expressions. The following type expr2 is just like expr1, except that the expression
        (Let2 "x" (Int2 3) (Plus2(Var2 "x", Var2 "x"))) stands for the OCaml expression (let x = 3
        in x + x). As in OCaml, variables are statically scoped. *)

type expr2 = Int2 of int
           | Plus2 of expr2 * expr2
           | Minus2 of expr2 * expr2
           | Mult2 of expr2 * expr2
           | Var2 of string
           | Let2 of string * expr2 * expr2
           [@@deriving show];;

(****)
(* 10b. Write a function (eval2 : expr2 -> int option) which returns the result of evaluating an
        expression, if it is defined.

        You will find it useful to maintain a map from variable names to bound values. We invoke the
        following magic incantation to start you off: *)

module StringMap = Map.Make(String);;
let empty_map = StringMap.empty;;

(*     To bind a key k to a value v, write (StringMap.add k v m). For example: *)

let m1 = StringMap.add "x" 3 empty_map;;

(*     To check if a map m contains a key k, write (StringMap.find_opt k m). For example,
       (StringMap.find_opt "x" m1) evaluates to Some 3, and (StringMap.find_opt "y" m1) evaluates to
       None.

       Note that maps are immutable objects: the result of evaluating StringMap.add is a new map
       with the desired key-value association; the original map is unaltered. For example,
       (StringMap.find_opt "x" empty_map) still evaluates to None. *)

let eval2 (env : int StringMap t) (e : expr2) : int option = match e with 
       | Int2 n -> Some n
       | Plus2 (e1, e2) -> 
          (match (eval2 env e1, eval2 env e2) with
          | (Some f1, Some f2) -> Some (f1 + f2)
          | _ -> None)
       |Minus2 (e1, e2) -> (match (eval2 env e1, eval2 env e2) with
        | (Some f1, Some f2) -> Some(f1 - f2)
        | _ -> None) 
       |Mult2 (e1, e2) -> (match (eval2 env e1, eval2 env e2) with
        | (Some f1, Some f2) -> Some(f1 * f2)
        | _ -> None)
       |Var2 x -> StringMap.find_opt x env 
       |Let2 (x, e1, e2) -> (match eval2 env e1 with
        |Some x_val -> let environment = StringMap.add x x_val env in
          eval2 environment e2 
        |None -> None);;
(*     Explain your solution: In eval2 the In2, Plus2, Minus2 and Mult2 pattern matchings
   follow the same methodology as eval1. We have to pattern match within the pattern match though and we have
   two additional possibilities within the type expr2: these are Var2 which gives a variable name and
   Let2 which is the binding of a variable. Here we had to use StringMap.add as suggested above.  *)

(**************************************************************************************************)
(* Question 11: Conclusion (0 points)

   Did you use an LLM to solve this assignment? How did it help?
   Yes. I used an LLM when I had questions that I couldn't quickly find answers to on
   google or StackExchange. I don't trust the LLM too much, but it is a good starting place
   if I'm not entirely sure of what direction to head in. After all, it is a lot simpler to verify
   information than to generate novel ideas. When I am stuck I asked the LLM for ideas, but rather
   than simply copying them I first worked to prove that what it did actually worked, then I worked to 
   understand why it worked, and once I had a better understanding I attempted to recreate the solution on my own. *)