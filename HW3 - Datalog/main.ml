(***************************************************************************************************
Homework 3
==========
CSCI 599, Fall 2023: An Introduction to Programming Languages
Mukund Raghothaman
Due: 10pm PT on 7 December, 2023
***************************************************************************************************)

(***************************************************************************************************
- Your name: Aaron Dardik
- Your partner's name (if any): Vineet 
***************************************************************************************************)

(***************************************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in the cheatsheet and
   on the course website.

2. Please feel free to collaborate with a partner, but make sure to write your submission
   independently.

3. This assignment consists of five files: main.ml (this file), q2-cipher.ods, q3-traffic.sqlite,
   q5-genealogies.dl, and q6-incr.dl. Once you have completed your work, place these files in a
   directory named HW3-YourName and compress the directory into a file named HW3-YourName.zip.
   Submit your work using Blackboard.

4. IMPORTANT: Make sure that I can cleanly import this file into the Ocaml toplevel when I call
   #use "main.ml". Comment out any questions you were unable to solve / have left unanswered.

***************************************************************************************************)

(*exception NotImplemented*)

(** Distribution of points: 15 + 15 + 10 + 10 + 10 + 20 = 80 points *)

(**************************************************************************************************)
(* Question 1: Incremental Evaluation in Spreadsheets (8 + 7 = 15 points)

   One important feature of spreadsheet applications is their ability to automatically recompute
   cells when the user changes the data. Engineers have to be careful while implementing this
   feature, since it can make the program unresponsive when the spreadsheet is large. Efficiently
   recomputing the results when the input data changes is an important problem in many practical
   applications. One common optimization is to use the dependence relation between the cells to only
   recompute those cells which might have changed.

   In this question, we will study this optimization.

   We start by defining "cells". A cell has an address, which we represent as a pair of integers:
   the indices of the row and column in which it appears. This is somewhat different from the
   traditional alphanumeric addressing (e.g. "G5"), but converting from one representation to the
   other is trivial. *)

type cell_address = int * int

(* For the purposes of this question, we are unconcerned with the formulas inside the cells, we only
   worry about their mutual dependencies. We will represent these dependencies as a list of pairs,

     deps : (cell_addrress * cell_address) list

   Each element (c1, c2) of deps indicates that the value of c2 is dependent on the value of c1.
   Recall that for the spreadsheet to be well-formed, these dependencies must form a DAG. Write a
   function to determine whether this is the case. *)

let is_dag (deps : (cell_address * cell_address) list) : bool =
  let rec dfs (visited : cell_address list) (current_path : cell_address list) (graph : (cell_address * cell_address) list) (node : cell_address) : bool =
    if List.mem node current_path then
      false (* Cycle detected *)
    else if not (List.mem node visited) then
      let dependencies = List.filter (fun (n, _) -> n = node) graph in
      List.fold_left (fun acc (_, dest) -> acc && dfs visited (node :: current_path) graph dest) true dependencies
    else
      true
  in
  List.fold_left
    (fun acc (src, dest) -> acc && dfs [] [] deps src && dfs [] [] deps dest)
    true deps




(* If your implementation is correct, then it will pass the following tests: *)

let _ =
  let c1 = (1, 1) in
  let c2 = (1, 2) in
  let c3 = (2, 1) in
  let c4 = (2, 2) in

  let _ = assert (is_dag [(c1, c2); (c2, c3); (c3, c4) ]) in
  let _ = assert (not (is_dag [(c1, c2); (c2, c3); (c2, c1)])) in
  let _ = assert (not (is_dag [(c1, c1)])) in
  () 



let test_is_dag () =
  let c1 = (1, 1) in
  let c2 = (1, 2) in
  let c3 = (2, 1) in
  let c4 = (2, 2) in

  let deps_valid = [(c2, c4); (c1, c2); (c3, c4)] in
  let deps_invalid = [(c1, c2); (c2, c3); (c2, c1)] in

  assert (is_dag deps_valid);
  assert (not (is_dag deps_invalid))

let () =
  test_is_dag ();
  print_endline "All tests passed for is_dag function."





(* Now write a function that accepts the list of dependencies that describes a spreadsheet, and the
   address of the cell initially changed by the user, and determine the cells which need
   recomputation, and the order in which they need to be recomputed. For this function, you may
   assume that (is_dag deps) evaluates to true. *)

module CellSet = Set.Make(
    struct
    type t = cell_address
    let compare = compare     
  end)
 
  (*deps is a (cell_address * cell_address) list - it is also known as graph in some of my calls. A
     cell address c1, for instance, can be c1 = (1, 1). deps - our dependency graph - is written in 
     the form of deps = [(c1, c2) ; (c2, c4) ; (c3, c4)] for example. *)
 
  (*Here stack is going to start empty, [] and we will be adding things to the beginning of the stack*)


  let topological_sort graph =
    let rec visit node visited stack =
      if CellSet.mem node visited then
        stack
      else
        let neighbors = try List.assoc node graph with Not_found -> [] in
        let stack' = List.fold_left (fun acc neighbor -> visit neighbor (CellSet.add node visited) acc) stack neighbors in
        node :: stack'
    in
  
    let rec visit_all nodes visited stack =
      match nodes with
      | [] -> stack
      | node :: rest -> visit_all rest visited (visit node visited stack)
    in
  
    visit_all (List.map fst graph) CellSet.empty []
  

    let plan_reeval (deps : (cell_address * cell_address) list) (init : cell_address) : cell_address list =
      let graph = List.fold_left (fun acc (src, dest) -> (src, dest :: try List.assoc src acc with Not_found -> []) :: acc) [] deps in
      let visited = ref CellSet.empty in
      let output_edges = List.map snd deps |> List.sort_uniq compare in
    
      let rec dfs node acc =
        if CellSet.mem node !visited then
          acc
        else begin
          visited := CellSet.add node !visited;
          let neighbors = try List.assoc node graph with Not_found -> [] in
          let acc' = List.fold_left (fun acc neighbor -> dfs neighbor acc) acc neighbors in
          if List.mem node output_edges && not (List.mem node acc') then node :: acc'
          else acc'
        end
      in
    
      dfs init []
    
    


  let test_plan_reeval () =
    let c1 = (1, 1) in
    let c2 = (1, 2) in
    let c3 = (2, 1) in
    let c4 = (2, 2) in
  
    let deps = [(c2, c4); (c1, c2); (c3, c4); (c1, c3)] in
  
    Printf.printf "Dependencies: %s\n" (String.concat "; " (List.map (fun (src, dest) -> Printf.sprintf "(%d, %d) -> (%d, %d)" (fst src) (snd src) (fst dest) (snd dest)) deps));
  
    let init_node = c1 in
    Printf.printf "Initial Node: (%d, %d)\n" (fst init_node) (snd init_node);
  
    let recomputation_order = plan_reeval deps init_node in
    Printf.printf "Recomputation Order: %s\n" (String.concat " -> " (List.map (fun (r, c) -> Printf.sprintf "(%d, %d)" r c) recomputation_order))
  
  let () =
    test_plan_reeval ()
  




(**************************************************************************************************)
(* Question 2: Implementing Substitution Ciphers in a Spreadsheet (15 points)

   In this question, we will construct a simple substitution cipher using a spreadsheet. The cipher
   we implement is called the Vigenere cipher. We begin with a plaintext message, such as
   "CLASSFROMONETOFOURTHIRTY", and a key, such as "CSCI". It repeats the key as many times as needed
   to cover the plaintext:

     CLASSFROMONETOFOURTHIRTY
     CSCICSCICSCICSCICSCICSCI

   It then shifts each letter in the plaintext message by the corresponding number of characters to
   obtain the ciphertext. For example, shifting the plaintext character C by the key character C
   results in the character C + C = C + 2 = E. Similarly, shifting the plaintext character L by the
   key character S results in the ciphertext character L + S = L + 18 = D. Taken together, this
   would result in the ciphertext:

     CLASSFROMONETOFOURTHIRTY
     CSCICSCICSCICSCICSCICSCI
     ------------------------
     EDCAUXTWOGPMVGHWWJVPKJVG

   Look at the spreadsheet cipher.ods, which you may open using the Calc spreadsheet program. The
   cell B1 contains the length of the key, and the cells B2:E2 contain the key chosen. The cells
   B4:Y4 contain the plaintext message.

   Complete this spreadsheet so that the desired ciphertext message "EDCAUXTWOGPMVGHWWJVPKJVG"
   appears in the ciphertext cells, B5:Y5. Assume that all characters are in uppercase letters
   between 'A' and 'Z'. You may use any other part of the spreadsheet for any purpose.

   You may use the tool available on the website https://www.dcode.fr/vigenere-cipher to test your
   implementation and to confirm encodings and decodings. *)

(**************************************************************************************************)
(* Question 3: Crashing Computers and Overdue Assignments (10 points)

   Here is data about the traffic in New York City at a particular moment in time. There are three
   tables:
   1. The table Intersects summarizes the city's geography, and contains all pairs of streets (x, y)
      such that there is an intersection between x and y. Of course, this table is commutative, so
      for every tuple (x, y) in it, it also contains the tuple (y, x).
   2. The table HasTraffic contains a single column, and lists all those streets with heavy traffic
      at that moment.
   3. The table GreenLight also contains only a single column, and lists all those streets where the
      traffic light is green.

   Intersects(s1, s2)
   -----------------------
   Broadway   | Liberty St
   Liberty St | Broadway
   Liberty St | William St
   William St | Liberty St
   William St | Wall St
   Wall St    | William St
   Wall St    | Broadway
   Broadway   | Wall St
   Broadway   | Whitehall
   Whitehall  | Broadway

   HasTraffic(s)
   -------------
   Broadway
   Wall St
   Whitehall
   William St

   GreenLight(s)
   -------------
   Broadway
   Liberty St
   Whitehall
   William St

   Your friend is taking a course on urban planning, and wrote a SQL query which computed the
   following output:

   Mystery(s1, s2)
   ---------------------
   Broadway  | Whitehall
   Whitehall | Broadway

   Unfortunately, their computer crashed, and the assignment is due today. What SQL query could they
   have written? Please test your response using the attached SQLite database.

   SELECT DISTINCT I.s1, I.s2
    FROM Intersects I
    JOIN HasTraffic HT1 ON I.s1 = HT1.s
    JOIN HasTraffic HT2 ON I.s2 = HT2.s
    JOIN GreenLight GL1 ON I.s1 = GL1.s
    JOIN Greenlight GL2 ON I.s1 = GL1.s 
    




   Bonus points: What could this query have been about?
   
   Answer: 
   At what intersection did an accident occur?
   *)

   


(**************************************************************************************************)
(* Question 4: Understanding SQL queries (10 points)

   Consider the following pairs of SQL queries. Will they produce the same result on all databases,
   or can you provide an example database on which their outputs differ? Justify your answers. You
   may assume that the tables r and s have the same schema.

   1. select *
      from (select * from r) union all (select * from s)
      where p

      and

      (select * from r where p)
      union all
      (select * from s where p)


    These two queries are equivalent due to the fact that
    regardless of whether one performs a select * from operatio after union all (ex 1)
    or if the union all occurs select * (case 2) the results will be the same. 
    In SQL both ways of querying - select * outside of union or inside of union - both ways
    produce "double ups." 




   2. (select * from r where p)
      union all
      (select * from s where p)

      and

      (select * from r)
      union all
      (select * from s where p) 


  These two queries are also not equivalent. 
  The WHERE condition in the first query applies to both r and s. 
  In the second query, the WHERE condition is only applied to the s part of the union. 
  As a result, these queries will generally produce different results.


   

  For the second pair of queries (2) consider the following database:

    -- Example Table r
  CREATE TABLE r (id INT, value VARCHAR(10));
  INSERT INTO r VALUES (1, 'A'), (2, 'B'), (3, 'C');

    -- Example Table s
  CREATE TABLE s (id INT, value VARCHAR(10));
  INSERT INTO s VALUES (2, 'B'), (3, 'C'), (4, 'D');

    -- Assume p is the condition id > 2



  The first query would return rows from both r and s where value = 'B'.
  The second query would return all rows from r and only rows from s 
  where value = 'B'. In this example, Pair 2 produces different results 
  because the condition value = 'B' is applied to s only in the second 
  query.



      *)
(**************************************************************************************************)
(* Question 5: Genealogies (5 + 5 = 10 points)

   Please follow the instructions in the attached file q5-genealogies.dl. *)

(**************************************************************************************************)
(* Question 6: Incremental Evaluation, Redux (5 + 8 + 7 = 20 points)

   Please follow the instructions in the attached file q6-incr.dl. *)

(**************************************************************************************************)
(* Question 7: Conclusion (0 points)

   Did you use an LLM to solve this assignment? How did it help?
   I used an LLM to help with the examples in Q4 and also with some 
   of the syntax for Q1. That said, it wasn't helpful as it repeatedly
   made mistakes and when I pointed them out it basically said "sorry 
   for the error..." and then made the exact same mistake again. I think
   that ChatGPT (and possibly other LLMs) tell you, to some extent, what
   it THINKS you want to hear. So, if you have a question and then tell
    ChatGPT it was wrong, it will admit it was wrong but it hasn't
   suddenly gained any additional knowledge so it can spit out the very same
   code, knowing it is wrong. *)