module type Set = sig
  type 'a t

  val empty : 'a t

  val insert : 'a -> 'a t -> 'a t

  val mem : 'a -> 'a t -> bool

  val string : ('a -> string) -> 'a t -> string
end

module ListSet : Set = struct
  type 'a t = 'a list

  let empty = []

  let mem = List.mem

  let insert x s =
    if mem x s then
      s
    else
      x :: s

  let string to_string s =
    s
    |> List.map (fun x -> to_string x ^ "; ")
    |> List.fold_left (fun x y -> x ^ y) " "
    |> (fun x -> "[" ^ x ^ "]")
end

module BstSet : Set = struct
  (** AF: 根节点 [Leaf] 表示空 Set，[Node (l, v, r)] 表示一个包含了值 v 的 set，
      它的左子元素为 [l]，右子元素为 [r]
      RI：对于每一个 [Node (l, v, r)]，任意左节点 [l] 对应的值一定小于 [v]，
      任意右节点 [r] 的值一定大于 [v]
  *)
  type 'a t =
    | Leaf
    | Node of 'a t * 'a * 'a t

  let empty = Leaf

  (** Efficiency: 最好 O(log n) 最差 O(n) => O(n)
      当元素本身是有顺序时，会退化成列表 *)
  let rec mem x = function
    | Leaf -> false
    | Node (l, v, r) ->
      if x < v then
        mem x l
      else if x > v then
        mem x r
      else
        true

  (** Efficiency: 最好 O(log n) 最差 O(n) => O(n)
      当元素本身是有顺序时，会退化成列表
      并且后续的每次插入都会重新构造子树 *)
  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) as n ->
      if x < v then
        Node (insert x l, v, r)
      else if x > v then
        Node (l, v, insert x r)
      else
        n

  let rec string_aux to_string = function
    | Leaf -> " "
    | Node (l, v, r) -> string_aux to_string l ^ (to_string v ^ ";") ^ string_aux to_string r

  let string to_string set = "[" ^ string_aux to_string set ^ "]"
end

(** 红黑树集合 *)
module RbSet : Set = struct
  type color =
    | Red
    | Blk

  type 'a t =
    | Leaf
    | Node of (color * 'a t * 'a * 'a t)

  let empty = Leaf

  (** Efficiency: O(log n) *)
  let rec mem x = function
    | Leaf -> false
    (* 查找不需要考虑颜色 *)
    | Node (_, l, v, r) ->
      if x < v then
        mem x l
      else if x > v then
        mem x r
      else
        true

  (** 无论是四种非法形式的哪一种，都修复为统一的合法格式
      Efficiency: O(1) *)
  let blance = function
    | (Blk, Node (Red, Node (Red, a, x, b), y, c), z, d) (* 1 *)
    | (Blk, Node (Red, a, x, Node (Red, b, y, c)), z, d) (* 2 *)
    | (Blk, a, x, Node (Red, Node (Red, b, y, c), z, d)) (* 3 *)
    | (Blk, a, x, Node (Red, b, y, Node (Red, c, z, d))) (* 4 *) ->
      Node (Red, Node (Blk, a, x, b), y, Node (Blk, c, z, d))
    | t -> Node t

  (** Efficiency: O(log n) *)
  let rec insert_aux x = function
    | Leaf -> Node (Blk, Leaf, x, Leaf)
    | Node (c, l, v, r) as n ->
      if x < v then
        blance (c, insert_aux x l, v, r)
      else if x > v then
        blance (c, l, v, insert_aux x r)
      else
        n

  (** 检查插入后的根节点状态，并将根节点置为黑色
      Efficiency: O(log n) *)
  let rec insert x s =
    match insert_aux x s with
    | Leaf -> failwith "Impossible Case"
    | Node (_, l, v, r) -> Node (Blk, l, v, r)

  let rec string_aux to_string = function
    | Leaf -> " "
    | Node (_, l, v, r) -> string_aux to_string l ^ (to_string v ^ ";") ^ string_aux to_string r

  let string to_string set = "[" ^ string_aux to_string set ^ "]"
end

let () =
  let set = RbSet.empty in
  let set = RbSet.insert 6 set in
  let set = RbSet.insert 2 set in
  let set = RbSet.insert 3 set in
  let set = RbSet.insert 1 set in
  let set = RbSet.insert 3 set in
  let set = RbSet.insert 5 set in
  let set = RbSet.insert 6 set in
  let set = RbSet.insert 1 set in
  let set = RbSet.insert 4 set in
  print_string (RbSet.string string_of_int set)
