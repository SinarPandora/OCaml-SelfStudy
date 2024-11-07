module type Map = sig
  (** [t] 是包含类型为 'k 的键，类型为 'v 的值的 map *)
  type ('k, 'v) map

  (** [insert k v m] 返回一个将键值对 [k]，[v] 插入到 map [m] 中的新 map
      如果 [k] 存在，覆盖之前的 [k] 对应的 [v] *)
  val insert : 'k -> 'v -> ('k, 'v) map -> ('k, 'v) map

  (** [find k m] 在 map [m] 中寻找键 [k] 对应的值 [v]
      如果找到，返回 [Some m] 否则返回 [None] *)
  val find : 'k -> ('k, 'v) map -> 'v option

  (** [remove k m] 返回删除了键 [k] 的 map [m]，
      如果 map [m] 中不存在键 [k]，则什么都不会发生 *)
  val remove : 'k -> ('k, 'v) map -> ('k, 'v) map

  (** [empty] 生成一个空 map *)
  val empty : ('k, 'v) map

  (** [of_list lst] 将一个键值对为元素的 [lst] 转换为对应类型的 map
      Requires: [lst] 不包含重复的键值对 *)
  val of_list : ('k * 'v) list -> ('k, 'v) map

  (** [bindings m] 将 map [m] 展开为 [('k * 'v)] 键值对为元素的列表 *)
  val bindings : ('k, 'v) map -> ('k * 'v) list
end

module AssocListMap : Map = struct
  (** AF: [(k1, k2); (k2, v2); ...; (kn, vn)]
          视为 Map {k1 : v1; k2 : v2; ...; kn : vn}
          如果列表包含重复键，取最左侧第一个键键值对的值作为对应的值
          例如：[(k1, v1), (k1, v2)]，此时 k1 对应的值为 v1
      RI: none
  *)
  type ('k, 'v) map = ('k * 'v) list

  (** Efficiency: O(1) *)
  let insert k v m = (k, v) :: m

  (** Efficiency: O(n) *)
  let find = List.assoc_opt

  (** Efficiency: O(n) *)
  let remove k m = List.filter (fun (k', _) -> k <> k') m

  let empty = []

  (** Efficiency: O(1) *)
  let of_list lst = lst

  (** [keys m] 将列出 map 中所有的键
      Efficiency: O(n * log n) *)
  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

  (** [binding m k] 返回键 [k] 对应的键值对 [(k, v)]
      Requires: [k] 是 map [m] 中的键
      Efficiency: O(n) *)
  let binding m k = (k, List.assoc k m)

  (** Efficiency: O(n * log n) + O(n) * O (n) => O(n^2)（n * log n 被忽略） *)
  let bindings m = List.map (binding m) (keys m)
end

(** 使用 int array 实现的 map
    由于 array 可变，所有更新返回 unit *)
module type DirectAddressMap = sig
  (** [t] 是包含类型为 int 的键，类型为 'v 的值的 map *)
  type 'v t

  (** [insert k v m] 返将键值对 [k]，[v] 插入到 map [m]
      如果 [k] 存在，覆盖之前的 [k] 对应的 [v]
      Requires: [k] 是 [m] 范围中的键 *)
  val insert : int -> 'v -> 'v t -> unit

  (** [find k m] 在 map [m] 中寻找键 [k] 对应的值 [v]
      如果找到，返回 [Some m] 否则返回 [None]
      Requires: [k] 是 [m] 范围中的键 *)
  val find : int -> 'v t -> 'v option

  (** [remove k m] 从 map [m] 删除键 [k]，
      如果 map [m] 中不存在键 [k]，则什么都不会发生
      Requires: [k] 是 [m] 范围中的键 *)
  val remove : int -> 'v t -> unit

  (** [create c] 创建一个指定大小的空 map
      [c] 是容量（capacity）
      键 [0] 到 [c - 1] 是在字典范围内的有效键 *)
  val create : int -> 'v t

  (** [of_list c lst] 将一个键值对为元素的 [lst] 转换为对应类型的 map
      Requires: [lst] 不包含重复的键值对，
      并且 [lst] 中所有的键都在 0 ~ [c-1] 的长度范围内 *)
  val of_list : int -> (int * 'v) list -> 'v t

  (** [bindings m] 将 map [m] 展开为 [('k * 'v)] 键值对为元素的列表 *)
  val bindings : 'v t -> (int * 'v) list
end

module ArrayMap : DirectAddressMap = struct
  (** AF: [[|Some v0; Some v1; ...|]] ->
     {0: v0; 1: v1; ... }
     如果键对应的值为 None，说明字典中没有（未使用）该键
     RI: none *)
  type 'v t = 'v option array

  (** Efficiency: O(1) *)
  let insert k v m = m.(k) <- Some v

  (** Efficiency: O(1) *)
  let find k m = m.(k)

  (** Efficiency: O(1) *)
  let remove k m = m.(k) <- None

  (** Efficiency: O(c) *)
  let create c = Array.make c None

  (** Efficiency: O(c) *)
  let of_list c lst =
    (* O(c) *)
    let a = create c in
    (* O(c) * O(n) = O(n)，最坏情况下，O(n) = O(c)  *)
    List.iter (fun (k, v) -> insert k v a) lst;
    a

  (** Efficiency: O(c) *)
  let bindings m =
    m
    |> Array.to_seqi (* O(c), c is the capacity *)
    |> Seq.filter_map (fun (i, v_opt) ->
           (* O(c) *)
           match v_opt with
           | Some v' -> Some (i, v')
           | None -> None
       )
    |> List.of_seq (* O(c) *)
end

module type TableMap = sig
  (** [t] 是包含类型为 'k 的键，类型为 'v 的值的 map *)
  type ('k, 'v) t

  (** [insert k v m] 返将键值对 [k]，[v] 插入到 map [m]
      如果 [k] 存在，覆盖之前的 [k] 对应的 [v]*)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit

  (** [find k m] 在 map [m] 中寻找键 [k] 对应的值 [v]
      如果找到，返回 [Some m] 否则返回 [None]
      *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] 从 map [m] 删除键 [k]，
      如果 map [m] 中不存在键 [k]，则什么都不会发生
      *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [create hash c] 创建一个指定大小的空 map
      [hash] 为哈希函数，[c] 为初始容量
      Requires: [hash] 函数应尽可能让键均匀分布在桶中
  *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [of_list c lst] 将一个键值对为元素的 [lst] 转换为对应类型的 map
      Requires: [lst] 不包含重复的键值对，
      并且 [lst] 中所有的键都在 0 ~ [c-1] 的长度范围内 *)
  (* val of_list : ('k * 'v) list -> ('k, 'v) t *)

  (** [bindings m] 将 map [m] 展开为 [('k * 'v)] 键值对为元素的列表 *)
  (* val bindings : ('k, 'v) t -> ('k * 'v) list *)
end

module HashMap : TableMap = struct
  (** AF: [| [(k11, v11); (k12, v12); ...]
           [(k21, v21); (k22, v22); ...] |]
		->
        {k11: v11, k12: v12, ...,
         k21: v21, k22: v22, ..., ...}
    RI: 1. 每个键只能出现一次
        2. 所有键都被放在正确的桶里：
           如果 k 在 b 桶，说明 hash(k) = b
        3. 哈希函数需要始终返回非负数
        4. 哈希函数 [hash] 的效率必须为常量
  *)
  type ('k, 'v) t = {
    hash: 'k -> int;
    mutable size: int;
    mutable buckets: ('k * 'v) list array;
  }

  (** [capacity tab] 目前桶的数量
      Efficiency: O(1) *)
  let capacity tab = Array.length tab.buckets

  (** [index k tab] 获得 [k] 应该被放在 [tab] 中的哪个桶中
      Efficiency: O(1) *)
  let index k tab = tab.hash k mod capacity tab

  (** [insert_no_resize k v tab] 将键值对插入到 map 中，
      并不负责对 map 进行扩容，即便负载因子已达到上限。
      Efficiency: O(L) *)
  let insert_no_resize k v tab =
    let b = index k tab in
    (* O(1) *)
    let old_bucket = tab.buckets.(b) in
    (* O(1) *)
    (* 删除（可能存在的）重复的绑定，并插入新绑定 *)
    (* remove_assoc O(L)，由于桶大小可空（负载因子）因此为常量 *)
    tab.buckets.(b) <- (k, v) :: List.remove_assoc k old_bucket;
    (* mem_assoc O(L)，由于桶大小可空（负载因子）因此为常量 *)
    if not (List.mem_assoc k old_bucket) then tab.size <- tab.size + 1

  (** [load_factor tab] 计算哈希表的扩展因子 *)
  let load_factor tab = float_of_int tab.size /. float_of_int (capacity tab)

  (** [rehash_all tab c] 将桶数组替换成容量为 [c] 的新数组，并重新计算所有键的哈希
      Efficiency: O(n)，n 是当前绑定的键值对数量 *)
  let rehash_all tab c =
    (* 插入绑定到新的桶 *)
    let rehash_binding (k, v) = insert_no_resize k v tab in
    (* 插入全部的绑定到新的桶 *)
    let rehash_bucket b = List.iter rehash_binding b in
    let old_buckets = tab.buckets in
    (* O(n) *)
    tab.buckets <- Array.make c [];
    tab.size <- 0;
    (* O(n) *)
    Array.iter rehash_bucket old_buckets

  (** [resize_if_needed tab] 将在扩展因子大于 2 或小于 1/2 时，重新哈希所有元素
     并更新桶大小 *)
  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then
      rehash_all tab (capacity tab * 2)
    else if lf < 0.5 then
      rehash_all tab (capacity tab / 2)

  (** Efficiency: O(n) *)
  let insert k v tab =
    insert_no_resize k v tab;
    resize_if_needed tab

  let find k tab = List.assoc_opt k tab.buckets.(index k tab)

  (** [remove_no_resize k tab] 从哈希表 [tab] 中直接删除键 [k]，
      并不负责对 map 进行缩容，即便负载因子已达到下限。
      Efficiency: O(L) *)
  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k old_bucket;
    if List.mem_assoc k old_bucket then tab.size <- tab.size - 1

  (** Efficiency: O(n) *)
  let remove k tab =
    remove_no_resize k tab;
    resize_if_needed tab

  (** Efficiency: O(c)
      哈希函数 [h] 需要始终返回非负数，且效率为常量 *)
  let create h c = { hash = h; size = 0; buckets = Array.make c [] }
end
