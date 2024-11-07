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

  let rehash_all tab c =
    let rehash_binding (k, v) = insert_no_resize k v tab in
    let rehash_bucket b = List.iter rehash_binding b in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make c [];
    tab.size <- 0;
    Array.iter rehash_bucket old_buckets

  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then
      rehash_all tab (capacity tab * 2)
    else if lf < 0.5 then
      rehash_all tab (capacity tab / 2)

  let insert k v tab =
    insert_no_resize k v tab;
    resize_if_needed tab

  let find k tab = List.assoc_opt k tab.buckets.(index k tab)

  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k old_bucket;
    if List.mem_assoc k old_bucket then tab.size <- tab.size - 1

  let remove k tab =
    remove_no_resize k tab;
    resize_if_needed tab

  let create h c = { hash = h; size = 0; buckets = Array.make c [] }
end
