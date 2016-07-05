;; ======================
;; CLOJURE MAP FUNCTIONS:
;; ======================
;; Create a new map: hash-map sorted-map sorted-map-by
;; 'change' a map: assoc dissoc select-keys merge merge-with zipmap
;; Examine a map: get contains? find keys vals map?
;; Examine a map entry: key val

;; ======================
;; HASKELL MAP FUNCTIONS:
;; ======================
;; Operators
;; (!) :: Ord k => Map k a -> k -> a
;; O(log n). Find the value at a key. Calls error when the element can not be found.
;; (\\) :: Ord k => Map k a -> Map k b -> Map k a
;; Same as difference.
;; Query
;; null :: Map k a -> Bool
;; O(1). Is the map empty?
;; size :: Map k a -> Int
;; O(1). The number of elements in the map.
;; member :: Ord k => k -> Map k a -> Bool
;; O(log n). Is the key a member of the map? See also notMember.
;; notMember :: Ord k => k -> Map k a -> Bool
;; O(log n). Is the key not a member of the map? See also member.

;; lookup :: Ord k => k -> Map k a -> Maybe a
;; O(log n). Lookup the value at a key in the map.
;; The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map.
;; findWithDefault :: Ord k => a -> k -> Map k a -> a
;; O(log n). The expression (findWithDefault def k map) returns the value at key k or returns default value def when the key is not in the map.
;;  findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
;;  findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

;; Construction
;; empty :: Map k a
;; O(1). The empty map.
;;  empty      == fromList []
;;  size empty == 0
;; singleton :: k -> a -> Map k a
;; O(1). A map with a single element.

;; Insertion
;; insert :: Ord k => k -> a -> Map k a -> Map k a
;; O(log n). Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value. insert is equivalent to insertWith const.
;; insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
;; O(log n). Insert with a function, combining new value and old value. insertWith f key value mp will insert the pair (key, value) into mp if key does not exist in the map. If the key does exist, the function will insert the pair (key, f new_value old_value).
;; insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
;; Same as insertWith, but the combining function is applied strictly. This is often the most desirable behavior.
;; insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
;; O(log n). Insert with a function, combining key, new value and old value. insertWithKey f key value mp will insert the pair (key, value) into mp if key does not exist in the map. If the key does exist, the function will insert the pair (key,f key new_value old_value). Note that the key passed to f is the same key passed to insertWithKey.
;; insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
;; Same as insertWithKey, but the combining function is applied strictly.
;; insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
;; O(log n). Combines insert operation with old value retrieval. The expression (insertLookupWithKey f k x map) is a pair where the first element is equal to (lookup k map) and the second element equal to (insertWithKey f k x map).
;; Delete/Update
;; delete :: Ord k => k -> Map k a -> Map k a
;; O(log n). Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
;; adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
;; O(log n). Update a value at a specific key with the result of the provided function. When the key is not a member of the map, the original map is returned.
;; adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
;; O(log n). Adjust a value at a specific key. When the key is not a member of the map, the original map is returned.
;; update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
;; O(log n). The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y.
;; updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
;; O(log n). The expression (updateWithKey f k map) updates the value x at k (if it is in the map). If (f k x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y.
;; updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
;; O(log n). Lookup and update. See also updateWithKey. The function returns changed value, if it is updated. Returns the original key value if the map entry is deleted.
;; alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
;; O(log n). The expression (alter f k map) alters the value x at k, or absence thereof. alter can be used to insert, delete, or update a value in a Map. In short : lookup k (alter f k m) = f (lookup k m).

;; Combine
;; Union
;; union :: Ord k => Map k a -> Map k a -> Map k a
;; O(n+m). The expression (union t1 t2) takes the left-biased union of t1 and t2. It prefers t1 when duplicate keys are encountered, i.e. (union == unionWith const). The implementation uses the efficient hedge-union algorithm. Hedge-union is more efficient on (bigset `union` smallset).
;; unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
;; O(n+m). Union with a combining function. The implementation uses the efficient hedge-union algorithm.
;; unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
;; O(n+m). Union with a combining function. The implementation uses the efficient hedge-union algorithm. Hedge-union is more efficient on (bigset `union` smallset).
;; unions :: Ord k => [Map k a] -> Map k a
;; The union of a list of maps: (unions == foldl union empty).
;; unionsWith :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a
;; The union of a list of maps, with a combining operation: (unionsWith f == foldl (unionWith f) empty).

;; Difference
;; difference :: Ord k => Map k a -> Map k b -> Map k a
;; O(n+m). Difference of two maps. Return elements of the first map not existing in the second map. The implementation uses an efficient hedge algorithm comparable with hedge-union.
;; differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
;; O(n+m). Difference with a combining function. When two equal keys are encountered, the combining function is applied to the values of these keys. If it returns Nothing, the element is discarded (proper set difference). If it returns (Just y), the element is updated with a new value y. The implementation uses an efficient hedge algorithm comparable with hedge-union.
;; differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
;; O(n+m). Difference with a combining function. When two equal keys are encountered, the combining function is applied to the key and both values. If it returns Nothing, the element is discarded (proper set difference). If it returns (Just y), the element is updated with a new value y. The implementation uses an efficient hedge algorithm comparable with hedge-union.

;; Intersection
;; intersection :: Ord k => Map k a -> Map k b -> Map k a
;; O(n+m). Intersection of two maps. Return data in the first map for the keys existing in both maps. (intersection m1 m2 == intersectionWith const m1 m2).
;; intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
;; O(n+m). Intersection with a combining function. Intersection is more efficient on (bigset `intersection` smallset).

;; Traversal
;; Map
;; map :: (a -> b) -> Map k a -> Map k b
;; O(n). Map a function over all values in the map.
;; mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
;; O(n). Map a function over all values in the map.
;;  let f key x = (show key) ++ ":" ++ x
;;  mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
;; mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
;; O(n). The function mapAccum threads an accumulating argument through the map in ascending order of keys.
;; mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
;; O(n). The function mapAccumWithKey threads an accumulating argument through the map in ascending order of keys.
;; mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
;; O(n). The function mapAccumR threads an accumulating argument through the map in descending order of keys.
;; mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
;; O(n*log n). mapKeys f s is the map obtained by applying f to each key of s.
;; The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the value at the smallest of these keys is retained.
;; mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
;; O(n*log n). mapKeysWith c f s is the map obtained by applying f to each key of s.
;; The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the associated values will be combined using c.
;; mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
;; O(n). mapKeysMonotonic f s == mapKeys f s, but works only when f is strictly monotonic. That is, for any values x and y, if x < y then f x < f y. The precondition is not checked. Semi-formally, we have:

;; Fold

;; fold :: (a -> b -> b) -> b -> Map k a -> b
;; O(n). Fold the values in the map, such that fold f z == foldr f z . elems. For example,
;; foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
;; O(n). Fold the keys and values in the map, such that foldWithKey f z == foldr (uncurry f) z . toAscList. For example,
;; foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
;; O(n). Post-order fold. The function will be applied from the lowest value to the highest.
;; elems :: Map k a -> [a]
;; O(n). Return all elements of the map in the ascending order of their keys.
;; keys :: Map k a -> [k]
;; O(n). Return all keys of the map in ascending order.
;; keysSet :: Map k a -> Set k
;; O(n). The set of all keys of the map.
;; assocs :: Map k a -> [(k, a)]
;; O(n). Return all key/value pairs in the map in ascending key order.

;; Lists
;; toList :: Map k a -> [(k, a)]
;; O(n). Convert to a list of key/value pairs.
;; fromList :: Ord k => [(k, a)] -> Map k a
;; O(n*log n). Build a map from a list of key/value pairs. See also fromAscList. If the list contains more than one value for the same key, the last value for the key is retained.
;; fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
;; O(n*log n). Build a map from a list of key/value pairs with a combining function. See also fromAscListWith.
;; fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
;; O(n*log n). Build a map from a list of key/value pairs with a combining function. See also fromAscListWithKey.

;; Ordered lists
;; toAscList :: Map k a -> [(k, a)]
;; O(n). Convert to an ascending list.
;;  toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
;; toDescList :: Map k a -> [(k, a)]
;; O(n). Convert to a descending list.
;; fromAscList :: Eq k => [(k, a)] -> Map k a
;; O(n). Build a map from an ascending list in linear time. The precondition (input list is ascending) is not checked.
;; fromAscListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> Map k a
;; O(n). Build a map from an ascending list in linear time with a combining function for equal keys. The precondition (input list is ascending) is not checked.
;; fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
;; O(n). Build a map from an ascending list in linear time with a combining function for equal keys. The precondition (input list is ascending) is not checked.
;; fromDistinctAscList :: [(k, a)] -> Map k a
;; O(n). Build a map from an ascending list of distinct elements in linear time. The precondition is not checked.

;; Filter
;; filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
;; O(n). Filter all values that satisfy the predicate.
;; filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
;; O(n). Filter all keys/values that satisfy the predicate.
;; partition :: Ord k => (a -> Bool) -> Map k a -> (Map k a, Map k a)
;; O(n). Partition the map according to a predicate. The first map contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also split.
;; partitionWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
;; O(n). Partition the map according to a predicate. The first map contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also split.
;; mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
;; O(n). Map values and collect the Just results.
;; mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> Map k a -> Map k b
;; O(n). Map keys/values and collect the Just results.
;; mapEither :: Ord k => (a -> Either b c) -> Map k a -> (Map k b, Map k c)
;; O(n). Map values and separate the Left and Right results.
;; mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
;; O(n). Map keys/values and separate the Left and Right results.
;; split :: Ord k => k -> Map k a -> (Map k a, Map k a)
;; O(log n). The expression (split k map) is a pair (map1,map2) where the keys in map1 are smaller than k and the keys in map2 larger than k. Any key equal to k is found in neither map1 nor map2.
;; splitLookup :: Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
;; O(log n). The expression (splitLookup k map) splits a map just like split but also returns lookup k map.

;; Submap
;; isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
;; O(n+m). This function is defined as (isSubmapOf = isSubmapOfBy (==)).
;; isSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
;; O(n+m). The expression (isSubmapOfBy f t1 t2) returns True if all keys in t1 are in tree t2, and when f returns True when applied to their respective values. For example, the following expressions are all True:

;; Indexed
;; lookupIndex :: Ord k => k -> Map k a -> Maybe Int
;; O(log n). Lookup the index of a key. The index is a number from 0 up to, but not including, the size of the map.
;; findIndex :: Ord k => k -> Map k a -> Int
;; O(log n). Return the index of a key. The index is a number from 0 up to, but not including, the size of the map. Calls error when the key is not a member of the map.
;; elemAt :: Int -> Map k a -> (k, a)
;; O(log n). Retrieve an element by index. Calls error when an invalid index is used.
;; updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
;; O(log n). Update the element at index. Calls error when an invalid index is used.
;; deleteAt :: Int -> Map k a -> Map k a
;; O(log n). Delete the element at index. Defined as (deleteAt i map = updateAt (k x -> Nothing) i map).

;; Min/Max
;; findMin :: Map k a -> (k, a)
;; O(log n). The minimal key of the map. Calls error is the map is empty.
;; findMax :: Map k a -> (k, a)
;; O(log n). The maximal key of the map. Calls error is the map is empty.
;; deleteMin :: Map k a -> Map k a
;; O(log n). Delete the minimal key. Returns an empty map if the map is empty.
;; deleteMax :: Map k a -> Map k a
;; O(log n). Delete the maximal key. Returns an empty map if the map is empty.
;; deleteFindMin :: Map k a -> ((k, a), Map k a)
;; O(log n). Delete and find the minimal element.
;; deleteFindMax :: Map k a -> ((k, a), Map k a)
;; O(log n). Delete and find the maximal element.
;; updateMin :: (a -> Maybe a) -> Map k a -> Map k a
;; O(log n). Update the value at the minimal key.
;; updateMax :: (a -> Maybe a) -> Map k a -> Map k a
;; O(log n). Update the value at the maximal key.
;; updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
;; O(log n). Update the value at the minimal key.
;; updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
;; O(log n). Update the value at the maximal key.
;; minView :: Map k a -> Maybe (a, Map k a)
;; O(log n). Retrieves the value associated with minimal key of the map, and the map stripped of that element, or Nothing if passed an empty map.
;; maxView :: Map k a -> Maybe (a, Map k a)
;; O(log n). Retrieves the value associated with maximal key of the map, and the map stripped of that element, or Nothing if passed an
;; minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
;; O(log n). Retrieves the minimal (key,value) pair of the map, and the map stripped of that element, or Nothing if passed an empty map.
;; maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
;; O(log n). Retrieves the maximal (key,value) pair of the map, and the map stripped of that element, or Nothing if passed an empty map.

;; Debugging
;; showTree :: (Show k, Show a) => Map k a -> String
;; O(n). Show the tree that implements the map. The tree is shown in a compressed, hanging format. See showTreeWith.
;; showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String
;; O(n). The expression (showTreeWith showelem hang wide map) shows the tree that implements the map. Elements are shown using the showElem function. If hang is True, a hanging tree is shown otherwise a rotated tree is shown. If wide is True, an extra wide version is shown.
;; valid :: Ord k => Map k a -> Bool
;; O(n). Test if the internal map structure is valid.
