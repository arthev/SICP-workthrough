;For a table implementation, the search part of both lookup and insertion
;has to be carried out through ordered comparison of the keys. So at any level,
;movement down the tree will be decided by key comparison of a key. Once a key is reached,
;then either it's time to insert a record, or it's time to go for the next-dimensional key,
;by continuing down from the first key's children. Because of a tree's node structure,
;adding the ability to use a given keychain both as an N-dimensional key and as a subchain
;for an M-dimensional key, is extra easy to implement by opening a slot for records on the node
;structures themselves. Eg. store records both in inner nodes and leaves. Also a possibility
;of implementing dimensions by storing a different tree as the value in any given key, thus having all records
;be in leaf nodes - but either as values or tables. Or both, to utilise as both N-key and M-subkey.
;
;While the table functions would be different, they would also be mostly the same, except
;for helper traversal functions in addition.
