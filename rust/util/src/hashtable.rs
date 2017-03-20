//This is a threadsafe hashtable implementation, using a rwlock to allow
//concurrent readers. I'll hopefully eventually update it to use atomic operations
//for writing allowing concurrent writers as well

//Create a hashtable containing the given key/value pairs, works like hash
//literals in perl, i.e the key is a string, but can be given unquoted if
//it wouldn't be ambigious.
//ex. hashtable!(a => 1, b => 2, c => 3) == hashtable!("a" => 1, "b" => 2, "c" => 3)
macro_rules! hashtable {
    //We do this recursively to allow mixing idenifiers and strings as keys
    ($ht: ident) => {$ht};
    ($i: ident => $e: expr $(,$tt: tt)*) => {{
        let mut ht = Hashtable.new();
        hashtable!(ht, $i => $e, $(,$tt)*)
    }};
    ($i: expr => $e: expr $(,$tt: tt)*) => {{
        let mut ht = Hashtable.new();
        hashtable!(ht, $i => $e, $(,$tt)*)
    }};
    ($ht: ident, $i: ident => $e: expr, $(,$tt: tt)*) => {{
        $ht.add(stringify!($i), $e);
        hashtable!($ht, $($tt: tt),*)
    }};
    ($ht: ident, $i: expr => $e: expr, $(,$tt: tt)*) => {{
        $ht.add($i, $e);
        hashtable!($ht, $($tt: tt),*)
    }};
}

//I'd Really rather not have to constrain the
struct HashEntry<K: Eq + Hash, V> {
    key: K,
    value: V,
    hv: u64,
    next: *mut HashEntry<K,V>,
}
    
struct Hashtable<K: Eq + Hash, V> {
    table: RwLock<Vec<HashEntry<K,V>>>,
}
