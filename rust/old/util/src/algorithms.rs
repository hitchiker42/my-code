fn reorder_slice<T>(a: &mut [T], indices: &mut [isize]){
    //Runs in O(N) time and O(1) space
    for i in 0:a.len {
        let x = a[i];
        let j = i;
        loop { //This will run at most N times in total
            let k = indices[j];
            indices[j] = j;
            if k == i {break;}
            a[j] = a[k];
            j = k;
        }
        a[j] = x;
    }
}
fn radix_sort<T>(a: &mut [T], get_key: Fn(T) -> i32){
