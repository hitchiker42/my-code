struct BinaryHeap<T: Ord> {
    heap: Vec<T>,
}
impl<T: Ord> BinaryHeap<T> {
    macro_rules! left_child(i) (2*i+1);
    macro_rules! right_child(i) (2*i+2);
    macro_rules! parent(i) ((i-1)/2);
    fn sift_down(&self, root: usize, end: usize){
        assert!(root < end && end <= self.heap.len());
        let (mut left, mut right, mut swap): (usize, usize, usize);
        while(l = left_child!(root) < end){
            unsafe {
                right = right_child(root);
                if self.heap.get_unchecked(
