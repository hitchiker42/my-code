struct Array2D<T> {
    mem: Vec<T>,
    rows: isize,
    cols: isize,
}
impl<T> for Array2D<T>
