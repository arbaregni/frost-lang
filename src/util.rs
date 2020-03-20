/// An iterator over consecutive tokens in a stream
pub struct Tuples<I> where I: Iterator {
    iter: I
}
impl <I> Tuples<I> where I: Iterator {
    pub fn new(iter: I) -> Tuples<I> {
        Tuples{iter}
    }
}
impl <I> Iterator for Tuples<I>
    where I: Iterator
{
    type Item = (I::Item, I::Item);
    fn next(&mut self) -> Option<Self::Item> {
        Some((self.iter.next()?, self.iter.next()?))
    }
}

/// Estimate the edge count for a certain number of nodes
/// Using the worst-case scenario
pub fn estimate_edge_count(nodes: usize) -> usize {
    // TODO: use better estimation
    (nodes * nodes - nodes) / 2
}