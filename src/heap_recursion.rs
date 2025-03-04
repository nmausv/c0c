#[derive(Debug)]
pub enum FrameProgress<T> {
    New,
    InProgress,
    Done(T),
}
