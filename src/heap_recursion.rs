#[derive(Debug)]
pub enum FrameProgress<S, T> {
    New(S),
    InProgress,
    Done(T),
}
