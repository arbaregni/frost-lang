use std::fmt::Formatter;

pub enum Error {

}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "there was an error")
    }
}