use std::fmt::Formatter;

pub enum ErrorKind {
    TypeInference,
}
pub struct Error {
    reason: String,
    kind: ErrorKind,
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.reason)
    }
}
impl Error {
    pub fn inference(reason: String) -> Error {
        Error {
            reason,
            kind: ErrorKind::TypeInference,
        }
    }
}
impl <T> std::convert::Into<Result<T, Error>> for Error {
    fn into(self) -> Result<T, Error> {
        Err(self)
    }
}
