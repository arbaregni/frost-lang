use crate::parse::{Rule, Span};
use std::fmt::Formatter;
use crate::type_inference::Type;
use crate::scope::ScopeId;

#[derive(Debug)]
pub enum ErrorKind {
    Parsing, // handled by pest
    TypeMismatch{expected_type: Type, actual_type: Type},
    SelfReferentialType{type_: Type},
    UndefinedSymbol{name: String, scope_id: ScopeId},
}
impl ErrorKind {
    pub fn into_error(self) -> Error {
        Error{
            kind: self,
            context_text: String::new(),
            maybe_span: None,
            debug_tag: "",
        }
    }
    pub fn into_result<T>(self) -> Result<T, Error> {
        Err(self.into())
    }
}
pub struct Error {
    kind: ErrorKind,
    context_text: String,
    maybe_span: Option<Span>,
    debug_tag: &'static str,
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if !self.debug_tag.is_empty() {
            writeln!(f, "DEBUG: {}", self.debug_tag)?;
        }
        write!(f, "{}", self.context_text)?;
        match &self.kind {
            ErrorKind::Parsing => { /* handled by pest */ Ok(()) },
            ErrorKind::TypeMismatch { ref expected_type, ref actual_type, } => {
                write!(f, "type mismatch: expected `{}` but got `{}`", expected_type, actual_type)
            },
            ErrorKind::SelfReferentialType { ref type_ } => {
                write!(f, "self referential type: `{}` contains a reference to itself", type_)
            },
            ErrorKind::UndefinedSymbol { ref name, scope_id: _, } => {
                write!(f, "undefined symbol: identifier `{}` is not defined in the current environment", name)
            },
        }
    }
}
impl Error {
    pub fn with_span(mut self, span: Span) -> Error {
        self.maybe_span = Some(span);
        self
    }
    pub fn tag(mut self, tag: &'static str) -> Error {
        self.debug_tag = tag;
        self
    }
    pub fn make_underlined(&mut self, source: &str) {
        if let Some(span) = self.maybe_span {
            self.context_text = span.underline(source);
        }
    }
}

impl std::convert::Into<Error> for ErrorKind {
    fn into(self) -> Error {
        self.into_error()
    }
}

impl std::convert::From<pest::error::Error<Rule>> for Error {
    fn from(pest_error: pest::error::Error<Rule>) -> Error {
        let mut error = ErrorKind::Parsing.into_error();
        error.context_text = format!("{}", pest_error);
        error
    }
}

pub trait ResultExtension {
    fn with_span(self, span: Span) -> Self;
    fn with_maybe_span(self, maybe_span: Option<Span>) -> Self;
    fn tag(self, tag: &'static str) -> Self;
}
impl <T> ResultExtension for Result<T, Error> {
    fn with_span(self, span: Span) -> Self {
        match self {
            Err(error) => Err(error.with_span(span)),
            _ => self
        }
    }
    fn with_maybe_span(self, maybe_span: Option<Span>) -> Self {
        match (self, maybe_span) {
            (Err(error), Some(span)) => Err(error.with_span(span)),
            (result, _) => result
        }
    }
    fn tag(self, tag: &'static str) -> Self {
        match self {
            Err(error) => Err(error.tag(tag)),
            _ => self
        }
    }
}