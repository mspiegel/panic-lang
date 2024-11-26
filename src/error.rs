use crate::parser::peg_grammar::Rule;

#[allow(clippy::large_enum_variant)]
#[derive(Debug, thiserror::Error, thiserror_ext::Box)]
#[thiserror_ext(newtype(name = PanicLangError))]
pub enum PanicErrorImpl {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("parser error")]
    ParserError(#[from] pest::error::Error<Rule>),
    #[error("syntax tree error")]
    SyntaxTreeError(String),
    #[error("evaluation error")]
    EvaluationError(String),
    #[error("unknown error")]
    Unknown,
}

impl<T> From<PanicErrorImpl> for Result<T, PanicLangError> {
    fn from(val: PanicErrorImpl) -> Self {
        Err(val.into())
    }
}
