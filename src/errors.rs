use std::ops::Range;

use miette::Diagnostic;
use miette::SourceSpan;
use thiserror::Error;

pub type Result<T, E = PanicLangError> = core::result::Result<T, E>;

#[derive(Error, Diagnostic, Debug)]
pub enum PanicLangError {
    #[error(transparent)]
    #[diagnostic(code(panic_lang::io_error))]
    IoError(#[from] std::io::Error),

    #[error(transparent)]
    #[diagnostic(transparent)]
    LexerError(#[from] LexerError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    SeveralErrors(#[from] SeveralErrors),
}

#[derive(Error, Diagnostic, Debug)]
#[error("lexer error")]
pub struct LexerError {
    #[label("unrecognized token")]
    pub at: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[error("several errors")]
pub struct SeveralErrors {
    #[related]
    pub errors: Vec<PanicLangError>,
}

pub fn to_span(range: &Range<usize>) -> SourceSpan {
    SourceSpan::new(range.start.into(), range.end - range.start)
}
