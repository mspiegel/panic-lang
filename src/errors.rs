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
    ParserErrorExpectedToken(#[from] ParserErrorExpectedToken),

    #[error("unexpected end of file (EOF)")]
    #[diagnostic(code(panic_lang::unexpected_eof))]
    ParserErrorUnexpectedEOF,

    #[error(transparent)]
    #[diagnostic(transparent)]
    SeveralErrors(#[from] SeveralErrors),
}

#[derive(Error, Diagnostic, Debug)]
#[error("lexer error")]
pub struct LexerError {
    #[label("here")]
    pub at: SourceSpan,
}

#[derive(Error, Diagnostic, Debug)]
#[error("expected {expected:?}")]
pub struct ParserErrorExpectedToken {
    #[label("here")]
    pub at: SourceSpan,
    pub expected: &'static str,
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
