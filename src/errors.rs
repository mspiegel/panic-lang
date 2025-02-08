use miette::Diagnostic;
use miette::SourceSpan;
use thiserror::Error;

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
    #[label("here")]
    pub at: SourceSpan,
}

#[derive(Debug, Error, Diagnostic)]
#[error("several errors")]
pub struct SeveralErrors {
    #[related]
    pub errors: Vec<PanicLangError>,
}
