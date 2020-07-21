use crate::intern::Interned;
use crate::{Fun, Ident, SExpr, SExprKind};

use std::fmt;

#[derive(Debug)]
pub struct InterpreterError {
    pub info: InterpreterErrorInfo,
    pub callstack: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum InterpreterErrorInfo {
    CannotEvaluate(SExpr),
    UnknownName(Interned<'static, Ident>),
    UndefinedSigil(char),
    CannotCall(SExpr),
    NonMatchingArgs(Fun, Vec<SExpr>),
    CannotConvert(&'static str, SExpr),
    NotA(SExprKind, SExpr),
    ReachedTheUnreachable,
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Error: {}", self.info)?;
        for line in &self.callstack {
            writeln!(f, "{}", line)?
        }
        Ok(())
    }
}

impl fmt::Display for InterpreterErrorInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InterpreterErrorInfo::*;
        match self {
            CannotEvaluate(expr) => write!(f, "Cannot evaluate {:?}", expr),
            UnknownName(ident) => write!(f, "Unknown name {:?}", ident),
            UndefinedSigil(c) => write!(f, "Undefined sigil {:?}", c),
            CannotCall(expr) => write!(f, "Cannot call {:?}", expr),
            NonMatchingArgs(fun, args) => write!(
                f,
                "Args {:?} did not match for {:?}",
                SExpr::List(args.to_vec()),
                fun
            ),
            CannotConvert(msg, example) => {
                write!(f, "Cannot convert ({}) {:?}", msg, example)
            }
            NotA(kind, expr) => write!(f, "Not a {:?}: {:?}", kind, expr),
            ReachedTheUnreachable => write!(f, "Reached the unreachable"),
        }
    }
}
