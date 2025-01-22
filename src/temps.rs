use crate::frontend::ast::Ident;

/// `Temp`s are meant to be created once,
/// and possibly copied many times,
/// but specifically they
/// should not be modified.
///
/// Right now they own their data,
/// but this is not strictly necessary
/// and is a reasonable target
/// for optimization.
///
/// As such, they only wrap `String`,
/// with From implemented for conversions,
/// but they should not be thought of as
/// modifiable like `String`.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Temp(String);

impl From<Temp> for Ident {
    fn from(value: Temp) -> Self {
        value.0
    }
}

impl From<String> for Temp {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl std::fmt::Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// `Label`s are meant to be created once,
/// and copied many times,
/// but specifically they
/// should not be modified.
///
/// Right now they own their data,
/// but this is not strictly necessary
/// and is a reasonable target
/// for optimization.
///
/// As such, they only wrap `String`,
/// with `From` implemented for conversions,
/// but they should not be thought of as
/// modifiable like `String`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Label(String);

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct TempFactory {
    temps_used: u32,
    labels_used: u32,
}

impl TempFactory {
    pub fn new() -> Self {
        TempFactory {
            temps_used: 0,
            labels_used: 0,
        }
    }

    pub fn make_temp(&mut self) -> Temp {
        self.temps_used += 1;
        Temp(format!("t{}", self.temps_used))
    }

    pub fn make_label(&mut self) -> Label {
        self.labels_used += 1;
        Label(format!("l{}", self.labels_used))
    }
}
