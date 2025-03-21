use std::rc::Rc;

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
pub struct Temp(Rc<str>);

impl From<&str> for Temp {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<String> for Temp {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<Temp> for String {
    fn from(value: Temp) -> Self {
        value.0.to_string()
    }
}

impl From<Temp> for Rc<str> {
    fn from(value: Temp) -> Self {
        value.0
    }
}

impl From<Rc<str>> for Temp {
    fn from(value: Rc<str>) -> Self {
        Self(value)
    }
}

impl std::fmt::Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// `Label`s are meant to be created once,
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Label(Rc<str>);

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct TempFactory {
    temps_used: usize,
    labels_used: usize,
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
        Temp(format!("t{}", self.temps_used).into())
    }

    pub fn make_label(&mut self) -> Label {
        self.labels_used += 1;
        Label(format!("l{}", self.labels_used).into())
    }
}
