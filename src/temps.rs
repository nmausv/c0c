use crate::frontend::elab_ast::Ident;

pub type Temp = Ident;

pub struct TempFactory {
    temps_used: u32,
}

impl TempFactory {
    pub fn new() -> Self {
        TempFactory { temps_used: 0 }
    }
    pub fn make_temp(&mut self) -> Temp {
        self.temps_used += 1;
        format!("t{}", self.temps_used)
    }
}
