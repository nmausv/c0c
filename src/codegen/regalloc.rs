use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use crate::{
    codegen::abstract_assembly::{Destination, Instruction, Operand, Register},
    temps::Temp,
};

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
enum Variable {
    Temp(Temp),
    Register(Register),
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Temp(temp) => write!(f, "{temp}"),
            Variable::Register(register) => write!(f, "{register}"),
        }
    }
}

impl PartialEq<Operand> for Variable {
    fn eq(&self, other: &Operand) -> bool {
        match (self, other) {
            (Variable::Temp(t1), Operand::Temp(t2)) => t1 == t2,
            (Variable::Register(r1), Operand::Register(r2)) => r1 == r2,
            _ => false,
        }
    }
}

impl PartialEq<Destination> for Variable {
    fn eq(&self, other: &Destination) -> bool {
        match (self, other) {
            (Variable::Temp(t1), Destination::Temp(t2)) => t1 == t2,
            (Variable::Register(r1), Destination::Register(r2)) => r1 == r2,
            _ => false,
        }
    }
}

impl From<Variable> for Operand {
    fn from(value: Variable) -> Self {
        match value {
            Variable::Temp(temp) => Operand::Temp(temp),
            Variable::Register(register) => Operand::Register(register),
        }
    }
}

impl From<Register> for Variable {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl From<Temp> for Variable {
    fn from(value: Temp) -> Self {
        Self::Temp(value)
    }
}

impl From<Destination> for Variable {
    fn from(value: Destination) -> Self {
        match value {
            Destination::Register(register) => Variable::Register(register),
            Destination::Temp(temp) => Variable::Temp(temp),
        }
    }
}

impl TryFrom<Operand> for Variable {
    type Error = ();
    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        match value {
            Operand::Register(register) => Ok(Variable::Register(register)),
            Operand::IntConst(_) => Err(()),
            Operand::Temp(temp) => Ok(Variable::Temp(temp)),
        }
    }
}

#[derive(Debug)]
struct Annotation<'a> {
    instr: &'a Instruction,
    live_in: HashSet<Variable>,
    live_out: HashSet<Variable>,
}

impl<'a> std::fmt::Display for Annotation<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_str = format!("{}", self.instr);

        let mut live_in = String::new();
        for (ind, var) in self.live_in.iter().enumerate() {
            if ind + 1 < self.live_in.len() {
                live_in += &format!("{var}, ");
            } else {
                live_in += &format!("{var}");
            }
        }

        let mut live_out = String::new();
        for (ind, var) in self.live_out.iter().enumerate() {
            if ind + 1 < self.live_out.len() {
                live_out += &format!("{var}, ");
            } else {
                live_out += &format!("{var}");
            }
        }

        write!(f, "{:<16} : {:>16} : {:>16}", line_str, live_in, live_out)
    }
}

#[derive(Debug)]
struct AnnotatedProgram<'a> {
    data: Vec<Annotation<'a>>,
}

impl<'a> Deref for AnnotatedProgram<'a> {
    type Target = [Annotation<'a>];
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a> std::fmt::Display for AnnotatedProgram<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:<16} : {:>16} : {:>16}", "Line", "Live In", "Live Out")?;
        for line in &self.data {
            writeln!(f, "{line}")?;
        }

        Ok(())
    }
}

fn compute_liveness(program: &[Instruction]) -> AnnotatedProgram {
    let mut annotated: Vec<_> = Vec::with_capacity(program.len());
    let mut live_in = HashSet::new();
    let mut live_out;
    let mut uses;
    let mut defines;

    for instr in program.iter().rev() {
        uses = instr.uses();
        defines = instr.defines();
        // current live out is the previous live in
        live_out = std::mem::take(&mut live_in);

        live_in = uses
            .union(&live_out.difference(&defines).cloned().collect())
            .cloned()
            .collect();

        annotated.push(Annotation {
            instr,
            live_in: live_in.clone(),
            live_out: live_out.clone(),
        });
    }

    annotated.reverse();
    AnnotatedProgram { data: annotated }
}

impl Instruction {
    fn uses(&self) -> HashSet<Variable> {
        match self {
            Instruction::Move {
                d: _,
                s: Operand::IntConst(_),
            } => [].into(),
            Instruction::Move { d: _, s } => {
                let s: Variable = s.clone().try_into().expect("");
                [s].into()
            }
            Instruction::Binop { s1, s2, .. } => {
                let mut uses = HashSet::new();

                match s1 {
                    Operand::Register(register) => {
                        uses.insert((*register).into());
                    }
                    Operand::IntConst(_) => (),
                    Operand::Temp(temp) => {
                        uses.insert(temp.clone().into());
                    }
                };
                match s2 {
                    Operand::Register(register) => {
                        uses.insert((*register).into());
                    }
                    Operand::IntConst(_) => (),
                    Operand::Temp(temp) => {
                        uses.insert(temp.clone().into());
                    }
                };

                uses
            }
            Instruction::Unop { .. } => todo!(),
            Instruction::Return => [Register::Return.into()].into(),
            Instruction::If { .. } => todo!(),
            Instruction::Goto(..) => todo!(),
            Instruction::Label(..) => todo!(),
        }
    }

    fn defines(&self) -> HashSet<Variable> {
        match self {
            Instruction::Move { d, .. }
            | Instruction::Binop { d, .. }
            | Instruction::Unop { d, .. } => [d.clone().into()].into(),
            Instruction::Return => [].into(),
            Instruction::If { .. } => todo!(),
            Instruction::Goto(..) => todo!(),
            Instruction::Label(..) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct InterferenceGraph {
    neighbors: HashMap<Variable, HashSet<Variable>>,
}

impl std::fmt::Display for InterferenceGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut acc = String::new();
        for (node, neighbors) in &self.neighbors {
            acc += &format!("{node}:");
            for neighbor in neighbors {
                acc += &format!(" {neighbor}");
            }
            acc += "\n";
        }

        write!(f, "{acc}")
    }
}

impl InterferenceGraph {
    // creates a completely empty graph
    fn empty() -> Self {
        InterferenceGraph {
            neighbors: HashMap::new(),
        }
    }

    // inserts an edge from s to t in the graph
    fn insert(&mut self, s: &Variable, t: &Variable) {
        self.neighbors
            .entry(s.clone())
            .and_modify(|s_nbors| {
                s_nbors.insert(t.clone());
            })
            .or_insert([t.clone()].into());

        self.neighbors
            .entry(t.clone())
            .and_modify(|t_nbors| {
                t_nbors.insert(s.clone());
            })
            .or_insert([s.clone()].into());
    }

    // creates a graph from an annotated program
    fn new(program: &AnnotatedProgram) -> Self {
        let mut graph = Self::empty();

        for annot in program.iter() {
            match annot.instr {
                Instruction::Move { d, s } => {
                    let d: Variable = d.clone().into();
                    for var in &annot.live_out {
                        if d != *var && *var != *s {
                            graph.insert(&d, var);
                        }
                    }
                }
                Instruction::Binop { d, .. } => {
                    let d: Variable = d.clone().into();
                    for var in &annot.live_out {
                        if d != *var {
                            graph.insert(&d, var);
                        }
                    }
                }
                Instruction::Unop { .. } => todo!(),
                Instruction::Return => (),
                Instruction::If { .. } => todo!(),
                Instruction::Goto(..) => todo!(),
                Instruction::Label(..) => todo!(),
            }
        }

        graph
    }
}

pub fn regalloc(program: &[Instruction]) {
    let annotated = compute_liveness(program);
    eprintln!("annotated program:\n{annotated}");

    let graph = InterferenceGraph::new(&annotated);
    eprintln!("interference graph:\n{graph}");
}
