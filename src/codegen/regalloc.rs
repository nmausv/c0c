use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use crate::{
    codegen::abstract_assembly::{Destination, Instruction, Operand, Register},
    frontend::ast::Binop,
};

const DEBUG: bool = false;

impl PartialEq<Operand> for Destination {
    fn eq(&self, other: &Operand) -> bool {
        match (self, other) {
            (Destination::Temp(t1), Operand::Temp(t2)) => t1 == t2,
            (Destination::Register(r1), Operand::Register(r2)) => r1 == r2,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct Annotation<'a> {
    instr: &'a Instruction,
    live_in: HashSet<Destination>,
    live_out: HashSet<Destination>,
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

        write!(f, "{:<24} : {:>24} : {:>24}", line_str, live_in, live_out)
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
        writeln!(f, "{:<24} : {:>24} : {:>24}", "Line", "Live In", "Live Out")?;
        for line in &self.data {
            writeln!(f, "{line}")?;
        }

        Ok(())
    }
}

impl<'a> AnnotatedProgram<'a> {
    fn new(program: &'a [Instruction]) -> Self {
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
}

impl Instruction {
    fn uses(&self) -> HashSet<Destination> {
        match self {
            Instruction::Move {
                d: _,
                s: Operand::IntConst(_),
            } => [].into(),
            Instruction::Move { d: _, s } => {
                let s: Destination = s
                    .clone()
                    .try_into()
                    .expect("should not be IntConst by match guard");
                [s].into()
            }
            Instruction::Binop { s1, s2, op, .. } => {
                let mut uses = HashSet::new();

                match op {
                    Binop::Divide | Binop::Modulo => {
                        uses.insert(Register::Return.into());
                        uses.insert(Register::Remainder.into());
                    }
                    _ => (),
                }
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
            Instruction::Unop { s, .. } => {
                let s: Destination = s.clone().try_into().expect("");
                [s].into()
            }
            Instruction::Return => [Register::Return.into()].into(),
            Instruction::If { .. } => todo!(),
            Instruction::Goto(..) => todo!(),
            Instruction::Label(..) => todo!(),
        }
    }

    fn defines(&self) -> HashSet<Destination> {
        match self {
            Instruction::Move { d, .. }
            | Instruction::Binop { d, .. }
            | Instruction::Unop { d, .. } => [d.clone()].into(),
            Instruction::Return => [].into(),
            Instruction::If { .. } => todo!(),
            Instruction::Goto(..) => todo!(),
            Instruction::Label(..) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
struct InterferenceGraph {
    neighbors: HashMap<Destination, HashSet<Destination>>,
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
    fn insert(&mut self, s: &Destination, t: &Destination) {
        self.neighbors
            .entry(s.clone())
            .or_default()
            .insert(t.clone());

        self.neighbors
            .entry(t.clone())
            .or_default()
            .insert(s.clone());
    }

    // creates a graph from an annotated program
    fn new(program: &AnnotatedProgram) -> Self {
        let mut graph = Self::empty();

        // all precolored nodes interfere with each other, ie all registers
        graph.insert(&Register::Return.into(), &Register::Remainder.into());

        for annot in program.iter() {
            match annot.instr {
                Instruction::Move { d, s } => {
                    let d: Destination = d.clone();
                    for var in &annot.live_out {
                        if d != *var && *var != *s {
                            graph.insert(&d, var);
                        }
                    }
                }
                Instruction::Binop { d, .. } => {
                    let d: Destination = d.clone();
                    for var in &annot.live_out {
                        if d != *var {
                            graph.insert(&d, var);
                        }
                    }
                }
                Instruction::Unop { d, .. } => {
                    let d: Destination = d.clone();
                    for var in &annot.live_out {
                        if d != *var {
                            graph.insert(&d, var);
                        }
                    }
                }
                Instruction::Return => (),
                Instruction::If { .. } => todo!(),
                Instruction::Goto(..) => todo!(),
                Instruction::Label(..) => todo!(),
            }
        }

        graph
    }

    /// Maximum Cardinality Search over the given graph,
    /// returns a Simplical Elimination Ordering
    fn mcs<'a>(&'a self) -> Vec<Destination> {
        let mut ordering = Vec::new();

        // note: registers need to be added in a fixed order
        // add all registers every time???

        ordering.push(Register::Return.into());
        ordering.push(Register::Remainder.into());

        /*
        for reg in self
            .neighbors
            .keys()
            .filter(|var| matches!(var, Variable::Register(_)))
        {
            ordering.push(reg);
        }
        */

        let mut remaining: HashSet<&Destination> = self
            .neighbors
            .keys()
            .filter(|var| matches!(var, Destination::Temp(_)))
            .collect();

        let mut weights: HashMap<&Destination, usize> =
            self.neighbors.keys().map(|var| (var, 0)).collect();

        while !remaining.is_empty() {
            // find node v of maximal weight in remaining, unless
            // a precolored node exists
            let &v = {
                remaining
                    .iter()
                    .max_by_key(|&&var| weights[var])
                    .expect("weights should not be empty by loop guard")
            };

            // put v next in the ordering
            ordering.push(v.clone());

            // for every neighbor of v still in the remaining set, increment the weight
            let neighbors = &self.neighbors[v];

            for neighbor in neighbors {
                weights.entry(neighbor).and_modify(|weight| {
                    *weight += 1;
                });
            }

            // remove v from remaining
            remaining.remove(v);
        }

        ordering
    }

    fn greedy_color(
        &self,
        order: &[Destination],
    ) -> HashMap<Destination, usize> {
        let mut color_map = HashMap::new();
        for var in order {
            let neighbors = &self.neighbors[var];

            // let c be lowest color not used in neighbors(var)
            let color_set: HashSet<usize> = neighbors
                .iter()
                .filter_map(|neighbor| color_map.get(neighbor).copied())
                .collect();
            let c = mex(&color_set);

            // color var with c
            color_map.insert(var.clone(), c);
        }

        color_map
    }
}

fn mex(set: &HashSet<usize>) -> usize {
    let mut n = 0;
    loop {
        if !set.contains(&n) {
            return n;
        }
        n += 1;
    }
}

#[derive(Debug)]
pub struct Coloring {
    color_map: HashMap<Destination, usize>,
    colors_used: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct StackLocation {
    /// offset from the stack pointer
    pub index: usize,
    /// in bytes
    pub size: usize,
}

impl std::cmp::PartialEq for StackLocation {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl std::cmp::Eq for StackLocation {}

impl std::cmp::PartialOrd for StackLocation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for StackLocation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.index < other.index {
            std::cmp::Ordering::Less
        } else if self.index == other.index {
            std::cmp::Ordering::Equal
        } else {
            std::cmp::Ordering::Greater
        }
    }
}

fn spill<'a>(variables: Vec<&'a Destination>) -> MemoryMap<'a> {
    let mut spills = HashMap::new();
    let mut next_stack_index = 0;
    for var in variables {
        // don't put the same destination in different memory locations
        if spills.contains_key(var) {
            continue;
        }

        match var {
            Destination::Register(reg) => {
                spills.insert(var, MemoryLocation::Register(*reg));
            }
            Destination::Temp(_) => {
                //TODO
                // since all variables are 4 bytes right now, can hard code the size
                let loc = StackLocation {
                    index: next_stack_index,
                    size: 4,
                };
                next_stack_index += loc.size;
                spills.insert(var, MemoryLocation::Stack(loc));
            }
        }
    }

    MemoryMap {
        map: spills,
        stack_space: next_stack_index,
    }
}

#[derive(Debug, Copy, Clone)]
pub enum MemoryLocation {
    Stack(StackLocation),
    Register(Register),
}

impl std::fmt::Display for MemoryLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryLocation::Stack(stack) => {
                write!(f, "stack slot {}, size {}", stack.index, stack.size)
            }
            MemoryLocation::Register(reg) => {
                write!(f, "{reg}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemoryMap<'a> {
    pub map: HashMap<&'a Destination, MemoryLocation>,
    pub stack_space: usize,
}

impl<'a> std::fmt::Display for MemoryMap<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "stack space used: {}", self.stack_space)?;

        for (var, loc) in &self.map {
            writeln!(f, "variable {var} lives in {loc}")?;
        }

        writeln!(f)?;

        writeln!(f, "stack diagram")?;

        let mut stack: Vec<(&Destination, StackLocation)> = self
            .map
            .iter()
            .filter_map(|(var, loc)| match loc {
                MemoryLocation::Stack(stack_loc) => Some((*var, *stack_loc)),
                _ => None,
            })
            .collect();

        stack.sort_by(|&(_, loc1), &(_, loc2)| loc1.cmp(&loc2));

        for (var, loc) in stack {
            writeln!(f, "{:<8} : {:>8}", loc.index, var)?
        }

        Ok(())
    }
}

pub fn regalloc(program: &[Instruction], optimization: usize) -> MemoryMap {
    if optimization == 0 {
        let variables: Vec<_> = program
            .iter()
            .filter_map(|i| match i {
                Instruction::Move { d, .. } => Some(d),
                Instruction::Binop { d, .. } => Some(d),
                Instruction::Unop { d, .. } => Some(d),
                Instruction::Return => None,
                Instruction::If { .. } => None,
                Instruction::Goto(..) => None,
                Instruction::Label(..) => None,
            })
            .collect();

        return spill(variables);
    }

    todo!("intelligent register allocation");

    #[allow(unreachable_code)]
    let annotated = AnnotatedProgram::new(program);
    if DEBUG {
        eprintln!("annotated program:\n{annotated}");
    }

    let graph = InterferenceGraph::new(&annotated);
    if DEBUG {
        eprintln!("interference graph:\n{graph}");
    }

    let seo = graph.mcs();
    if DEBUG {
        eprintln!(
            "seo: \n{}",
            seo.iter().fold(String::new(), |acc, arg| format!(
                "{acc}{}, ",
                &arg.to_string()
            ))
        );
    }

    let color_map = graph.greedy_color(&seo);
    let colors_used = *color_map
        .values()
        .max()
        .expect("should use at least one color")
        + 1;
    if DEBUG {
        eprintln!("coloring: \n{color_map:?}");
        eprintln!("colors used: {colors_used}");
    }
    // can't actually perform the replacement yet, delay until we know which assembly language
    // we're targeting
    todo!()
}

#[cfg(test)]
mod tests {
    use super::regalloc;

    use crate::codegen::abstract_assembly::{
        Destination, Instruction, Operand, Register,
    };
    use crate::frontend::ast::Binop;

    macro_rules! instr {
        (ret) => {{
            Instruction::Return
        }};
        (reg ret <- int $src: expr ) => {{
            Instruction::Move {
                d: Destination::Register(Register::Return),
                s: Operand::IntConst($src),
            }
        }};
        (reg rem <- int $src: expr ) => {{
            Instruction::Move {
                d: Destination::Register(Register::Remainder),
                s: Operand::IntConst($src),
            }
        }};
        (var $dest: tt <- int $src: expr) => {{
            Instruction::Move {
                d: Destination::Temp($dest.into()),
                s: Operand::IntConst($src),
            }
        }};
        (reg ret <- reg ret) => {{
            Instruction::Move {
                d: Destination::Register(Register::Return),
                s: Register(Register::Return),
            }
        }};
        (reg ret <- var $src: expr) => {{
            Instruction::Move {
                d: Destination::Register(Register::Return),
                s: Operand::Temp($src.into()),
            }
        }};
        (var $dest: tt <- var $src: expr) => {{
            Instruction::Move {
                d: Destination::Temp($dest.into()),
                s: Operand::Temp($src.into()),
            }
        }};
        (var $dest: tt <- var $lhs: tt + var $rhs: tt) => {{
            Instruction::Binop {
                d: Destination::Temp($dest.into()),
                s1: Operand::Temp($lhs.into()),
                op: Binop::Plus,
                s2: Operand::Temp($rhs.into()),
            }
        }};
        (var $dest: tt <- var $lhs: tt / var $rhs: tt) => {{
            Instruction::Binop {
                d: Destination::Temp($dest.into()),
                s1: Operand::Temp($lhs.into()),
                op: Binop::Divide,
                s2: Operand::Temp($rhs.into()),
            }
        }};
    }

    #[test]
    fn example() {
        let program = vec![
            instr!(var "x1" <- int 0),
            instr!(var "x2" <- int 1),
            instr!(var "x3" <- var "x1" + var "x2"),
            instr!(var "x4" <- var "x2" + var "x3"),
            instr!(var "x5" <- var "x3" + var "x4"),
            instr!(reg ret <- var "x5"),
            instr!(ret),
        ];

        regalloc(&program, 1);
        /*
        let coloring = regalloc(&program);
        dbg!(&coloring);
        assert!(coloring.colors_used == 2);
        */
    }

    #[test]
    fn division() {
        let program = vec![
            instr!(reg ret <- int 4),
            instr!(var "x1" <- int 9),
            instr!(var "x2" <- int 27),
            instr!(var "x3" <- var "x2" / var "x1"),
            instr!(ret),
        ];

        regalloc(&program, 1);
        panic!();
    }
}
