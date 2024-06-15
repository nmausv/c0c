#![allow(dead_code)]
use std::collections::{HashMap, HashSet};

/// Simple regular expression library for the c0c lexer
///
/// The RegExp must be built by hand.
///
/// Used as keys for a HashMap when converting to an NFA, so implements `Eq`, `Hash`, etc.
#[derive(Eq, PartialEq, PartialOrd, Ord, Hash, Clone, Debug)]
enum RegExp {
    /// `Single(c)` == `c`
    Single(char),
    /// `Range(a,z)` == `[a-z]`
    Range(char, char),
    /// `Empty` matches empty string
    Empty,
    /// `Or(p1, p2)` == `p1 | p2`
    Or(Box<RegExp>, Box<RegExp>),
    /// `Split(p1, p2)` == `p1p2`
    Split(Box<RegExp>, Box<RegExp>),
    /// `Star(p1)` == `p1*`
    Star(Box<RegExp>),
}

/// The Nondeterministic Finite Automaton type
///
/// The alphabet is implicitly the set of all `char`s, and the state set is a set of `usize`s.
/// The initial state will always be 0.
///
/// The type parameter `T` indicates the keys used for the transition functions.
/// After the initial processing, `T` will always be `Option<char>`.
#[derive(Debug)]
struct NFA<T> {
    /// Set of states = usizes
    ///
    /// List of states
    ///
    /// For a given state `s`, `states[s] = None` if the state is not defined for this NFA.
    /// Moreover, `states[s] = Some(true)` if and only if the state `s` is an accepting state.
    states: Vec<Option<bool>>,
    /// Transition functions
    ///
    /// Indexed by current state and the parameter type T.
    /// Almost every function will require the type T = Option<char>,
    /// but for building the NFA we use T = RegExp.
    transitions: HashMap<(usize, T), HashSet<usize>>,
}

impl<'a> NFA<&'a RegExp> {
    fn insert_transition<'b: 'a>(&mut self, start: usize, r: &'b RegExp, end: usize) {
        let r_key = (start, r);
        let r_entry = self.transitions.entry(r_key);
        r_entry
            .and_modify(|ends| {
                ends.insert(end);
            })
            .or_insert(HashSet::from([end]));
    }

    fn remove_regex(mut self) -> NFA<Option<char>> {
        // states stay the same, just iterate over every transition and switch from RegExp to
        // possibly many transitions from chars

        let mut char_nfa: NFA<Option<char>> = NFA {
            states: self.states,
            transitions: HashMap::new()
        };

        for ((start, pat), ends) in self.transitions.drain() {
            match pat {
                RegExp::Empty => {
                    char_nfa.transitions.insert((start, None), ends);
                },
                RegExp::Single(c) => {
                    char_nfa.transitions.insert((start, Some(*c)), ends.clone());
                },
                RegExp::Range(c1, c2) => {
                    for c in *c1..*c2 {
                        char_nfa.transitions.insert((start, Some(c)), ends.clone());
                    }
                },
                _ => { panic!("Found complex RegExp when in remove_regex!") }
            }
        }

        char_nfa
    }
}

impl NFA<Option<char>> {
    /// Convert a regular expression into a NFA
    ///
    /// Procedure adapted from
    /// [https://www.cs.cmu.edu/~janh/courses/411/24/lectures/09-lex.pdf]
    fn from_regex(pat: RegExp) -> Self {
        let mut nfa: NFA<&RegExp> = NFA {
            states: Vec::new(),
            transitions: HashMap::new(),
        };
        // initialize with initial -> final via pat
        // initial state at 0
        nfa.states.push(Some(false));
        // final state at 1
        nfa.states.push(Some(true));
        // transition
        nfa.transitions.insert((0, &pat), HashSet::from([1]));

        // keep worklist of transition labels to decompose
        let mut worklist: Vec<(usize, &RegExp, usize)> = vec![(0, &pat, 1)];

        while !worklist.is_empty() {
            let (start, pat, end) = worklist.pop().unwrap();

            match pat {
                RegExp::Or(r, s) => {
                    nfa.insert_transition(start, r, end);
                    nfa.insert_transition(start, s, end);

                    // add new transitions to worklist
                    worklist.push((start, r, end));
                    worklist.push((start, s, end));

                    // remove old transition
                    nfa.transitions.remove(&(start, pat));
                }
                RegExp::Split(r, s) => {
                    // make new intermediate state
                    let mid = nfa.states.len();
                    nfa.states.push(Some(false));

                    nfa.insert_transition(start, r, mid);
                    nfa.insert_transition(mid, s, end);

                    // add new transitions to worklist
                    worklist.push((start, r, end));
                    worklist.push((start, s, end));

                    // remove old transition
                    nfa.transitions.remove(&(start, pat));
                }
                RegExp::Star(r) => {
                    // need two intermediate states
                    let q1 = nfa.states.len();
                    nfa.states.push(Some(false));
                    let p1 = nfa.states.len();
                    nfa.states.push(Some(false));

                    nfa.insert_transition(start, &RegExp::Empty, end);
                    nfa.insert_transition(start, &RegExp::Empty, q1);
                    nfa.insert_transition(q1, r, p1);
                    nfa.insert_transition(p1, &RegExp::Empty, q1);
                    nfa.insert_transition(p1, &RegExp::Empty, end);

                    // add transitions to worklist (skip Empty transitions)
                    worklist.push((q1, r, p1));

                    // remove old transition
                    nfa.transitions.remove(&(start, r));
                }
                _ => {}
            }
        }

        // when the worklist is done, every RegExp transition is either single, range, or empty
        // map those into transitions labelled by characters

        nfa.remove_regex()
    }

    /// Match the NFA against a string
    fn matches_against(&self, s: &str) -> bool {
        let mut states = HashSet::from([0 as usize]);

        for c in s.chars() {
            for state in states.drain() {
                todo!();
            }
        }

        todo!()
    }
}
