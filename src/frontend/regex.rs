#![allow(dead_code)]
use std::collections::HashMap;

/// Simple regular expression library for the c0c lexer
///
/// The RegExp must be built by hand.
///
/// Used as keys for a HashMap when converting to an DFA, so implements `Eq`, `Hash`, etc.
#[derive(Eq, PartialEq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum RegExp {
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

impl RegExp {
    /// Convenience function to create a regex matching a given word.
    ///
    /// Does not support `Range`, `Or`, or `Star` constructions.
    ///
    /// Essentially, given a word `w`, this will create a `RegExp` that matches
    /// each character from `w` in order.
    pub fn from_word(w: &str) -> RegExp {
        if w.is_empty() {
            RegExp::Empty
        } else if w.len() == 1 {
            // optimization, use single without empty
            // if the word has only one character
            // unwrap guaranteed success since len > 0
            RegExp::Single(w.chars().next().unwrap())
        } else {
            // unwrap guaranteed success since len > 0
            let first_char = w.chars().next().unwrap();
            RegExp::Split(
                Box::new(RegExp::Single(first_char)),
                Box::new(RegExp::from_word(&w[1..])),
            )
        }
    }

    /// Convenience function to create a regex that matches any character
    /// in a given word.
    ///
    /// Does not support `Range`, `Or`, or `Star` constructions.
    ///
    /// Essentially, given a string of characters, this will create a `RegExp`
    /// each character from `w` in order.
    pub fn from_charlist(w: &str) -> RegExp {
        if w.is_empty() {
            RegExp::Empty
        } else if w.len() == 1 {
            RegExp::Single(w.chars().next().unwrap())
        } else {
            let first_char = w.chars().next().unwrap();
            RegExp::Or(
                Box::new(RegExp::Single(first_char)),
                Box::new(RegExp::from_charlist(&w[1..])),
            )
        }
    }
}

/// The DFA states are encoded as usizes for easy indexing into `Vec`
type DFAState = usize;

/// The Nondeterministic Finite Automaton type
///
/// The alphabet is implicitly the set of all `char`s, and the state set is a set of `usize`s.
/// The initial state will always be 0.
///
/// The type parameter `T` indicates the keys used for the transition functions.
#[derive(Debug, Clone)]
struct InvalidDFA<T> {
    /// Set of states = usizes
    ///
    /// List of states
    ///
    /// For a given state `s`, `states[s] = true` if and only if the state `s` is an accepting state,
    /// so a non accepting state `s` will satisfy `state[s] = false`
    states: Vec<bool>,
    /// Transition functions
    ///
    /// Indexed by current state and the parameter type `T`.
    /// Almost every function will require the type `T = Option<char>`,
    /// but for building the DFA we use `T = RegExp`.
    transitions: HashMap<(DFAState, T), Vec<DFAState>>,
}

#[derive(Debug, Clone)]
pub struct DFA(InvalidDFA<Option<char>>);

impl<'a> InvalidDFA<&'a RegExp> {
    fn insert_transition<'b: 'a>(
        &mut self,
        start: DFAState,
        r: &'b RegExp,
        end: DFAState,
    ) {
        let r_key = (start, r);
        let r_entry = self.transitions.entry(r_key);
        r_entry
            .and_modify(|ends| {
                ends.push(end);
            })
            .or_insert(vec![end]);
    }

    /// Converts a `DFA<RegExp>` into an `DFA<Option<char>>`
    ///
    /// Require that every `RegExp` in the `DFA` is one of
    /// - `Empty`
    /// - `Single`
    /// - `Range`
    fn remove_regex(mut self) -> DFA {
        // states stay the same, just iterate over every transition and switch
        // from RegExp to possibly many transitions from chars

        let mut char_nfa: InvalidDFA<Option<char>> = InvalidDFA {
            states: self.states,
            transitions: HashMap::new(),
        };
        for ((start, pat), ends) in self.transitions.drain() {
            match pat {
                RegExp::Empty => {
                    char_nfa.transitions.insert((start, None), ends);
                }
                RegExp::Single(c) => {
                    char_nfa
                        .transitions
                        .insert((start, Some(*c)), ends.clone());
                }
                RegExp::Range(c1, c2) => {
                    // inclusive range
                    for c in *c1..=*c2 {
                        char_nfa
                            .transitions
                            .insert((start, Some(c)), ends.clone());
                    }
                }
                _ => {
                    panic!("Found complex RegExp when in remove_regex!")
                }
            }
        }

        DFA(char_nfa)
    }
}

impl DFA {
    /// Convert a regular expression into a DFA
    ///
    /// Procedure adapted from
    /// [https://www.cs.cmu.edu/~janh/courses/411/24/lectures/09-lex.pdf]
    pub fn from_regex(pat: &RegExp) -> Self {
        let mut nfa: InvalidDFA<&RegExp> = InvalidDFA {
            states: Vec::new(),
            transitions: HashMap::new(),
        };
        // initialize with initial -> final via pat
        // initial state at 0
        nfa.states.push(false);
        // final state at 1
        nfa.states.push(true);
        // transition
        nfa.transitions.insert((0, pat), vec![1]);

        // keep worklist of transition labels to decompose
        let mut worklist: Vec<(DFAState, &RegExp, DFAState)> =
            vec![(0, pat, 1)];

        while let Some((start, pat, end)) = worklist.pop() {
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
                    nfa.states.push(false);

                    nfa.insert_transition(start, r, mid);
                    nfa.insert_transition(mid, s, end);

                    // add new transitions to worklist
                    worklist.push((start, r, mid));
                    worklist.push((mid, s, end));

                    // remove old transition
                    nfa.transitions.remove(&(start, pat));
                }
                RegExp::Star(r) => {
                    // need two intermediate states
                    let q1 = nfa.states.len();
                    nfa.states.push(false);
                    let p1 = nfa.states.len();
                    nfa.states.push(false);

                    nfa.insert_transition(start, &RegExp::Empty, end);
                    nfa.insert_transition(start, &RegExp::Empty, q1);
                    nfa.insert_transition(q1, r, p1);
                    nfa.insert_transition(p1, &RegExp::Empty, q1);
                    nfa.insert_transition(p1, &RegExp::Empty, end);

                    // add transitions to worklist (skip Empty transitions)
                    worklist.push((q1, r, p1));

                    // remove old transition
                    nfa.transitions.remove(&(start, pat));
                }
                _ => {}
            }
        }

        // when the worklist is done, every RegExp transition is either single
        // range, or empty map those into transitions labelled by characters

        nfa.remove_regex()
    }

    /// Compute the epsilon closure of a set of states
    ///
    /// Returns the set of all states reachable by following `Empty` transitions,
    /// including any input states.
    /// `current_states` is empty afterwards, and `next_states` contains all of
    /// the states reachable by epsilon closure.
    /// Additionally, returns whether or not any of the reachable states is
    /// accepting.
    fn eps_closure(
        &self,
        current_states: &mut Vec<DFAState>,
        next_states: &mut Vec<DFAState>,
    ) -> bool {
        let DFA(internal) = self;

        let mut accepting = false;

        while let Some(state) = current_states.pop() {
            if next_states.contains(&state) {
                continue;
            }

            // get all epsilon transitions, add to current states
            if let Some(neighbors) = internal.transitions.get(&(state, None)) {
                current_states.append(&mut neighbors.clone());
            }

            // mark this state as visited, and also mark if it's accepting
            next_states.push(state);
            if internal.states[state] {
                accepting = true;
            }
        }

        debug_assert!(current_states.is_empty());

        accepting
    }

    /// Match a (prefix of) a string against the DFA
    ///
    /// Returns the index (if any) of the longest match.
    /// Note that all matches must start at the beginning of the string.
    pub fn matches_against(&self, s: &str) -> Option<usize> {
        let DFA(internal) = self;

        // switch to Vec since we're expecting to not have too many states

        // we know that we'll only have at most internal.states.len() states
        // so we can reserve that much space

        let mut current_states = Vec::with_capacity(internal.states.len());
        let mut next_states = Vec::with_capacity(internal.states.len());
        let mut accepting: Option<usize> = None;

        // start at initial state
        current_states.push(0);

        // need to perform epsilon closure even if input string is empty, cannot
        // rely on doing it in the loop to handle the empty case as well

        if self.eps_closure(&mut current_states, &mut next_states) {
            accepting = Some(0);
        }
        // set current states to next states
        std::mem::swap(&mut current_states, &mut next_states);

        for (i, c) in s.chars().enumerate() {
            // transition via c
            // insert new states into pre_states
            assert!(next_states.is_empty());

            for state in current_states.drain(..) {
                // add new states to pre_states
                if let Some(end_states) =
                    internal.transitions.get(&(state, Some(c)))
                {
                    let _ = end_states
                        .iter()
                        .map(|s| {
                            if !next_states.contains(s) {
                                next_states.push(*s)
                            }
                        })
                        .collect::<Vec<_>>();
                }
                // if no end states, do nothing, to drain out the bad state
            }

            // transition via empty
            // if no match, don't overwrite last match found
            // since accepting keeps track of length, need to add one
            if self.eps_closure(&mut next_states, &mut current_states) {
                accepting = Some(i + 1);
            }

            // if post_states is empty, break early since we can't possibly
            // transition from anywhere
            if current_states.is_empty() {
                break;
            }
        }

        accepting
    }
}

#[cfg(test)]
mod tests {
    use super::{RegExp, DFA};

    #[test]
    fn empty() {
        let pat = RegExp::Empty;
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against(""), Some(0));
        assert_eq!(nfa.matches_against("hello"), Some(0));
        // the empty RegExp will match against any string,
        // since the empty string is always a prefix of any string
    }

    #[test]
    fn singleton() {
        let pat = RegExp::Single('l');
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against(""), None);
        assert_eq!(nfa.matches_against("hello"), None);
        assert_eq!(nfa.matches_against("lollipop"), Some(1));
    }

    #[test]
    fn simple_or() {
        let pat = RegExp::Or(
            Box::new(RegExp::Single('b')),
            Box::new(RegExp::Single('c')),
        );
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against(""), None);
        assert_eq!(nfa.matches_against("bat"), Some(1));
        assert_eq!(nfa.matches_against("cat"), Some(1));
        assert_eq!(nfa.matches_against("acat"), None);
        assert_eq!(nfa.matches_against("abat"), None);
    }

    #[test]
    fn simple_split() {
        let pat = RegExp::Split(
            Box::new(RegExp::Single('h')),
            Box::new(RegExp::Single('i')),
        );
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against(""), None);
        assert_eq!(nfa.matches_against("hiya"), Some(2));
        assert_eq!(nfa.matches_against("hello"), None);
        assert_eq!(nfa.matches_against("it works!"), None);
    }

    #[test]
    fn simple_range() {
        let pat = RegExp::Range('a', 'z');
        let nfa = DFA::from_regex(&pat);
        for c in 'a'..='z' {
            let s = String::from(c);
            assert_eq!(nfa.matches_against(&s), Some(1));
        }
    }

    #[test]
    fn simple_star() {
        let pat = RegExp::Star(Box::new(RegExp::Single('a')));
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against(""), Some(0));
        assert_eq!(nfa.matches_against("aaaaa"), Some(5));
        assert_eq!(nfa.matches_against("a"), Some(1));
        assert_eq!(nfa.matches_against("aaabaa"), Some(3));
    }

    #[test]
    fn long_split() {
        // sidenote: wow this is grimy
        // once this is sufficiently tested I definitely need to use
        // this to parse regex inputs
        let pat = RegExp::Split(
            Box::new(RegExp::Single('h')),
            Box::new(RegExp::Split(
                Box::new(RegExp::Single('e')),
                Box::new(RegExp::Split(
                    Box::new(RegExp::Single('l')),
                    Box::new(RegExp::Split(
                        Box::new(RegExp::Single('l')),
                        Box::new(RegExp::Split(
                            Box::new(RegExp::Single('o')),
                            Box::new(RegExp::Empty),
                        )),
                    )),
                )),
            )),
        );
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against(""), None);
        assert_eq!(nfa.matches_against("hello, world!"), Some(5));
        assert_eq!(nfa.matches_against("help me!"), None);
    }

    #[test]
    fn match_reals() {
        // ([0-9][0-9]*.[0-9]*)|(.[0-9][0-9]*)
        // OR (
        //      Split(
        //          Range(0,9),
        //          Split(
        //              Star(Range(0,9)),
        //              Split(
        //                  Single(.),
        //                  Star(Range(0,9))
        //              )
        //          )
        //      ),
        //      Split(
        //          Single(.),
        //          Split(
        //              Range(0,9),
        //              Star(Range(0,9))
        //          )
        //      )
        // )
        let pat = RegExp::Or(
            Box::new(RegExp::Split(
                Box::new(RegExp::Range('0', '9')),
                Box::new(RegExp::Split(
                    Box::new(RegExp::Star(Box::new(RegExp::Range('0', '9')))),
                    Box::new(RegExp::Split(
                        Box::new(RegExp::Single('.')),
                        Box::new(RegExp::Star(Box::new(RegExp::Range(
                            '0', '9',
                        )))),
                    )),
                )),
            )),
            Box::new(RegExp::Split(
                Box::new(RegExp::Single('.')),
                Box::new(RegExp::Split(
                    Box::new(RegExp::Range('0', '9')),
                    Box::new(RegExp::Star(Box::new(RegExp::Range('0', '9')))),
                )),
            )),
        );
        // holy gross
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against("0123"), None);
        assert_eq!(nfa.matches_against("3.1415"), Some(6));
        assert_eq!(nfa.matches_against(".0"), Some(2));
        assert_eq!(nfa.matches_against(".9"), Some(2));
        assert_eq!(nfa.matches_against("really? .0"), None);
        assert_eq!(nfa.matches_against("420.69 haha weed"), Some(6));
    }

    #[test]
    fn match_ident() {
        // [a-z]([a-z]|[0-9])*
        let pat = RegExp::Split(
            Box::new(RegExp::Range('a', 'z')),
            Box::new(RegExp::Star(Box::new(RegExp::Or(
                Box::new(RegExp::Range('a', 'z')),
                Box::new(RegExp::Range('0', '9')),
            )))),
        );
        let nfa = DFA::from_regex(&pat);
        assert_eq!(nfa.matches_against("0123"), None);
        assert_eq!(nfa.matches_against(""), None);
        assert_eq!(nfa.matches_against("x"), Some(1));
        assert_eq!(nfa.matches_against("x2"), Some(2));
        assert_eq!(nfa.matches_against("x2y2z2? maybe idk"), Some(6));
    }
}
