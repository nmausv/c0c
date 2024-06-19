#![allow(dead_code)]
use std::collections::{HashMap, HashSet};

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

/// The DFA states are encoded as usizes for easy indexing into `Vec`
type DFAState = usize;
/// The Nondeterministic Finite Automaton type
///
/// The alphabet is implicitly the set of all `char`s, and the state set is a set of `usize`s.
/// The initial state will always be 0.
///
/// The type parameter `T` indicates the keys used for the transition functions.
/// After the initial processing, `T` will always be `Option<char>`.
#[derive(Debug)]
pub struct DFA<T> {
    /// Set of states = usizes
    ///
    /// List of states
    ///
    /// For a given state `s`, `states[s] = None` if the state is not defined for this DFA.
    /// Moreover, `states[s] = Some(true)` if and only if the state `s` is an accepting state.
    states: Vec<Option<bool>>,
    /// Transition functions
    ///
    /// Indexed by current state and the parameter type T.
    /// Almost every function will require the type T = Option<char>,
    /// but for building the DFA we use T = RegExp.
    ///
    /// The reason to use `HashSet<usize>` instead of `Vec<bool>` or similar is that we expect
    /// that most end state lists to be relatively small, so using a `Vec<bool>` requires iterating
    /// over all the states to find the "good" ones, where as the `HashSet<usize>` doesn't need to
    /// iterate over every state.
    transitions: HashMap<(DFAState, T), HashSet<DFAState>>,
}

impl<'a> DFA<&'a RegExp> {
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
                ends.insert(end);
            })
            .or_insert(HashSet::from([end]));
    }

    /// Converts an `DFA<RegExp>` into an `DFA<Option<char>>`
    ///
    /// Require that every `RegExp` in the `DFA` is one of
    /// - `Empty`
    /// - `Single`
    /// - `Range`
    fn remove_regex(mut self) -> DFA<Option<char>> {
        // states stay the same, just iterate over every transition and switch
        // from RegExp to possibly many transitions from chars

        let mut char_nfa: DFA<Option<char>> = DFA {
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

        char_nfa
    }
}

impl DFA<Option<char>> {
    /// Convert a regular expression into a DFA
    ///
    /// Procedure adapted from
    /// [https://www.cs.cmu.edu/~janh/courses/411/24/lectures/09-lex.pdf]
    pub fn from_regex(pat: &RegExp) -> Self {
        let mut nfa: DFA<&RegExp> = DFA {
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
        let mut worklist: Vec<(DFAState, &RegExp, DFAState)> =
            vec![(0, &pat, 1)];

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
                    worklist.push((start, r, mid));
                    worklist.push((mid, s, end));

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
    /// Additionally, returns whether or not any of the reachable states is
    /// accepting.
    fn eps_closure(
        &self,
        pre_states: &mut HashSet<DFAState>,
    ) -> (HashSet<DFAState>, bool) {
        let mut post_states: HashSet<DFAState> = HashSet::new();
        // check if any pre states accept
        let mut accepting = false;
        while !pre_states.is_empty() {
            post_states = post_states.union(pre_states).cloned().collect();
            // get a state
            let &state = pre_states.iter().next().unwrap();
            // if anything accepts, make accepting true, don't overwrite false
            accepting |= self.states[state] == Some(true);
            // remove the state from pre_states
            pre_states.remove(&state);
            // get empty transition end states
            if let Some(end_states) = self.transitions.get(&(state, None)) {
                for &end_state in end_states {
                    // add any new states to pre for further epsilon traversal
                    pre_states.insert(end_state);
                    // add any new states to post to keep track of them
                    post_states.insert(end_state);

                    // if post_state is accepting, mark accepting
                    // optimization: skip check if accepting already marked
                    if !accepting && self.states[end_state] == Some(true) {
                        accepting = true;
                    }
                }
            }
        }

        (post_states, accepting)
    }

    /// Match a (prefix of) a string against the DFA
    ///
    /// Returns the index (if any) of the longest match.
    /// Note that all matches must start at the beginning of the string.
    pub fn matches_against(&self, s: &str) -> Option<usize> {
        // before epsilon closure
        let mut pre_states: HashSet<DFAState> = HashSet::from([0 as DFAState]);
        // after epsilon closure
        let mut post_states: HashSet<DFAState>;

        let mut accepting: Option<usize>;

        // need to perform epsilon closure even if input string is empty, cannot
        // rely on doing it in the loop to handle the empty case as well
        (post_states, accepting) = match self.eps_closure(&mut pre_states) {
            (post, true) => (post, Some(0)),
            (post, false) => (post, None),
        };

        // move back into pre_states to set up the loop
        // pre_states = post_states.clone();

        for (i, c) in s.chars().enumerate() {
            // transition via c
            // insert new states into pre_states
            for state in post_states.drain() {
                // add new states to pre_states
                if let Some(end_states) =
                    self.transitions.get(&(state, Some(c)))
                {
                    pre_states =
                        pre_states.union(end_states).cloned().collect();
                }
                // if no end states, do nothing, to drain out the bad state
            }

            // transition via empty
            // if no match, don't overwrite last match found
            // since accepting keeps track of length, need to add one
            (post_states, accepting) = match self.eps_closure(&mut pre_states) {
                (post, true) => (post, Some(i + 1)),
                (post, false) => (post, accepting),
            };
            // pre_states is empty now

            // if post_states is empty, break early since we can't possibly
            // transition from anywhere
            if post_states.is_empty() {
                break;
            }
        }

        accepting
    }
}

#[cfg(test)]
mod dfa_tests {
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
