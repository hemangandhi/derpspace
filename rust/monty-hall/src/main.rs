// NOTE: I can't be arsed to make a package yet.
// Otherwise I'd totally use clap and ratio.

use std::collections::{BTreeMap, HashSet};

mod ratio {
    use std::cmp;
    use std::convert;
    use std::fmt;
    use std::ops;

    fn gcd(mut x: usize, mut y: usize) -> usize {
        while y != 0 {
            let t = y;
            y = x % y;
            x = t;
        }
        return x;
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Ratio {
        pub num: usize,
        pub denom: usize,
    }

    impl fmt::Display for Ratio {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{} / {}", self.num, self.denom)
        }
    }

    impl Ratio {
        fn simplify(&mut self) {
            let common_factors = gcd(self.num, self.denom);
            self.num /= common_factors;
            self.denom /= common_factors;
        }
    }

    impl convert::From<usize> for Ratio {
        fn from(value: usize) -> Self {
            Ratio {
                num: value,
                denom: 1,
            }
        }
    }

    impl convert::From<(usize, usize)> for Ratio {
        fn from(value: (usize, usize)) -> Self {
            let mut r = Ratio {
                num: value.0,
                denom: value.1,
            };
            r.simplify();
            return r;
        }
    }

    impl ops::Div for Ratio {
        type Output = Self;

        fn div(self, rhs: Self) -> Self {
            let mut quotient = Ratio {
                num: self.num * rhs.denom,
                denom: self.denom * rhs.num,
            };
            quotient.simplify();
            return quotient;
        }
    }

    impl cmp::Ord for Ratio {
        fn cmp(&self, other: &Ratio) -> cmp::Ordering {
            (self.num * other.denom).cmp(&(self.denom * other.num))
        }
    }

    impl cmp::PartialOrd for Ratio {
        fn partial_cmp(&self, other: &Ratio) -> Option<cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl cmp::PartialEq for Ratio {
        fn eq(&self, other: &Ratio) -> bool {
            (self.num * other.denom) == (self.denom * other.num)
        }
    }

    impl cmp::Eq for Ratio {}

    #[cfg(test)]
    mod test {
        use crate::ratio::Ratio;

        #[test]
        fn test_division() {
            assert_eq!(
                Into::<Ratio>::into(3usize) / 6usize.into(),
                Ratio { num: 1, denom: 2 }
            );
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct DoorState {
    is_opened: bool,
    is_prize: bool,
}

#[derive(Default)]
struct Metrics {
    runs: usize,
    wins: usize,
    losses: usize,

    wins_at_swapiness: BTreeMap<ratio::Ratio, usize>,
    plays_at_swapiness: BTreeMap<ratio::Ratio, usize>,
}

impl std::iter::Sum for Metrics {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut total = Metrics::default();
        for i in iter {
            total.runs += i.runs;
            total.wins += i.wins;
            total.losses += i.losses;

            for (ratio, win) in i.wins_at_swapiness.iter() {
                *total.wins_at_swapiness.entry(*ratio).or_insert(0) += win;
            }
            for (ratio, play) in i.plays_at_swapiness.iter() {
                *total.plays_at_swapiness.entry(*ratio).or_insert(0) += play;
            }
        }
        return total;
    }
}

impl std::fmt::Display for Metrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Stats:
Plays: {}
Wins: {}
Losses: {}
Win rate across all strategies: {}
Wins per swappiness:

Swappiness\tWin rate in swap\tWin rate over all
----------\t----------------\t-----------------
",
            self.runs,
            self.wins,
            self.losses,
            ratio::Ratio::from((self.wins, self.runs))
        )?;
        for (swapiness, plays) in self.plays_at_swapiness.iter() {
            let wins = self.wins_at_swapiness[swapiness];
            write!(
                f,
                "{}\t\t{}\t\t\t{}\n",
                swapiness,
                ratio::Ratio::from((wins, *plays)),
                ratio::Ratio::from((wins, self.runs))
            )?;
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
struct MontyHallState {
    used_doors: HashSet<usize>,
    previous_door: usize,
    swapped_at_plays: HashSet<usize>,

    door_states: Vec<DoorState>,
    win_index: usize,
}

impl MontyHallState {
    fn to_metric(self) -> Metrics {
        let is_win = self.win_index == self.previous_door;
        let swapiness =
            ratio::Ratio::from((self.swapped_at_plays.len(), self.door_states.len() - 2));
        Metrics {
            runs: 1,
            wins: if is_win { 1 } else { 0 },
            losses: if !is_win { 1 } else { 0 },
            wins_at_swapiness: BTreeMap::from([(swapiness, if is_win { 1 } else { 0 })]),
            plays_at_swapiness: BTreeMap::from([(swapiness, 1)]),
        }
    }

    fn initial_states_for_doors(n_doors: usize) -> Vec<MontyHallState> {
        (0usize..n_doors)
            .flat_map(|win_index| {
                (0usize..n_doors).map(move |first_guess| MontyHallState {
                    used_doors: HashSet::from([first_guess]),
                    previous_door: first_guess,
                    swapped_at_plays: HashSet::new(),
                    door_states: (0usize..n_doors)
                        .map(|i| DoorState {
                            is_opened: false,
                            is_prize: i == win_index,
                        })
                        .collect(),
                    win_index,
                })
            })
            .collect()
    }

    fn generate_plays(&self, pick_index: usize) -> Vec<MontyHallState> {
        (0usize..(self.door_states.len()))
            .flat_map(|host_open| {
                (0usize..(self.door_states.len())).filter_map(move |next_pick| {
                    // TODO: filter these earlier?
                    if self.door_states[host_open].is_opened || self.door_states[host_open].is_prize
                    {
                        return None;
                    }
                    if next_pick == host_open || self.door_states[next_pick].is_opened {
                        return None;
                    }
                    Some(MontyHallState {
                        used_doors: &self.used_doors | &HashSet::from([next_pick]),
                        previous_door: next_pick,
                        swapped_at_plays: &self.swapped_at_plays
                            | &(if next_pick == self.previous_door {
                                HashSet::new()
                            } else {
                                HashSet::from([pick_index])
                            }),
                        door_states: self
                            .door_states
                            .iter()
                            .enumerate()
                            .map(|(i, d)| DoorState {
                                is_opened: d.is_opened || (i == host_open),
                                is_prize: d.is_prize,
                            })
                            .collect(),
                        win_index: self.win_index,
                    })
                })
            })
            .collect()
    }
}

fn tabluate_plays(n_doors: usize) -> Metrics {
    (1..(n_doors - 1))
        .fold(
            MontyHallState::initial_states_for_doors(n_doors),
            |states, pick_index| {
                states
                    .into_iter()
                    .flat_map(|s| s.generate_plays(pick_index))
                    .collect()
            },
        )
        .into_iter()
        .map(|s| s.to_metric())
        .sum()
}

fn main() {
    println!("Monty hall 4: {}", tabluate_plays(4));
}
