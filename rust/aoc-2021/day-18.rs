use std::borrow::BorrowMut;
use std::ops::Add;
use std::fmt::{Display, Formatter};

const INPUT: &str = "[[2,[[4,8],7]],[[9,7],[[2,0],9]]]
[0,[7,5]]
[[[5,[6,9]],4],[3,3]]
[[[6,[6,9]],4],[[[4,8],8],[6,5]]]
[[[[1,4],[2,1]],[6,0]],[[[9,1],[4,2]],[[0,4],0]]]
[[9,4],[[8,6],1]]
[[[[0,7],0],7],[1,[2,9]]]
[[[2,9],[[8,4],[4,0]]],[[[6,2],2],[9,5]]]
[[[0,[5,8]],[6,8]],[[[0,7],4],[[2,8],4]]]
[[3,[[4,1],[0,7]]],[[1,[5,1]],4]]
[[[[2,9],6],[[5,3],2]],[[8,[2,0]],9]]
[0,[[[2,7],9],[1,8]]]
[[[2,[6,2]],[[4,0],[9,6]]],[[6,1],[8,9]]]
[[[[9,6],9],[5,[1,8]]],[[[9,6],9],[[2,0],[3,8]]]]
[[[[4,3],[0,8]],4],[6,6]]
[[[[4,3],7],[[7,0],5]],[2,[[9,9],4]]]
[[[[4,3],[1,7]],[[3,1],[0,9]]],0]
[[5,[[2,5],[2,8]]],[[4,0],[[5,2],[9,8]]]]
[[[0,[3,5]],7],[[[5,9],2],4]]
[[9,[[4,4],8]],[[[2,8],1],[[0,9],5]]]
[[[6,8],[0,1]],[[8,2],[2,0]]]
[[[1,9],[[9,1],2]],[[6,4],[[7,7],[8,3]]]]
[[1,[5,[7,6]]],[[[4,7],4],5]]
[[[8,0],9],[[[6,0],4],1]]
[[[4,[4,2]],7],[[6,[0,9]],[[3,0],[7,6]]]]
[[[[3,4],[9,0]],[4,4]],[[9,6],7]]
[4,[[8,3],[7,1]]]
[6,[6,8]]
[[[[0,6],[7,6]],[5,3]],[[[8,9],[6,0]],[[8,5],7]]]
[[[[0,3],1],5],[[[4,3],[3,2]],[2,[5,9]]]]
[[[[3,1],0],[1,[8,4]]],[4,5]]
[[[0,[4,1]],1],[[1,6],[[4,8],[8,3]]]]
[[[1,4],6],[9,[1,2]]]
[[9,[[0,7],1]],[[0,9],[0,[4,4]]]]
[[1,[7,4]],[[2,[5,3]],[[6,6],9]]]
[0,[0,[0,[0,4]]]]
[[[[9,7],[4,9]],[9,[3,5]]],[[9,7],7]]
[5,[9,[[4,1],[2,9]]]]
[[0,[8,4]],1]
[[[9,[3,3]],[8,6]],[7,[[1,6],0]]]
[[[1,[0,7]],[[9,1],8]],[[[2,2],5],[[7,1],[2,2]]]]
[[[7,[0,3]],4],[[6,[1,6]],[8,7]]]
[[[[4,8],3],[[6,1],7]],[8,[3,[7,8]]]]
[3,[[[9,6],9],3]]
[[[5,[1,0]],[1,4]],5]
[[[[4,7],2],[[7,0],[6,7]]],[[1,[0,3]],0]]
[9,[[3,7],[6,1]]]
[[[2,5],[[0,7],[0,7]]],[[[0,3],2],8]]
[[[[4,4],7],[2,[0,7]]],[[[1,4],[6,6]],[[8,9],[5,2]]]]
[[[[0,8],5],[[3,5],7]],[[[5,6],[0,0]],[[3,8],6]]]
[4,[8,[9,[2,3]]]]
[[[[6,6],9],0],[[[2,9],[0,8]],5]]
[[[8,[4,0]],[[2,1],[7,3]]],[8,7]]
[[6,[9,[1,8]]],[[7,[7,9]],[[2,3],1]]]
[[6,[[1,7],1]],[[[5,3],[2,0]],[[4,4],9]]]
[[[[8,0],[0,3]],[[4,8],[0,9]]],[8,[7,[8,6]]]]
[6,0]
[[[[5,2],0],[3,3]],[0,4]]
[[[9,5],[6,4]],[[[7,2],0],8]]
[[[0,9],[5,[2,3]]],2]
[[[[5,4],[2,9]],[1,[9,0]]],[[9,9],[9,6]]]
[[[7,[4,8]],[9,8]],[[[1,3],0],[4,[4,7]]]]
[[7,[7,9]],0]
[[[[6,7],[8,1]],[[0,2],2]],[[[7,6],6],[[3,4],[9,9]]]]
[[7,[6,[2,2]]],[[[8,8],[0,4]],[5,[7,7]]]]
[[[[0,6],[9,2]],[8,1]],[[[0,4],2],[[5,9],[4,9]]]]
[[[[9,1],[1,7]],[[3,1],[0,7]]],[[2,[4,9]],[9,1]]]
[[[9,4],2],[[[2,3],3],[6,[5,7]]]]
[[[0,8],[[0,9],2]],[[[0,7],[4,4]],7]]
[[[5,2],4],[0,6]]
[[3,[9,[9,2]]],[8,[1,[6,8]]]]
[3,[7,[[8,0],[1,7]]]]
[[[[2,4],[7,3]],[[0,7],0]],5]
[[[[6,0],8],[1,4]],[[[3,3],[8,6]],5]]
[[5,[5,[6,2]]],4]
[[[0,7],[[4,1],4]],[[8,[3,2]],[7,7]]]
[[1,[[6,5],[2,2]]],[[6,[2,8]],[1,0]]]
[6,[4,[[2,2],[1,8]]]]
[[[[3,3],1],[[4,1],7]],[[[5,2],7],[4,[4,7]]]]
[[[[2,2],1],[[4,1],3]],[1,[[0,9],[3,8]]]]
[[0,[0,4]],[[9,[7,5]],[8,[8,0]]]]
[[[[0,3],3],[[7,3],5]],[4,[[0,1],[3,0]]]]
[[4,8],3]
[[[6,0],7],[[6,8],[8,6]]]
[[[[8,5],3],[[6,2],[2,6]]],[[[2,7],5],[[3,8],[6,9]]]]
[7,[4,2]]
[[[[6,0],[7,8]],6],[[[4,6],6],7]]
[[[0,[2,1]],[5,[3,8]]],[[[3,9],3],[[0,9],3]]]
[[[8,6],[4,0]],[2,[[4,1],8]]]
[[[0,1],[[2,0],5]],[[[0,1],[7,0]],[[1,2],[1,4]]]]
[[[8,8],[[4,4],3]],[1,[4,1]]]
[[[5,[0,7]],[7,5]],[[7,6],[5,5]]]
[[[9,[1,3]],[[3,3],6]],[4,[[5,6],8]]]
[[[9,[3,0]],[8,5]],[1,[[8,0],3]]]
[[[3,[3,9]],[[2,4],[4,6]]],[[1,2],3]]
[[[1,[3,1]],[3,[6,3]]],[1,[5,7]]]
[[[[5,5],[1,5]],3],[9,[[7,4],[9,2]]]]
[[[6,[7,1]],[[6,6],[1,6]]],7]
[[[[1,4],0],[8,3]],[[[8,2],9],[[0,3],[9,5]]]]
[[4,[1,[0,1]]],[[1,[7,3]],1]]";

#[derive(Clone)]
enum Pair {
  Zero,
  Regular(u32),
  Paired(Box<Pair>, Box<Pair>)
}

impl Pair {
  fn magnitude(&self) -> u32 {
    match self {
      Pair::Zero => 0,
      Pair::Regular(x) => *x,
      Pair::Paired(left, right) => 3 * left.magnitude() + 2 * right.magnitude() 
    }
  }

  fn reduce(&mut self) {
    fn move_number(
      left: bool,
      n: u32,
      pair: &mut Pair) {
      match pair {
        Pair::Zero => {
          *pair = Pair::Regular(n);
        }
        Pair::Regular(x) => {
          *pair = Pair::Regular(*x + n);
        },
        Pair::Paired(l, r) => {
          if left {
            move_number(left, n, r.borrow_mut());
          } else {
            move_number(left, n, l.borrow_mut());
          }
        }
      }
    }

    fn find_split(pair: &mut Pair) -> bool {
      match pair {
        Pair::Zero => false,
        Pair::Regular(x) => {
          if *x >= 10 {
            *pair = 
              Pair::Paired(
                Box::new(Pair::Regular(*x/2)),
                Box::new(Pair::Regular(*x/2 + *x % 2)));
	    true
          } else {
            false
	  }
        }
        Pair::Paired(l, r) => find_split(l.borrow_mut()) || find_split(r.borrow_mut())
      }
    }

    fn find_explosion(pair: &mut Pair, depth: u8) -> (Option<u32>, Option<u32>, bool) {
      match pair {
        Pair::Zero => (Option::None, Option::None, false),
        Pair::Regular(x) => (Option::None, Option::None, false),
        Pair::Paired(l, r) => {
	  if depth >= 4 {
            if let Pair::Regular(lv) = **l {
              if let Pair::Regular(rv) = **r {
                *pair = Pair::Regular(0);
                 return (Option::Some(lv), Option::Some(rv), true);
              }
            }
	  }
          let (lm, rm, m, wr) = {
            let (mut lm, mut rm, mut m) = find_explosion(l.borrow_mut(), depth + 1);
            let wr = !m;
            if !m {
              let (lm1, rm1, m1) = find_explosion(r.borrow_mut(), depth + 1);
              lm = lm1; rm = rm1; m = m1;
            }
            if !m {
              return (Option::None, Option::None, false);
            }
            (lm, rm, m, wr)
          };
          if wr {
            if let Option::Some(leftward) = lm {
              move_number(true, leftward, l);
	    }
            return (Option::None, rm, m);
          } else {
            if let Option::Some(rightward) = rm {
              move_number(false, rightward, r);
            }
            return (lm, Option::None, m);
          }
        }
      }
    } // find_explosion

    let mut needs_reduction = true;
    while needs_reduction {
      needs_reduction = find_explosion(self, 0).2 || find_split(self);
    }
  }
}

impl Display for Pair {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Pair::Zero => write!(f, "0"),
      Pair::Regular(n) => write!(f, "{}", n),
      Pair::Paired(l, r) => write!(f, "[{},{}]", *l, *r)
    }
  }
}

impl Add for Pair {
  type Output = Pair;

  fn add(self, other: Pair) -> Pair {
    match (self, other) {
      (Pair::Zero, x) => x,
      (y, Pair::Zero) => y,
      (x, y) => {
        let mut ured = Pair::Paired(Box::new(x), Box::new(y));
	ured.reduce();
	ured
      }
    }
  }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum PairToken {
  Open,
  Close,
  Comma,
  Int(u32)
}

#[derive(Debug)]
enum PairParseProblem {
  IntParseIssue,
  Unexpected(PairToken),
  Truncated
}

fn expect_precisely(
  etok: &PairToken,
  it: &mut impl Iterator<Item=Result<PairToken, PairParseProblem>>)
-> Result<(), PairParseProblem> {
  if let Option::Some(tok_res) = it.next() {
    match tok_res {
      Result::Ok(tok) => {
        return if tok != *etok {
          Result::Err(PairParseProblem::Unexpected(tok))
        } else {
          Result::Ok(())
        };
      }
      Result::Err(e) => { return Result::Err(e); }
    }
  }
  return Result::Err(PairParseProblem::Truncated);
}

fn parse_paired(it: &mut impl Iterator<Item=Result<PairToken, PairParseProblem>>)
 -> Result<Pair, PairParseProblem> {
  if let Option::Some(tok) = it.next() {
    match tok? {
      PairToken::Open => {
        let left = Box::new(parse_paired(it)?);
        expect_precisely(&PairToken::Comma, it)?;
        let right = Box::new(parse_paired(it)?);
        expect_precisely(&PairToken::Close, it)?;
        Result::Ok(Pair::Paired(left, right))
      }
      PairToken::Close => Result::Err(PairParseProblem::Unexpected(PairToken::Close)),
      PairToken::Comma => Result::Err(PairParseProblem::Unexpected(PairToken::Comma)),
      PairToken::Int(i) => Result::Ok(Pair::Regular(i))
    }
  } else {
    Result::Err(PairParseProblem::Truncated)
  }
}

struct PairTokenizer<'a> {
  base: &'a [u8],
  str_idx: usize
}

impl Iterator for PairTokenizer<'_> {
  type Item = Result<PairToken, PairParseProblem>;

  fn next(&mut self) -> Option<Result<PairToken, PairParseProblem>> {
    if self.str_idx > self.base.len() {
      return Option::None;
    }
    match self.base[self.str_idx] {
      b'[' => {
        self.str_idx += 1;
       Option::Some(Result::Ok(PairToken::Open))
      }
      b']' => {
        self.str_idx += 1;
        Option::Some(Result::Ok(PairToken::Close))
      }
      b',' => {
        self.str_idx += 1;
        Option::Some(Result::Ok(PairToken::Comma))
      }
      bit => {
        if !bit.is_ascii_digit() {
          return Option::Some(Result::Err(PairParseProblem::IntParseIssue));
        }
        let mut acc = (bit - b'0') as u32;
        self.str_idx += 1;
        while self.str_idx < self.base.len() && self.base[self.str_idx].is_ascii_digit() {
          acc *= 10;
          acc += (self.base[self.str_idx] - b'0') as u32;
          self.str_idx += 1;
        }
        Option::Some(Result::Ok(PairToken::Int(acc)))
      }
    }
  }
}

impl std::str::FromStr for Pair {
  type Err = PairParseProblem;

  fn from_str(s: &str) -> Result<Pair, PairParseProblem> {
    let mut p = parse_paired(&mut PairTokenizer {
      base: s.as_bytes(),
      str_idx: 0
    })?;
    p.reduce();
    Result::Ok(p)
  }
}

fn main() {
  println!("Part 1: {}", INPUT.split('\n')
                              .map(|l| l.parse::<Pair>().unwrap())
			      .fold(Pair::Zero, |s, p| {println!("Read: {}, running total: {}", p, s); s + p})
			      .magnitude());
  let par_dieu: Vec<Pair> = INPUT.split('\n')
                                 .map(|l| l.parse().unwrap())
				 .collect();
  println!("Part 2: {}", par_dieu.iter()
                                 .enumerate()
				 .flat_map(|(i, p)| par_dieu.iter().enumerate()
				                            .map(move |(j, q)| (i, p, j, q)))
                                 .filter(|(i, _p, j, _q)| i != j)
				 .map(|(_i, p, _j, q)| (p.clone() + q.clone()).magnitude())
				 .max().unwrap());
/*
  const EXAMPLE: &str = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]";
  println!("Part 1: {}", EXAMPLE.split('\n')
                              .map(|l| l.parse::<Pair>().unwrap())
			      .fold(Pair::Zero, |s, p| {println!("Read: {}, running total: {}", p, s); s + p})
			      .magnitude());
*/
}