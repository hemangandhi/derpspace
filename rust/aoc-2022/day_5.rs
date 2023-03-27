use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::{from_utf8, Utf8Error};
use std::string::FromUtf8Error;

const INPUT: &[u8] = include_bytes!("day_5_input.txt");

struct SupplyStack(HashMap<u8, RefCell<Vec<u8>>>);

#[derive(Debug)]
enum StackParseError {
    MisalignedStack,
    MismatchedNumberOfStacks,
    UnexpectedCharacter(u8),
    BadLine(String),
    ColumnIndexMismatch(usize),
    NoBottomRow,
    UnidentifiedStack(u8),
    EmptyStack(u8),
    CantParseMove(String),
    WrongNumberOfParts,
    PointlessMove,
}

impl TryFrom<&[u8]> for SupplyStack {
    type Error = StackParseError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let mut bottom_row: HashMap<usize, u8> = HashMap::new();
        let mut result = HashMap::new();
        for row in value
            .split(|c| *c == b'\n')
            .filter_map(|line| {
                if line.contains(&b'[') {
                    Option::Some(
                        line.chunks(4)
                            .enumerate()
                            .filter_map(|(idx, chunk)| match chunk[0] {
                                b' ' => Option::None,
                                b'[' => Option::Some(Result::Ok((idx, chunk[1]))),
                                _ => Option::Some(Result::Err(
                                    StackParseError::UnexpectedCharacter(chunk[0]),
                                )),
                            })
                            .collect::<Result<Vec<(usize, u8)>, StackParseError>>(),
                    )
                } else if !bottom_row.is_empty() {
                    Option::Some(Result::Err(StackParseError::BadLine(
                        String::from_utf8_lossy(line).to_string(),
                    )))
                } else {
                    <HashMap<usize, u8> as Extend<(usize, u8)>>::extend(
                        &mut bottom_row,
                        line.chunks(4).map(|c| c[1]).enumerate(),
                    );
                    Option::None
                }
            })
            .collect::<Result<Vec<Vec<(usize, u8)>>, StackParseError>>()?
        {
            if bottom_row.is_empty() {
                return Result::Err(StackParseError::NoBottomRow);
            }
            for (col_idx, crate_id) in row {
                if let Option::Some(&actual_id) = bottom_row.get(&col_idx) {
                    result
                        .entry(actual_id)
                        .or_insert(RefCell::new(Vec::new()))
                        .get_mut()
                        .push(crate_id);
                } else {
                    return Result::Err(StackParseError::ColumnIndexMismatch(col_idx));
                }
            }
        }
        for (_, queue) in result.iter_mut() {
            queue.get_mut().reverse();
        }
        Result::Ok(SupplyStack(result))
    }
}

struct CrateMove {
    num_crate: u32,
    source: u8,
    dest: u8,
}
impl TryFrom<&[u8]> for CrateMove {
    type Error = StackParseError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let bits = value
            .split(|c| *c == b' ')
            .enumerate()
            .filter_map(|(idx, sp)| {
                if idx % 2 == 0 {
                    Option::None
                } else {
                    Option::Some(sp)
                }
            })
            .collect::<Vec<&[u8]>>();
        if bits.len() != 3 {
            return Result::Err(StackParseError::CantParseMove(
                String::from_utf8_lossy(value).to_string(),
            ));
        }
        Result::Ok(CrateMove {
            num_crate: from_utf8(bits[0])
                .map_err(|_parse_e| {
                    StackParseError::CantParseMove(String::from_utf8_lossy(value).to_string())
                })
                .and_then(|v| {
                    v.parse().map_err(|_parse_e| {
                        StackParseError::CantParseMove(String::from_utf8_lossy(value).to_string())
                    })
                })?,
            source: bits[1][0],
            dest: bits[2][0],
        })
    }
}

impl SupplyStack {
    fn do_move(&mut self, to_do: CrateMove) -> Option<StackParseError> {
        let CrateMove {
            num_crate,
            source,
            dest,
        } = to_do;
        let source_vec = if let Option::Some(stack) = self.0.get(&source) {
            stack
        } else {
            return Option::Some(StackParseError::UnexpectedCharacter(source));
        };
        let dest_vec = if let Option::Some(stack) = self.0.get(&dest) {
            stack
        } else {
            return Option::Some(StackParseError::UnexpectedCharacter(dest));
        };
        let mut sv_borrow = source_vec.borrow_mut();
        let mut dv_borrow = match dest_vec.try_borrow_mut() {
            Result::Ok(x) => x,
            Result::Err(_) => {
                return Option::Some(StackParseError::PointlessMove);
            }
        };
        for _ in 0..num_crate {
            if sv_borrow.is_empty() {
                return Option::Some(StackParseError::EmptyStack(source));
            }
            dv_borrow.push(sv_borrow.pop().unwrap());
        }
        Option::None
    }

    fn do_move_9001(&mut self, to_do: CrateMove) -> Option<StackParseError> {
        let CrateMove {
            num_crate,
            source,
            dest,
        } = to_do;
        let source_vec = if let Option::Some(stack) = self.0.get(&source) {
            stack
        } else {
            return Option::Some(StackParseError::UnexpectedCharacter(source));
        };
        let dest_vec = if let Option::Some(stack) = self.0.get(&dest) {
            stack
        } else {
            return Option::Some(StackParseError::UnexpectedCharacter(dest));
        };
        let mut sv_borrow = source_vec.borrow_mut();
        let mut temp = Vec::new();
        for _ in 0..num_crate {
            if sv_borrow.is_empty() {
                return Option::Some(StackParseError::EmptyStack(source));
            }
            temp.push(sv_borrow.pop().unwrap());
        }
        temp.reverse();
        let mut dv_borrow = match dest_vec.try_borrow_mut() {
            Result::Ok(x) => x,
            Result::Err(_) => {
                return Option::Some(StackParseError::PointlessMove);
            }
        };
        dv_borrow.extend(temp.iter());
        Option::None
    }

    fn top_line(&self) -> Result<String, FromUtf8Error> {
        let mut top_letters: Vec<(u8, u8)> = self
            .0
            .iter()
            .filter_map(|(n, v)| Option::Some((*n, *v.borrow().last()?)))
            .collect();
        top_letters.sort();
        String::from_utf8(top_letters.iter().map(|(_t, l)| *l).collect::<Vec<u8>>())
    }
}

fn find_double_newline(values: &[u8]) -> Option<usize> {
    let mut prev_ch = 0;
    for (idx, ch) in values.iter().enumerate() {
        if *ch == prev_ch && *ch == b'\n' {
            return Option::Some(idx);
        }
        prev_ch = *ch;
    }
    Option::None
}

fn main() {
    let (crates_str, moves_str) = INPUT.split_at(find_double_newline(INPUT).unwrap());
    let mut crates = SupplyStack::try_from(crates_str.split_last().unwrap().1).unwrap();
    for move_obj_or in moves_str
        .split(|c| *c == b'\n')
        .filter(|s| !s.is_empty())
        .map(CrateMove::try_from)
    {
        let move_obj = move_obj_or.unwrap();
        if let Option::Some(err) = crates.do_move_9001(move_obj) {
            panic!("Move error! {:?}", err);
        }
    }
    println!("{}", crates.top_line().unwrap());
}
