use std::str::FromStr;

#[derive(Copy, Clone)]
struct Instruction {
    source: i8,
    destination: i8,
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Instruction, String> {
        let (idx_of_direction, direction) =
            match s.char_indices().skip_while(|(_i, c)| c.is_digit(10)).next() {
                Option::Some(x) => x,
                Option::None => return Result::Err("Missing direction character".into()),
            };
        let source = s[..idx_of_direction]
            .parse::<i8>()
            .map_err(|pe| pe.to_string())?;
        let upwards = match direction {
            'd' => false,
            'u' => true,
            _ => return Result::Err(format!("Expected u or d, got {}", direction)),
        };
        if idx_of_direction >= s.len() {
            return Result::Err("Missing destination".into());
        }
        let destination = s[idx_of_direction..]
            .parse::<i8>()
            .map_err(|pe| pe.to_string())?;
        if upwards != (source < destination) {
            return Result::Err(format!(
                "Direction specifier {} is invalid for moving from {} to {}",
                direction, source, destination
            ));
        }
        Ok(Instruction {
            source,
            destination,
        })
    }
}

struct ElevatorIter {
    is_upwards: bool,
    turning_point: i8,
    in_progess_instructions: Vec<Instruction>,
    pending_instructions: Vec<Instruction>,
}

impl ElevatorIter {
    fn start_at_floor(floor: i8, instructions: Vec<Instruction>) -> Option<Self> {
        if instructions.is_empty() {
            return Option::None;
        }

        let first_instruction = instructions[0];
        Option::Some(ElevatorIter {
            is_upwards: floor > first_instruction.source,
            turning_point: first_instruction.destination,
            in_progess_instructions: vec![],
            instructions
        })
    }
}

impl Iterator for ElevatorIter {
    type Item = i8;

    fn next(&mut self) -> Option<i8> {
        if !self.in_progess_instructions.is_empty() {
            return Option::Some(self.in_progess_instructions.pop());
        }
        if self.pending_instructions.is_empty() {
            return Option::None;
        }

        todo!("Implement");
    }

}

fn main() {}
