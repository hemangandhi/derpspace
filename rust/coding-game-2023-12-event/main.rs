use std::collections::{HashMap, HashSet};
use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => {
        $x.trim().parse::<$t>().unwrap()
    };
}

#[derive(Clone, Copy)]
enum Instruction {
    Move(u32, u32, bool),
    Wait(bool),
}

fn print_instruction(i: Instruction) {
    match (i) {
        Instruction::Move(x, y, l) => println!("MOVE {} {} {}", x, y, if l { 1 } else { 0 }),
        Instruction::Wait(true) => println!("WAIT 1"),
        Instruction::Wait(false) => println!("WAIT 0"),
    }
}

#[derive(Default)]
struct Creature {
    id: i32,
    color: i32,
    c_type: i32,
    x: i32,
    y: i32,
    vx: i32,
    vy: i32,
}

#[derive(Default)]
struct Drone {
    id: i32,
    x: i32,
    y: i32,
    battery: i32,
}

#[derive(Default)]
struct Player {
    score: i32,
    drones: HashMap<i32, Drone>,
    scanned_creatures: HashSet<i32>,
}

impl Player {
    fn score_one_creature(
        &self,
        creature_id: i32,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
    ) -> i32 {
        let creature = creature_map.get(&creature_id).unwrap();
        let type_base_score = creature.c_type + 1;
        let has_first_creature_bonus = (&self.scanned_creatures.contains(&creature_id)
            || &foe.scanned_creatures.contains(&creature_id));

        let has_all_of_color = creature_map.iter().all(|(&k, &v)| {
            if v.color != creature.color {
                return false;
            }
            k == creature_id || self.scanned_creatures.contains(&k)
        });
        let foe_has_all_of_color = creature_map.iter().all(|(&k, &v)| {
            if v.color != creature.color {
                return false;
            }
            self.scanned_creatures.contains(&k)
        });

        let has_all_of_type = creature_map.iter().all(|(&k, &v)| {
            if v.c_type != creature.c_type {
                return false;
            }
            k == creature_id || self.scanned_creatures.contains(&k)
        });
        let foe_has_all_of_type = creature_map.iter().all(|(&k, &v)| {
            if v.c_type != creature.c_type {
                return false;
            }
            self.scanned_creatures.contains(&k)
        });

        let base = type_base_score * (if has_first_creature_bonus { 2 } else { 1 });
        let color_score =
            (if has_all_of_color { 3 } else { 0 }) * (if foe_has_all_of_color { 1 } else { 2 });
        let type_score =
            (if has_all_of_type { 3 } else { 0 }) * (if foe_has_all_of_type { 1 } else { 2 });
        base + color_score + type_score
    }

    fn score_scan(
        &self,
        scan: HashSet<i32>,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
    ) -> i32 {
        scan.iter()
            .map(|&c| self.score_one_creature(c, creature_map, foe))
            .sum()
    }
}

/**
 * Score points by scanning valuable fish faster than your opponent.
 **/
fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let creature_count = parse_input!(input_line, i32);
    let mut creature_map = HashMap::new();
    for i in 0..creature_count as usize {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let inputs = input_line.split(" ").collect::<Vec<_>>();
        let creature_id = parse_input!(inputs[0], i32);
        let color = parse_input!(inputs[1], i32);
        let _type = parse_input!(inputs[2], i32);
        creature_map.insert(
            creature_id,
            Creature {
                id: creature_id,
                color,
                c_type: _type,
                x: 0,
                y: 0,
                vx: 0,
                vy: 0,
            },
        );
    }

    let mut me = Player::default();
    let mut foe = Player::default();
    // game loop
    loop {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let my_score = parse_input!(input_line, i32);
        me.score = my_score;
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let foe_score = parse_input!(input_line, i32);
        foe.score = foe_score;
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let my_scan_count = parse_input!(input_line, i32);
        for i in 0..my_scan_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let creature_id = parse_input!(input_line, i32);
            me.scanned_creatures.insert(creature_id);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let foe_scan_count = parse_input!(input_line, i32);
        for i in 0..foe_scan_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let creature_id = parse_input!(input_line, i32);
            foe.scanned_creatures.insert(creature_id);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let my_drone_count = parse_input!(input_line, i32);
        for i in 0..my_drone_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let drone_id = parse_input!(inputs[0], i32);
            let mut drone = me.drones.entry(drone_id).or_insert(Drone::default());
            drone.x = parse_input!(inputs[1], i32);
            drone.y = parse_input!(inputs[2], i32);
            let emergency = parse_input!(inputs[3], i32);
            drone.battery = parse_input!(inputs[4], i32);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let foe_drone_count = parse_input!(input_line, i32);
        for i in 0..foe_drone_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let drone_id = parse_input!(inputs[0], i32);
            let mut drone = foe.drones.entry(drone_id).or_insert(Drone::default());
            drone.x = parse_input!(inputs[1], i32);
            drone.y = parse_input!(inputs[2], i32);
            let emergency = parse_input!(inputs[3], i32);
            drone.battery = parse_input!(inputs[4], i32);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let drone_scan_count = parse_input!(input_line, i32);
        for i in 0..drone_scan_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let drone_id = parse_input!(inputs[0], i32);
            let creature_id = parse_input!(inputs[1], i32);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let visible_creature_count = parse_input!(input_line, i32);
        for i in 0..visible_creature_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let creature_id = parse_input!(inputs[0], i32);
            let mut creature = creature_map.entry(creature_id).or_insert(Creature::default());
            creature.x = parse_input!(inputs[1], i32);
            creature.y = parse_input!(inputs[2], i32);
            creature.vx = parse_input!(inputs[3], i32);
            creature.vy = parse_input!(inputs[4], i32);
        }
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let radar_blip_count = parse_input!(input_line, i32);
        for i in 0..radar_blip_count as usize {
            let mut input_line = String::new();
            io::stdin().read_line(&mut input_line).unwrap();
            let inputs = input_line.split(" ").collect::<Vec<_>>();
            let drone_id = parse_input!(inputs[0], i32);
            let creature_id = parse_input!(inputs[1], i32);
            let radar = inputs[2].trim().to_string();
        }
        for i in 0..my_drone_count as usize {
            // Write an action using println!("message...");
            // To debug: eprintln!("Debug message...");

            println!("WAIT 1"); // MOVE <x> <y> <light (1|0)> | WAIT <light (1|0)>
        }
    }
}
