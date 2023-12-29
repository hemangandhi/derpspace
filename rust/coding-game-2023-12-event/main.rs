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

impl Instruction {
    fn print_instruction(self) {
        match self {
            Instruction::Move(x, y, l) => println!("MOVE {} {} {}", x, y, if l { 1 } else { 0 }),
            Instruction::Wait(true) => println!("WAIT 1"),
            Instruction::Wait(false) => println!("WAIT 0"),
        }
    }

    fn with_big_light(self, big_light: bool) -> Self {
        match self {
            Instruction::Move(x, y, _l) => Instruction::Move(x, y, big_light),
            Instruction::Wait(_b) => Instruction::Wait(big_light)
        }
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

impl Drone {
    fn get_scan(&self, creature_map: &HashMap<i32, Creature>, large: bool) -> HashSet<i32> {
        let radius = if large { 2000 } else { 800 };
        creature_map
            .iter()
            .filter_map(|(&k, v)| {
                let fx = v.x + v.vx;
                let fy = v.y + v.vy;
                let dist = (fx - self.x) * (fx - self.x) + (fy - self.y) * (fy - self.y);
                if dist <= radius * radius {
                    Some(k)
                } else {
                    None
                }
            })
            .collect()
    }
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
        creature: &Creature,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
    ) -> i32 {
        let type_base_score = creature.c_type + 1;
        let has_first_creature_bonus = self.scanned_creatures.contains(&creature.id)
            || foe.scanned_creatures.contains(&creature.id);

        let has_all_of_color = creature_map.iter().all(|(&k, v)| {
            if v.color != creature.color {
                return false;
            }
            k == creature.id || self.scanned_creatures.contains(&k)
        });
        let foe_has_all_of_color = creature_map.iter().all(|(&k, v)| {
            if v.color != creature.color {
                return false;
            }
            self.scanned_creatures.contains(&k)
        });

        let has_all_of_type = creature_map.iter().all(|(&k, v)| {
            if v.c_type != creature.c_type {
                return false;
            }
            k == creature.id || self.scanned_creatures.contains(&k)
        });
        let foe_has_all_of_type = creature_map.iter().all(|(&k, v)| {
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
            .map(|&c| self.score_one_creature(creature_map.get(&c).unwrap(), creature_map, foe))
            .sum()
    }

    fn get_closest_drone_dist(&self, creature: &Creature) -> i32 {
        let cx = creature.x + creature.vx;
        let cy = creature.y + creature.vy;
        self.drones
            .iter()
            .map(|(&_k, v)| (v.x - cx) * (v.x - cx) + (v.y - cy) * (v.y - cy))
            .min()
            .unwrap()
    }

    fn best_move_centroid(
        &self,
        drone_id: i32,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
        creature_set: Option<HashSet<i32>>,
    ) -> Instruction {
        let drone = self.drones.get(&drone_id).unwrap();
        let drone_fish_map = creature_map
            .iter()
            .map(|(&k, v)| {
                let fx = v.x + v.vx;
                let fy = v.y + v.vy;
                let dist = (drone.x - fx) * (drone.x - fx) + (drone.y - fy) * (drone.y - fy);
                (k, dist)
            })
            .collect::<HashMap<i32, i32>>();
        let furthest_fish = drone_fish_map.values().max().unwrap();
        let heuristic_map = creature_map
            .iter()
            .map(|(&k, v)| {
                let my_dist = furthest_fish - drone_fish_map.get(&k).unwrap();
                let foe_dist = foe.get_closest_drone_dist(v);
                let score = self.score_one_creature(v, creature_map, foe);
                (k, score * 1000 + my_dist + foe_dist)
            })
            .collect::<HashMap<i32, i32>>();
        let total_heuristic: i32 = heuristic_map.values().sum();
        let (px, py) = creature_map
            .iter()
            .filter_map(|(&k, v)| {
                if !creature_set
                    .as_ref()
                    .and_then(|s| Some(s.contains(&k)))
                    .unwrap_or(true)
                {
                    return None;
                }
                let (fx, fy) = (v.x + v.vx, v.y + v.vy);
                let score = (*heuristic_map.get(&k).unwrap() as f64) / (total_heuristic as f64);
                Some(((fx as f64) * score, (fy as f64) * score))
            })
            .fold((0., 0.), |(tx, ty), (nx, ny)| (tx + nx, ty + ny));
        Instruction::Move(px as u32, py as u32, false)
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
            let mut creature = creature_map
                .entry(creature_id)
                .or_insert(Creature::default());
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
            // TODO: understand what order the game actually expects drone instructions and what
            // order it provides the drone states in.
            let drone = me
                .drones
                .values()
                .next()
                .unwrap();
            let reachable_creatures = drone.get_scan(&creature_map, false);
            let mut use_big_light = false;
            if reachable_creatures.is_empty() && drone.battery > 10 {
                let mut use_big_light = true;
                let reachable_creatures = drone.get_scan(&creature_map, false);
            }
            me.best_move_centroid(
                *me.drones.keys().next().unwrap(),
                &creature_map,
                &foe,
                if reachable_creatures.is_empty() {
                    None
                } else {
                    Some(reachable_creatures)
                },
            )
            .with_big_light(use_big_light)
            .print_instruction();
        }
    }
}
