use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::io;

macro_rules! parse_input {
    ($x:expr, $t:ident) => {
        $x.trim().parse::<$t>().unwrap()
    };
}

#[derive(Clone, Copy)]
enum Instruction {
    Move(i32, i32, bool),
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

    fn is_large_scan(&self) -> bool {
        match *self {
            Instruction::Move(x, y, l) => l,
            Instruction::Wait(l) => l,
        }
    }

    fn with_light(self, big_light: bool) -> Self {
        match self {
            Instruction::Move(x, y, _l) => Instruction::Move(x, y, big_light),
            Instruction::Wait(_b) => Instruction::Wait(big_light),
        }
    }

    fn flip_light_for_drone_greedily(
        self,
        drone: &Drone,
        creature_map: &HashMap<i32, Creature>,
        me: &Player,
        foe: &Player,
    ) -> Self {
        if drone.battery < 5 {
            return self.with_light(false);
        }
        let new_point = drone.get_position_after_instruction(&self);
        let dim_score = me.score_scan(
            Drone::get_scan_from_point(new_point, creature_map, false),
            creature_map,
            foe,
        );
        let bright_score = me.score_scan(
            Drone::get_scan_from_point(new_point, creature_map, true),
            creature_map,
            foe,
        );
        if (bright_score - dim_score) > (3 - drone.battery / 5) {
            self.with_light(true)
        } else {
            self.with_light(false)
        }
    }
}

enum RadarBlip {
    TopLeftOf(i32, i32),
    TopRightOf(i32, i32),
    BottomLeftOf(i32, i32),
    BottomRightOf(i32, i32),
}

enum CreatuePositionData {
    None,
    KnownPosition{
        position: (i32, i32),
        velocity: (i32, i32)
    },
    PredictedPosition{
        position: (i32, i32),
        velocity: (i32, i32),
        supporting_blips: Vec<RadarBlip>
    },
    RadarBlips(Vec<RadarBlip>)
}

impl Default for CreatuePositionData {
    fn default() {
        CreatuePositionData::None
    }
}

impl CreatuePositionData {
    fn get_next_epoch_position(&self) -> Option<(i32, i32)> {
        if let CreatuePositionData::KnownPosition { position: (x, y), velocity: (vx, vy) } = self {
            Some((x + vx, y + vy))
        }
        None
    }
}

#[derive(Default)]
struct Creature {
    id: i32,
    color: i32,
    c_type: i32,
    latest_position: CreatuePositionData
}

#[derive(Default)]
struct Drone {
    id: i32,
    x: i32,
    y: i32,
    battery: i32,
}

impl Drone {
    fn get_scan_from_point(
        (x, y): (i32, i32),
        creature_map: &HashMap<i32, Creature>,
        large: bool,
    ) -> HashSet<i32> {
        let radius = if large { 2000 } else { 800 };
        creature_map
            .iter()
            .filter_map(|(&k, v)| {
                let (fx, fy) = v.latest_position.get_next_epoch_position()?;
                let dist = (fx - x) * (fx - x) + (fy - y) * (fy - y);
                if dist <= radius * radius {
                    Some(k)
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_scan(&self, creature_map: &HashMap<i32, Creature>, large: bool) -> HashSet<i32> {
        Drone::get_scan_from_point((self.x, self.y), creature_map, large)
    }

    fn get_position_after_instruction(&self, i: &Instruction) -> (i32, i32) {
        match i {
            Instruction::Wait(_l) => (self.x, self.y + 300),
            Instruction::Move(x, y, _l) => {
                let vect = ((*x as i32) - self.x, (*y as i32) - self.y);
                let d_sq = vect.1 * vect.1 + vect.0 * vect.0;
                if d_sq <= 600 * 600 {
                    return (*x as i32, *y as i32);
                }
                let l2 = (d_sq as f64).sqrt();
                let scaled_vec = (
                    (vect.0 as f64) / (l2 * 600.0),
                    (vect.1 as f64) / (l2 * 600.0),
                );
                (
                    (*x as i32) + (scaled_vec.0 as i32),
                    (*y as i32) + (scaled_vec.1 as i32),
                )
            }
        }
    }

    fn get_reachable_fish<'a>(
        &self,
        creature_map: &'a HashMap<i32, Creature>,
        large: bool,
    ) -> impl Iterator<Item = &'a Creature> {
        let radius = if large { 2000 } else { 800 } + 600;
        // Copy out of &self so that &self's lifetime isn't needed in the return type.
        let sx = self.x;
        let sy = self.y;
        creature_map.values().filter_map(move |v| {
            let (fx, fy) = v.latest_position.get_next_epoch_position()?;
            let dist = (fx - sx) * (fx - sx) + (fy - sy) * (fy - sy);
            if dist <= radius * radius {
                Some(v)
            } else {
                None
            }
        })
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
        already_scanned: &HashSet<i32>,
    ) -> i32 {
        if already_scanned.contains(&creature.id) || self.scanned_creatures.contains(&creature.id) {
            return 0;
        }
        let type_base_score = creature.c_type + 1;
        let has_first_creature_bonus = !self.scanned_creatures.contains(&creature.id)
            && !foe.scanned_creatures.contains(&creature.id)
            && !already_scanned.contains(&creature.id);

        let has_all_of_color = creature_map.iter().all(|(&k, v)| {
            if v.color != creature.color {
                return true;
            }
            k == creature.id || self.scanned_creatures.contains(&k)
        });
        let foe_has_all_of_color = creature_map.iter().all(|(&k, v)| {
            if v.color != creature.color {
                return true;
            }
            foe.scanned_creatures.contains(&k)
        });

        let has_all_of_type = creature_map.iter().all(|(&k, v)| {
            if v.c_type != creature.c_type {
                return true;
            }
            k == creature.id || self.scanned_creatures.contains(&k) || already_scanned.contains(&k)
        });
        let foe_has_all_of_type = creature_map.iter().all(|(&k, v)| {
            if v.c_type != creature.c_type {
                return true;
            }
            foe.scanned_creatures.contains(&k)
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
            .fold((HashSet::new(), 0), |(mut ss, total), &c| {
                let addend =
                    self.score_one_creature(creature_map.get(&c).unwrap(), creature_map, foe, &ss);
                ss.insert(c);
                (ss, total + addend)
            })
            .1
    }

    fn get_closest_drone_dist(&self, creature: &Creature) -> Option<i32> {
        let (cx, cy) = creature.latest_position.get_next_epoch_position();
        let cx = creature.x + creature.vx;
        let cy = creature.y + creature.vy;
        self.drones
            .iter()
            map(|(&_k, v)| (v.x - cx) * (v.x - cx) + (v.y - cy) * (v.y - cy))
            .min()
    }

    // TODO: we actually will have to worry about allocating drones.
    fn best_move_greedy_fish(
        &self,
        drone: &Drone,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
    ) -> Instruction {
        creature_map
            .values()
            .max_by_key(|c| {
                let fx = c.x + c.vx;
                let fy = c.y + c.vy;
                let dist = (drone.x - fx) * (drone.x - fx) + (drone.y - fy) * (drone.y - fy);
                (self.score_one_creature(c, creature_map, foe, &HashSet::default()), -dist)
            })
            .and_then(|c| {
                eprintln!("Thinking {} (at ({}, {})) is best", c.id, c.x, c.y);
                Some(Instruction::Move(
                    (c.x + c.vx).try_into().unwrap_or(0),
                    (c.y + c.vy).try_into().unwrap_or(0),
                    false,
                ))
            })
            .unwrap()
    }

    fn best_move_greedy_reachable_fish(
        &self,
        drone_id: i32,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
        large_scan: bool,
    ) -> Option<Instruction> {
        self.drones
            .get(&drone_id)
            .unwrap()
            .get_reachable_fish(creature_map, large_scan)
            .max_by_key(|v| self.score_one_creature(v, creature_map, foe, &HashSet::default()))
            .and_then(|c| {
                Some(Instruction::Move(
                    (c.x + c.vx).try_into().unwrap_or(0),
                    (c.y + c.vy).try_into().unwrap_or(0),
                    large_scan,
                ))
            })
    }

    // There are some flaws with this as a heuristic:
    // 1) in general the centriod is meaningless -- it doesn't mean you can actually scan any fish.
    // 2) adding the filter for nearby fish causes a lot of flip-flops in the center point without
    //    the drone actually reaching the fish.
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
                let score = self.score_one_creature(v, creature_map, foe, &HashSet::new());
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
        Instruction::Move(px as i32, py as i32, false)
    }

    fn instruct_drone(
        &self,
        drone_id: usize,
        creature_map: &HashMap<i32, Creature>,
        foe: &Self,
    ) -> Instruction {
        // TODO: understand what order the game actually expects drone instructions and what
        // order it provides the drone states in.
        let (drone_key, drone) = self.drones.iter().next().unwrap();
        let best_move = self
            .best_move_greedy_fish(drone, &creature_map, &foe)
            .flip_light_for_drone_greedily(drone, creature_map, self, foe);
        if let Some(greedy_move) = self
            .best_move_greedy_reachable_fish(*drone_key, &creature_map, &foe, false)
            .or_else(|| {
                if drone.battery < 5 {
                    None
                } else {
                    self.best_move_greedy_reachable_fish(*drone_key, &creature_map, &foe, true)
                }
            })
        {
            let expected_greedy_score = self.score_scan(
                Drone::get_scan_from_point(
                    drone.get_position_after_instruction(&greedy_move),
                    creature_map,
                    greedy_move.is_large_scan(),
                ),
                creature_map,
                foe,
            );
            let expected_fish_score = self.score_scan(
                Drone::get_scan_from_point(
                    drone.get_position_after_instruction(&best_move),
                    creature_map,
                    best_move.is_large_scan(),
                ),
                creature_map,
                foe,
            );
            if expected_greedy_score > expected_fish_score {
                eprintln!("Local greed trumps global");
                return greedy_move;
            }
        }
        best_move
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
                latest_position: CreatuePositionData::None
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
            // Bad old centroid ideas
            //            let reachable_creatures = drone.get_scan(&creature_map, false);
            //            let mut use_big_light = false;
            //            if reachable_creatures.is_empty() && drone.battery > 10 {
            //                use_big_light = true;
            //                reachable_creatures = drone.get_scan(&creature_map, true);
            //            }
            //            me.best_move_centroid(
            //                *me.drones.keys().next().unwrap(),
            //                &creature_map,
            //                &foe,
            //                if reachable_creatures.is_empty() {
            //                    None
            //                } else {
            //                    Some(reachable_creatures)
            //                },
            //            )
            //            .with_big_light(use_big_light)
            //            .print_instruction();

            me.instruct_drone(i, &creature_map, &foe)
                .print_instruction();
        }
    }
}
