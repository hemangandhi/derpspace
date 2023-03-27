use std::borrow::Borrow;
use std::boxed::Box;
use std::str::FromStr;

const INPUT: &str = include_str!("day_7_input.txt");

enum FileSystem {
    Directory(String, Vec<Box<FileSystem>>),
    File(String, usize),
}

impl FileSystem {
    fn get_member_sizes(&self) -> Vec<(String, bool, usize)> {
        match self {
            FileSystem::File(path, sz) => vec![(path.clone(), false, *sz)],
            FileSystem::Directory(name, members) => {
                let mut sub_members: Vec<(String, bool, usize)> = members
                    .iter()
                    .flat_map(|path| path.get_member_sizes())
                    .collect();
                let total: usize = sub_members.iter().map(|(_p, _d, s)| *s).sum();
                sub_members.push((name.clone(), true, total));
                sub_members
            }
        }
    }
}

#[derive(Debug)]
enum CmdLnParseError {
    BadShellLine(String),
    BadLsOutput(String),
    InvalidFsState,
    BadCdCommand(String),
    InsufficientLines,
    FolderNotFound(String),
}

fn consume_ls_command<'a>(
    dir: &'a mut FileSystem,
    lines: &'a mut impl Iterator<Item = &'a str>,
) -> Result<bool, CmdLnParseError> {
    let folder = if let FileSystem::Directory(_, v) = dir {
        v
    } else {
        return Result::Err(CmdLnParseError::InvalidFsState);
    };
    while let Option::Some(line) = lines.next() {
        if line.starts_with("$") {
            return handle_shell_command(line, dir, lines);
        }
        if let Option::Some((sz_str, file_str)) = line.split_once(" ") {
            folder.push(Box::new(if sz_str == "dir" {
                FileSystem::Directory(file_str.to_string(), Vec::new())
            } else {
                let sz = if let Result::Ok(s) = sz_str.parse() {
                    s
                } else {
                    return Result::Err(CmdLnParseError::BadLsOutput(line.to_string()));
                };
                FileSystem::File(file_str.to_string(), sz)
            }));
        } else {
            return Result::Err(CmdLnParseError::BadLsOutput(line.to_string()));
        }
    }
    Result::Ok(false)
}

fn handle_shell_command<'a>(
    shell_cmd: &'a str,
    dir: &'a mut FileSystem,
    lines: &'a mut impl Iterator<Item = &'a str>,
) -> Result<bool, CmdLnParseError> {
    let cmd = if let Option::Some((prompt, cmd)) = shell_cmd.split_once(" ") {
        if prompt != "$" {
            return Result::Err(CmdLnParseError::BadShellLine(shell_cmd.to_string()));
        }
        cmd
    } else {
        return Result::Err(CmdLnParseError::BadShellLine(shell_cmd.to_string()));
    };
    if cmd.starts_with("ls") {
        let d = consume_ls_command(dir, lines);
        if d.unwrap_or(false) {
            if let Option::Some(next_command) = lines.next() {
                return handle_shell_command(next_command, dir, lines);
            }
            return Result::Ok(false);
        }
        return d;
    }
    let new_dir = if let Option::Some((cd, dir)) = cmd.split_once(" ") {
        if cd != "cd" {
            return Result::Err(CmdLnParseError::BadShellLine(shell_cmd.to_string()));
        }
        dir
    } else {
        return Result::Err(CmdLnParseError::BadShellLine(shell_cmd.to_string()));
    };
    if new_dir == ".." {
        return Result::Ok(true);
    }
    let folder = if let FileSystem::Directory(_, v) = dir {
        v
    } else {
        return Result::Err(CmdLnParseError::InvalidFsState);
    };
    let mut found_file = if let Option::Some(m) = folder
        .iter_mut()
        .filter(|box_memb| {
            if let FileSystem::Directory(n, _) = (**box_memb).borrow() {
                return n == new_dir;
            }
            false
        })
        .next()
    {
        m
    } else {
        return Result::Err(CmdLnParseError::FolderNotFound(new_dir.to_string()));
    };
    if let Option::Some(next_command) = lines.next() {
        let d = handle_shell_command(next_command, &mut found_file, lines);
        if d.unwrap_or(false) {
            if let Option::Some(next_command) = lines.next() {
                return handle_shell_command(next_command, dir, lines);
            }
        }
    }
    return Result::Ok(false);
}

impl FromStr for FileSystem {
    type Err = CmdLnParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut root_dir = FileSystem::Directory("/".to_string(), Vec::new());
        let mut lines = s.split("\n").skip(1);
        let second_line = if let Option::Some(l) = lines.next() {
            l
        } else {
            return Result::Err(CmdLnParseError::InsufficientLines);
        };
        handle_shell_command(second_line, &mut root_dir, &mut lines)?;
        Result::Ok(root_dir)
    }
}

fn main() {
    let fs: FileSystem = INPUT.parse().unwrap();
    let sizes = fs.get_member_sizes();
    println!(
        "{}",
        sizes
            .iter()
            .filter(|(_p, d, s)| *d && *s > 100000)
            .map(|(_p, _d, s)| *s)
            .sum::<usize>()
    );
}
