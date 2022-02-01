use std::iter::FromIterator;

const INPUT: &str = "220D700071F39F9C6BC92D4A6713C737B3E98783004AC0169B4B99F93CFC31AC4D8A4BB89E9D654D216B80131DC0050B20043E27C1F83240086C468A311CC0188DB0BA12B00719221D3F7AF776DC5DE635094A7D2370082795A52911791ECB7EDA9CFD634BDED14030047C01498EE203931BF7256189A593005E116802D34673999A3A805126EB2B5BEEBB823CB561E9F2165492CE00E6918C011926CA005465B0BB2D85D700B675DA72DD7E9DBE377D62B27698F0D4BAD100735276B4B93C0FF002FF359F3BCFF0DC802ACC002CE3546B92FCB7590C380210523E180233FD21D0040001098ED076108002110960D45F988EB14D9D9802F232A32E802F2FDBEBA7D3B3B7FB06320132B0037700043224C5D8F2000844558C704A6FEAA800D2CFE27B921CA872003A90C6214D62DA8AA9009CF600B8803B10E144741006A1C47F85D29DCF7C9C40132680213037284B3D488640A1008A314BC3D86D9AB6492637D331003E79300012F9BDE8560F1009B32B09EC7FC0151006A0EC6082A0008744287511CC0269810987789132AC600BD802C00087C1D88D05C001088BF1BE284D298005FB1366B353798689D8A84D5194C017D005647181A931895D588E7736C6A5008200F0B802909F97B35897CFCBD9AC4A26DD880259A0037E49861F4E4349A6005CFAD180333E95281338A930EA400824981CC8A2804523AA6F5B3691CF5425B05B3D9AF8DD400F9EDA1100789800D2CBD30E32F4C3ACF52F9FF64326009D802733197392438BF22C52D5AD2D8524034E800C8B202F604008602A6CC00940256C008A9601FF8400D100240062F50038400970034003CE600C70C00F600760C00B98C563FB37CE4BD1BFA769839802F400F8C9CA79429B96E0A93FAE4A5F32201428401A8F508A1B0002131723B43400043618C2089E40143CBA748B3CE01C893C8904F4E1B2D300527AB63DA0091253929E42A53929E420";

#[derive(Debug)]
enum LengthType {
  Literal,
  Bits(usize),
  Packets(u32),
}


#[derive(Debug)]
struct Packet {
  version: u8,
  packet_type: u8,
  length: LengthType,
  subpackets: Vec<Packet>,
  value: u64,
}

#[derive(Debug)]
enum TransmissionParseResult{
  Ok(Packet),
  BadBit(u8),
  LengthBitsMismatch,
  StreamTruncated(usize)
}

fn intern_bits(it: &mut impl Iterator<Item=u8>) -> Result<u64, TransmissionParseResult> {
  let mut acc = 0u64;
  for bit in it {
    match bit {
      b'0' => {
        acc <<= 1;
      },
      b'1' => {
        acc <<= 1;
        acc |= 1;
      },
      _ => {
        return Result::Err(TransmissionParseResult::BadBit(bit));
      }
    }
  }
  return Result::Ok(acc);
}

fn take_n<T>(it: &mut impl Iterator<Item=T>, n: usize) -> Result<Vec<T>, TransmissionParseResult> {
  let mut v = vec![];
  for i in 0..n {
    if let Option::Some(t) = it.next() {
      v.push(t);
    } else {
      return Result::Err(TransmissionParseResult::StreamTruncated(n - i));
    }
  }
  return Result::Ok(v);
}

fn parse_literal(it: &mut impl Iterator<Item=u8>) -> Result<(u64, usize), TransmissionParseResult> {
  let mut more_chunks = true;
  let mut bits = vec![];
  let mut n_chunks = 0;
  while more_chunks {
    n_chunks += 1;
    let next_bits = take_n(it, 5)?;
    match next_bits[0] {
      b'0' => { more_chunks = false; },
      b'1' => { more_chunks = true; },
      x => {return Result::Err(TransmissionParseResult::BadBit(x))}
    }
    bits.extend(&mut next_bits.into_iter().skip(1));
  }
  let n_bits = bits.len();
  let b = intern_bits(&mut bits.into_iter()); 
  Result::Ok((b?, n_bits + n_chunks))
}

fn parse_literals_of_length<T: Iterator<Item=u8>>(
  len: &LengthType,
  it: &mut T,
  bit_counter: &mut usize,
  parser: &mut impl FnMut(&mut T, &mut usize) -> Result<Packet, TransmissionParseResult>)
-> Result<Vec<Packet>, TransmissionParseResult> {
  match *len {
    LengthType::Literal => {
      Result::Err(TransmissionParseResult::LengthBitsMismatch)
    },
    LengthType::Bits(b) => {
      let mut v = vec![];
      let init_bits = *bit_counter;
      while *bit_counter - init_bits < b {
        let next_literal = parser(it, bit_counter)?;
        v.push(next_literal);
      }
      if *bit_counter - init_bits != b {
        Result::Err(TransmissionParseResult::LengthBitsMismatch)
      } else {
        Result::Ok(v)
      }
    },
    LengthType::Packets(p) => {
      let mut v = vec![];
      for _i in 0..p {
        v.push(parser(it, bit_counter)?);
      }
      Result::Ok(v)
    }
  }
}

fn parse_packet(it: &mut impl Iterator<Item=u8>, bc: &mut usize) -> Result<Packet, TransmissionParseResult> {
  let version = intern_bits(&mut take_n(it, 3)?.into_iter())? as u8;
  let packet_type = intern_bits(&mut take_n(it, 3)?
                                        .into_iter())? as u8;
  *bc += 6;
  let length = if packet_type == 4 {
    LengthType::Literal
  } else {
    if let Option::Some(bit) = it.next() {
      match bit {
        b'0' => {
          *bc += 16;
          LengthType::Bits(intern_bits(&mut take_n(it, 15)?.into_iter())? as usize)
        }
        b'1' => {
          *bc += 12;
          LengthType::Packets(intern_bits(&mut take_n(it, 11)?.into_iter())? as u32)
        },
        _ => {
          return Result::Err(TransmissionParseResult::BadBit(bit));
        }
      }
    } else {
      return Result::Err(TransmissionParseResult::StreamTruncated(1))
    }
  };
  match length {
    LengthType::Literal => {
      let (value, n_bits) = parse_literal(it)?;
      *bc += n_bits;
      Result::Ok(Packet {
        version,
        packet_type,
        length: LengthType::Literal,
        subpackets: vec![],
        value,
      })
    },
    x => {
      let subpackets = parse_literals_of_length(&x, it, bc, &mut parse_packet)?;
      Result::Ok(Packet {
        version,
        packet_type,
        length: x,
        subpackets,
        value: 0u64,
      })
    }
  }
}

fn do_parse_transmission<T: IntoIterator<Item=u8>>(iit: T) -> Result<TransmissionParseResult, TransmissionParseResult> {
  let mut it = iit.into_iter();
  let mut bit_count = 0;
  let packet = parse_packet(&mut it, &mut bit_count)?;
  if bit_count % 4 != 0 {
    let pad = 4 - bit_count % 4;
    for i in 0..pad {
      if !it.next().is_some() {
        return Result::Err(TransmissionParseResult::StreamTruncated(pad - i));
      }
    }
  }
  Result::Ok(TransmissionParseResult::Ok(packet))
}

impl FromIterator<u8> for TransmissionParseResult {
  fn from_iter<T: IntoIterator<Item=u8>>(it: T) -> TransmissionParseResult {
    match do_parse_transmission(it) {
      Result::Ok(x) => x,
      Result::Err(x) => x
    }
  }
}

#[derive(Debug)]
enum PacketEvalError {
  UnknownType(u8),
  ArgsOutOfBounds
}

impl Packet {
  fn version_sum(&self) -> usize {
    (self.version as usize) + self.subpackets
      .iter()
      .map(|sp| sp.version_sum())
      .sum::<usize>()
  }

  fn eval(&self) -> Result<u64, PacketEvalError> {
    match self.packet_type {
      4 => Result::Ok(self.value),
      0 => Result::Ok(self.subpackets
                          .iter()
                          .map(|p| p.eval())
                          .collect::<Result<Vec<u64>, PacketEvalError>>()?
                          .iter()
                          .sum()),
      1 => Result::Ok(self.subpackets
                          .iter()
                          .map(|p| p.eval())
                          .collect::<Result<Vec<u64>, PacketEvalError>>()?
                          .iter()
                          .product()),
      2 => Result::Ok(*self.subpackets
                          .iter()
                          .map(|p| p.eval())
                          .collect::<Result<Vec<u64>, PacketEvalError>>()?
                          .iter()
                          .min()
                          .ok_or(PacketEvalError::ArgsOutOfBounds)?),
      3 => Result::Ok(*self.subpackets
                          .iter()
                          .map(|p| p.eval())
                          .collect::<Result<Vec<u64>, PacketEvalError>>()?
                          .iter()
                          .max()
                          .ok_or(PacketEvalError::ArgsOutOfBounds)?),
      5 => {
        if self.subpackets.len() != 2 {
          Result::Err(PacketEvalError::ArgsOutOfBounds)
        } else {
          Result::Ok(if self.subpackets[0].eval()? > self.subpackets[1].eval()? { 1 } else { 0 })
        }
      }
      6 => {
        if self.subpackets.len() != 2 {
          Result::Err(PacketEvalError::ArgsOutOfBounds)
        } else {
          Result::Ok(if self.subpackets[0].eval()? < self.subpackets[1].eval()? { 1 } else { 0 })
        }
      }
      7 => {
        if self.subpackets.len() != 2 {
          Result::Err(PacketEvalError::ArgsOutOfBounds)
        } else {
          Result::Ok(if self.subpackets[0].eval()? == self.subpackets[1].eval()? { 1 } else { 0 })
        }
      },
      x => Result::Err(PacketEvalError::UnknownType(x))
    }
  }
}

fn bits_of_str(s: &str) -> impl Iterator<Item=u8> + '_ {
  const HEXMAP: [(u8, &str); 16] = [
    (b'0', "0000"),
    (b'1', "0001"),
    (b'2', "0010"),
    (b'3', "0011"),
    (b'4', "0100"),
    (b'5', "0101"),
    (b'6', "0110"),
    (b'7', "0111"),
    (b'8', "1000"),
    (b'9', "1001"),
    (b'A', "1010"),
    (b'B', "1011"),
    (b'C', "1100"),
    (b'D', "1101"),
    (b'E', "1110"),
    (b'F', "1111"),
  ];
  s.bytes()
   .flat_map(|b| HEXMAP.iter()
                   .filter(|(bb, _s)| *bb == b)
                   .next()
                   .unwrap()
                   .1
                   .bytes())
}

fn main() {
  /*
  println!("{:?}", bits_of_str("D2FE28").collect::<TransmissionParseResult>());
  println!("{:?}", bits_of_str("EE00D40C823060").collect::<TransmissionParseResult>());
  println!("{:?}", bits_of_str("8A004A801A8002F478").collect::<TransmissionParseResult>());
  println!("{:?}", bits_of_str("38006F45291200").collect::<TransmissionParseResult>());
  println!("{:?}", bits_of_str("620080001611562C8802118E34").collect::<TransmissionParseResult>());
  println!("{:?}", bits_of_str("A0016C880162017C3686B18A3D4780").collect::<TransmissionParseResult>());
  */
  let parsed: TransmissionParseResult = bits_of_str(INPUT).collect();
  if let TransmissionParseResult::Ok(pack) = parsed {
    println!("Part 1: {}", pack.version_sum());
    println!("Part 2: {}", pack.eval().unwrap());
  } else {
    println!("Error: {:?}", parsed);
  }
}