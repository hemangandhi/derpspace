extern crate rdxsort;

use rdxsort::*;

fn main() {
    let mut data = vec![9, 0, 2, 1, 0];
    data.rdxsort();
    println!("{:?}", data);
}
