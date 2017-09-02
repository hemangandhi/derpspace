enum Stream<T, N: Fn()->Stream<T, N>>{
    Data{value: T, next: N},
    Nil
};

fn<A, B, F, F1, F2> map(stream: Stream<A, F1>, func: F) -> Stream<B, F2>
where F:  Fn (A) -> B,
      F1: Fn ()  -> Stream<A, F1>,
      F2: Fn ()  -> Stream<B, F2>{
}

fn main () {

}
