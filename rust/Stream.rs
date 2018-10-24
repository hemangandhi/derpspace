enum Stream<'a, T: 'a>{
    Data{value: &'a T, next: &'a Box<(FnMut()->&'a Stream<'a, T>)>},
    Nil
}

fn map<'a, 'b, A: 'a, B: 'a, F>(stream: Stream<A>, func: F) -> &'b Stream<B>
where 'a: 'b, F:  FnMut(&'a A) -> &'a B{
          match stream{
            Stream::Nil => &Stream::Nil,
            Stream::Data{value, next} => &Stream::Data{value: func(value), next: &Box::new(|| map(*next(), func)): Box<FnMut() -> &'a Stream<'a, A>>}
          }
}

fn main () {
}
