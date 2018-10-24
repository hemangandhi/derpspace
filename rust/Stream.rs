enum Stream<'a, T: 'a>{
    Data{value: &'a T, next: &'a Box<(FnMut()->&'a Stream<'a, T>)>},
    Nil
}

fn map<'a, A, B, F>(stream: &'a Stream<A>, func: F) -> &'a Stream<'a, B>
where F:  FnMut(&A) -> &B{
          match stream{
            Stream::Nil => &Stream::Nil,
            Stream::Data{value, next} => &Stream::Data{value: func(value), next: &Box::new(|| map(next(), func))}
          }
}

fn main () {
}
