use std::rc::Rc;

enum Stream<'a, T> {
    Data {
        value: T,
        next: Rc<dyn Fn() -> &'a Stream<'a, T> + 'a>,
    },
    Nil,
}

impl<T: Clone> Clone for Stream<'_, T> {
    fn clone(&self) -> Self {
        match self {
            Stream::Nil => Stream::Nil,
            Stream::Data { value, next } => Stream::Data {
                value: value.clone(),
                next: next.clone(),
            },
        }
    }
}

fn map<'a, A: 'a, B>(stream: &'a Stream<'a, A>, func: &'a impl Fn(&A) -> B) -> &'a Stream<'a, B> {
    match stream {
        Stream::Nil => &Stream::Nil,
        Stream::Data { value, next } => &Stream::Data {
            value: func(value),
            next: Rc::new(move || map(&next(), func)),
        },
    }
}

fn concat<'a, A: Clone + 'a>(stream1: &'a Stream<'a, A>, stream2: &'a Stream<'a, A>) -> &'a Stream<'a, A> {
    match stream1 {
        Stream::Nil => stream2,
        Stream::Data { value, next } => &Stream::Data {
            value: value.clone(),
            next: Rc::new(move || concat(next(), stream2)),
        },
    }
}

fn singleton_stream<'a, A>(a: A) -> Stream<'a, A> {
    Stream::Data {
        value: a,
        next: Rc::new(|| &Stream::Nil),
    }
}

fn subsets<'a, A: 'a + Clone>(stream: &'a Stream<'a, A>) -> &'a Stream<'a, Stream<'a, A>> {
    match stream {
        Stream::Nil => &singleton_stream(Stream::Nil),
        Stream::Data { value, next } => {
            let others = subsets(next());
            concat(
                others,
                map(others, &move |st| concat(&singleton_stream(value.clone()), st)),
            )
        }
    }
}

fn main() {}
