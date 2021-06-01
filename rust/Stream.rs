use std::rc::Rc;

enum Stream<T> {
    Data {
        value: T,
        next: Rc<dyn Fn() -> Stream<T>>,
    },
    Nil,
}

impl<T: Clone> Clone for Stream<T> {
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

fn map<A: 'static, B: 'static>(stream: Stream<A>, func: Rc<dyn Fn(A) -> B>) -> Stream<B> {
    match stream {
        Stream::Nil => Stream::Nil,
        Stream::Data { value, next } => Stream::Data {
            value: func(value),
            next: Rc::new(move || map(next(), func.clone())),
        },
    }
}

fn concat<A: 'static + Clone>(stream1: Stream<A>, stream2: Stream<A>) -> Stream<A> {
    match stream1 {
        Stream::Nil => stream2,
        Stream::Data { value, next } => Stream::Data {
            value: value.clone(),
            next: Rc::new(move || concat(next(), stream2.clone())),
        },
    }
}


fn singleton_stream<A>(a: A) -> Stream<A> {
    Stream::Data {
        value: a,
        next: Rc::new(|| Stream::Nil),
    }
}


fn subsets<A: 'static + Clone>(stream: Stream<A>) -> Stream<Stream<A>> {
    match stream {
        Stream::Nil => singleton_stream(Stream::Nil),
        Stream::Data { value, next } => {
            let others = subsets(next());
            concat(
                others.clone(),
                map(others, Rc::new(move |st| concat(singleton_stream(value.clone()), st))),
            )
        }
    }
}

struct IterStream<T> {
    val: Stream<T>,
}

impl<T: Clone> IntoIterator for Stream<T> {
    type Item = T;
    type IntoIter = IterStream<T>;

    fn into_iter(self) -> IterStream<T> {
        IterStream { val: self }
    }
}

impl<T: Clone> Iterator for IterStream<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if let Stream::Data { value, next } = &self.val {
            let rv = value.clone();
            self.val = next();
            return Option::Some(rv);
        }
        Option::None
    }
}

fn stream_of_iter<T: 'static + Clone>(it: impl Iterator<Item = T>) -> Stream<T> {
    it.fold(Stream::Nil, |acc, val| concat(singleton_stream(val.clone()), acc))
}

fn main() {
    let strm: Stream<&i32> = stream_of_iter([1i32, 2i32, 3i32, 4i32, 5i32].iter());
    let subs: Vec<Vec<&i32>> = map(
        subsets(strm),
        Rc::new(|st| st.into_iter().collect()),
    )
    .into_iter()
    .collect();
    println!("{:#?}", subs);
}