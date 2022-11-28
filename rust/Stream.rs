use std::iter::FromIterator;
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

fn filter_map<A: 'static, B: 'static>(
    stream: Stream<A>,
    func: Rc<dyn Fn(A) -> Option<B>>,
) -> Stream<B> {
    match stream {
        Stream::Nil => Stream::Nil,
        Stream::Data { value, next } => match func(value) {
            Option::Some(val) => Stream::Data {
                value: val,
                next: Rc::new(move || filter_map(next(), func.clone())),
            },
            Option::None => filter_map(next(), func.clone()),
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
                map(
                    others,
                    Rc::new(move |st| concat(singleton_stream(value.clone()), st)),
                ),
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

impl<T: 'static + Clone> FromIterator<T> for Stream<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        iter.into_iter().fold(Stream::Nil, |acc, val| {
            concat(singleton_stream(val.clone()), acc)
        })
    }
}

fn increment_infinite_binary_num(prev: Stream<bool>) -> Stream<bool> {
    match prev {
        Stream::Nil => singleton_stream(true),
        Stream::Data { value: true, next } => concat(
            singleton_stream(false),
            increment_infinite_binary_num(next()),
        ),
        Stream::Data { value: false, next } => concat(singleton_stream(true), next()),
    }
}

fn iterate_fn_stream<T: 'static + Clone>(initial: T, update: Rc<dyn Fn(T) -> T>) -> Stream<T> {
    Stream::Data {
        value: initial.clone(),
        next: Rc::new(move || iterate_fn_stream(update(initial.clone()), update.clone())),
    }
}

fn zip_streams<A: 'static, B: 'static>(left: Stream<A>, right: Stream<B>) -> Stream<(A, B)> {
    match left {
        Stream::Nil => Stream::Nil,
        Stream::Data {
            value: lv,
            next: ln,
        } => match right {
            Stream::Nil => Stream::Nil,
            Stream::Data {
                value: rv,
                next: rn,
            } => Stream::Data {
                value: (lv, rv),
                next: Rc::new(move || zip_streams(ln(), rn())),
            },
        },
    }
}

fn large_finite_subsets<A: 'static + Clone>(set: Stream<A>) -> Stream<Stream<A>> {
    let nats = iterate_fn_stream(Stream::Nil, Rc::new(increment_infinite_binary_num));
    map(
        nats,
        Rc::new(move |nat| {
            filter_map(
                zip_streams(set.clone(), nat),
                Rc::new(|(x, n)| if n { Option::Some(x) } else { Option::None }),
            )
        }),
    )
}

fn main() {
    let strm: Stream<&i32> = [1i32, 2i32, 3i32, 4i32, 5i32].iter().collect();
    let subs: Vec<Vec<&i32>> = map(
        subsets(strm.clone()),
        Rc::new(|st| st.into_iter().collect()),
    )
    .into_iter()
    .collect();
    println!("{:#?}", subs);
    let subs2: Vec<Vec<&i32>> = map(
        large_finite_subsets(strm),
        Rc::new(|sub| sub.into_iter().collect()),
    )
    .into_iter()
    .take(33)
    .collect();
    println!("{:#?}", subs2);
}
