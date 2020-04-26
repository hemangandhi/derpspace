use std::marker::PhantomData;

pub trait Nat: Clone {
    fn reify() -> u32;
}

// Zero is a number
#[derive(Clone)]
pub struct ZNat;
impl Nat for ZNat {
    fn reify() -> u32 { 0 }
}

// 1 + a number is a number (the successor)
#[derive(Clone)]
pub struct SNat<A: Nat = ZNat>(PhantomData<A>);
impl<A: Nat> Nat for SNat<A> {
    fn reify() -> u32 {
        1 + A::reify()
    }
}

// Addition of natural numbers
pub trait AddNats<A: Nat>: Nat{
    type Sum: Nat;
}
impl<T: Nat> AddNats<T> for ZNat {
    type Sum = T;
}
impl<T: Nat, U: AddNats<T> + Nat> AddNats<T> for SNat<U> {
    type Sum = SNat<<U as AddNats<T>>::Sum>;
}

type Plus<X: Nat, Y: Nat> = <X as AddNats<Y>>::Sum;

// A list that knows its length as a type.
pub trait FixedList<'a, A: 'a, L: Nat> {
    fn reify(&self) -> Vec<A>;
    fn box_clone(&self) -> Box<dyn FixedList<'a, A, L> + 'a>;
}

impl<'a, A: 'a, L: Nat> Clone for Box<dyn FixedList<'a, A, L> + 'a> {
    fn clone(&self) -> Self {
	(*self).box_clone()
    }
}

pub struct EmptyList<A>(PhantomData<A>); /* surpress unused warning */

impl<'a, A: 'a + Clone> FixedList<'a, A, ZNat> for EmptyList<A> {
    fn reify(&self) -> Vec<A> { vec![] }
    fn box_clone(&self) -> Box<dyn FixedList<'a, A, ZNat> + 'a> {
	let Self(ph) = *self;
	Box::new(Self(ph))
    }
}

pub struct ConsList<'a, A: 'a + Clone, Lm1: Nat, Nxt: ?Sized + FixedList<'a, A, Lm1>>(A, Box<&'a Nxt>, PhantomData<Lm1>);

impl<'a, A: 'a + Clone, Lm1: 'static + Nat, Nxt: ?Sized + FixedList<'a, A, Lm1>> FixedList<'a, A, SNat<Lm1>> for ConsList<'a, A, Lm1, Nxt> {
    fn reify(&self) -> Vec<A> {
	let Self(curr, rest, _) = self;
	let mut v = rest.reify();
	v.insert(0, curr.clone());
	return v;
    }
    fn box_clone(&self) -> Box<dyn FixedList<'a, A, SNat<Lm1>> + 'a> {
	let Self(c, n, l) = *self;
	Box::new(Self(c, n, l))
    }
}

pub trait ConcatList<'a, A: 'a, L1: Nat + AddNats<L2>, L2: Nat + AddNats<L1>, X: 'a + FixedList<'a, A, L1>>: FixedList<'a, A, L2>{
    fn concat_lists(self, X) -> Box<dyn FixedList<'a, A, Plus<L2, L1>> + 'a>;
}
impl<'a, A, L1, X> ConcatList<'a, A, L1, ZNat, X> for EmptyList<A>
where A: 'a + Clone,
      L1: Nat + AddNats<ZNat>,
      X: 'a + FixedList<'a, A, L1> {
    fn concat_lists(self, x: X) -> Box<dyn FixedList<'a, A, Plus<ZNat, L1>> + 'a> { x.box_clone() }
}
impl<'a, A, L1, L2m1, X, Nxt> ConcatList<'a, A, L1, SNat<L2m1>, X> for ConsList<'a, A, L2m1, Nxt>
where A: 'a + Clone,
      L1: Nat + AddNats<SNat<L2m1>> + AddNats<L2m1>,
      L2m1: 'static + Nat + AddNats<L1>,
      X: 'a + FixedList<'a, A, L1>,
      Nxt: 'a + FixedList<'a, A, L2m1> + ConcatList<'a, A, L1, L2m1, X> {
    fn concat_lists(self, l: X) -> Box<dyn FixedList<'a, A, Plus<SNat<L2m1>, L1>> + 'a> {
	let Self(x, t_xs, ph) = self;
	let xs: Nxt = **t_xs;
	let catted: Box<dyn FixedList<'a, A, Plus<L2m1, L1>> + 'a> = xs.concat_lists(l);
	let size: PhantomData<<L2m1 as AddNats<L1>>::Sum>;
	Box::new(ConsList(x, Box::new(&*catted), size))
    }
}

fn main() {
    println!("{}", SNat::<ZNat>::reify());
    println!("{}", <SNat::<ZNat> as AddNats<SNat::<SNat::<ZNat>>>>::Sum::reify());
}
