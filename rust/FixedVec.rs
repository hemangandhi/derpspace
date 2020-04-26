use std::marker::PhantomData;

pub trait Nat {
    fn reify() -> u32;
}

// Zero is a number
pub struct ZNat;
impl Nat for ZNat {
    fn reify() -> u32 { 0 }
}

// 1 + a number is a number (the successor)
pub struct SNat<A: Nat = ZNat>(A);
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
pub trait FixedList<A, L: Nat> {
    fn reify(&self) -> Vec<&A>;
}

pub struct EmptyList<A>(PhantomData<A>); /* surpress unused warning */
impl<A> FixedList<A, ZNat> for EmptyList<A> {
    fn reify(&self) -> Vec<&A> { vec![] }
}

pub struct ConsList<A, Lm1: Nat>(A, Box<dyn FixedList<A, Lm1>>);
impl<A, Lm1: Nat> FixedList<A, SNat<Lm1>> for ConsList<A, Lm1> {
    fn reify(&self) -> Vec<&A> {
	let Self(curr, rest) = self;
	let mut v = rest.reify();
	v.insert(0, curr);
	return v;
    }
}

// TODO(to make this build): need an alterate bound to account for the fact that a ConsList<A, n> is a FixedList<A, succ n>.
pub trait ConcatList<A, L1: Nat + AddNats<L2>, L2: Nat + AddNats<L1>, X: FixedList<A, L1>>: FixedList<A, L2>{
    fn concat_lists(self, X) -> Box<dyn FixedList<A, Plus<L2, L1>>>;
}
impl<A, L1, X> ConcatList<A, L1, ZNat, X> for EmptyList<A>
where L1: Nat + AddNats<ZNat>,
      X: FixedList<A, L1> {
    fn concat_lists(self, x: X) -> Box<dyn FixedList<A, Plus<ZNat, L1>>> { Box::new(x) }
}
impl<A, L1, L2m1, X> ConcatList<A, L1, SNat<L2m1>, X> for ConsList<A, L2m1>
where L1: Nat + AddNats<SNat<L2m1>>, L2m1: Nat + AddNats<L1>,
      X: FixedList<A, L1> {
    fn concat_lists(self, l: X) -> Box<dyn FixedList<A, Plus<SNat<L2m1>, L1>>> {
	let Self(x, xs) = self;
	Box::new(ConsList(x, xs.concat_lists(l)))
    }
}

fn main() {
    println!("{}", SNat::<ZNat>::reify());
    println!("{}", <SNat::<ZNat> as AddNats<SNat::<SNat::<ZNat>>>>::Sum::reify());
}
