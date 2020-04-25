use std::marker::PhantomData;

pub trait Nat {
    fn reify(self) -> u32;
}

// Zero is a number
pub struct ZNat;
impl Nat for ZNat {
     fn reify(self) -> u32 { 0 }
}

// 1 + a number is a number (the successor)
pub struct SNat<A: Nat = ZNat>(A);
impl<A: Nat> Nat for SNat<A> {
    fn reify(self) -> u32 {
	let Self(n) = self;
        1 + n.reify()
    }
}

// A list that knows its length as a type.
pub trait FixedList<A, L: Nat> {
    fn reify(self) -> Vec<A>;
}

pub struct EmptyList<A>(PhantomData<A>); /* surpress unused warning */
impl<A> FixedList<A, ZNat> for EmptyList<A> {
    fn reify(self) -> Vec<A> { vec![] }
}

pub struct ConsList<A, Lm1: Nat>(A, SNat<Lm1>, Box<dyn FixedList<A, Lm1>>);
impl<A, Lm1: Nat> FixedList<A, SNat<Lm1>> for ConsList<A, Lm1> {
    fn reify(self) -> Vec<A> {
	let Self(curr, _, rest) = self;
	let mut v = rest.reify();
	v.insert(0, curr);
	return v;
    }
}
