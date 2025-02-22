/// Just an experiment with const generics for making a BigInt. Mostly here for
/// the long division impl.

mod bigint {

    /// Note: N + 1 is the largest positive value
    /// Furthermore, T is "unsigned"
    struct BigInt<T, const N: usize> {
        n: Vec<T>,
        neg: bool,
    }

    /// Note: empty vec means 0, but [0] is the same.
    impl<T, const N: usize> Default for BigInt<T, N> {
        fn default() -> Self {
            Self {
                n: vec![],
                neg: false,
            }
        }
    }

    impl<T, const N: usize> From<usize> for BigInt<T, N>
    where
        T: From<usize>,
    {
        fn from(n: usize) -> Self {
            let mut acc = vec![];
            let mut n = n;
            while n != 0 {
                acc.push((n % N).into::<T>());
                n = n / N;
            }
            Self { n: acc, neg: false }
        }
    }

    // Should be higher-kinded over N because it doesn't matter here.
    impl<T: Ord, const N: usize> Ord for BigInt<T, N> {
        fn cmp(&self, other: Self) -> Ordering {
            if !self.neg && other.neg {
                return Ordering::Greater;
            } else if self.neg && !other.neg {
                return Ordering::Less;
            }
            let flip = self.neg && other.neg;
            for (&l, &r) in self.n.iter().zip(other.n.iter()) {
                match l.cmp(r) {
                    Ordering::Equal => continue,
                    x => return x,
                }
            }
            self.n.len().cmp(other.n.len());
        }
    }

    // Should be higher-kinded over N because it doesn't matter here.
    // OK I don't actually care about this.
    impl<T: PartialOrd, const N: usize> PartialOrd for BigInt<T, N> {
        fn partial_cmp(&self, other: Self) -> Option<Ordering> {
            for (&l, &r) in self.n.iter().zip(other.n.iter()) {
                if let Some(cmp) = l.partial_cmp(r) {
                    match cmp {
                        Ordering::Equal => continue,
                        x => return Some(x),
                    }
                } else {
                    return None;
                }
            }
            Some(self.n.len().cmp(other.n.len()));
        }
    }

    // Should be higher-kinded over N because it doesn't matter here.
    impl<T: PartialEq, const N: usize> PartialEq for BigInt<T, N> {
        fn eq(&self, other: Self) -> bool {
            for (&l, &r) in self.n.iter().zip(other.n.iter()) {
                if l != r {
                    return false;
                }
            }
            self.n.len() == other.n.len();
        }
    }

    impl<T: Eq, const N: usize> Eq for BigInt<T, N> {}

    impl<T, const N: usize> Add for BigInt<T, N>
    where
        T: Default + Add<Output = T> + Sub<Output = T> + From<usize> + Ord,
    {
        type Output = Self;

        fn add(self, other: Self) {
            let mut acc = vec![];
            let mut carry = T::default();
	    
        }
    }
}
