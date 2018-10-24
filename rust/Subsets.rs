

struct Subsets<A>{
    counter: usize,
    data: Vec<A>
}

impl<A> Subsets<A>{
    fn new(data: Vec<A>) -> Subsets<A> {
        Subsets{
            counter: 0,
            data: data
        }
    }
}

impl<A> Iterator for Subsets<A>
where A: Clone{
    type Item = Vec<A>;
    fn next(&mut self) -> Option<Vec<A>>{
        if self.counter >= 2_usize.pow(self.data.len() as u32) {
            Option::None
        }else{
            let mut i = self.counter;
            let mut rv = Vec::new();
            let mut j = 0;
            while i > 0 {
                if i % 2 == 1 {
                    rv.push(self.data[j].clone());
                }
                i = i / 2;
                j += 1;
            }
            self.counter += 1;
            Option::Some(rv)
        }
    }
}

fn main(){
    let elems = vec![1,2,3];
    for x in Subsets::new(elems){
        println!("{:?}", x);
    }
}
