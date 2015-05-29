struct philosopher {
    name : String;//box for &str
}
impl philosopher {
    fn new(name: &str) -> philosopher {
        philosopher {
            name: name.to_string();//box up &str
        }
    }
    fn eat(&self){
        println!("{} is done eating.", self.name);
    }
}


fn main() {
    let p1 = philosopher::new("Baruch Spinoza");
    let p2 = philosopher::new("Gilles Deleuze");
    let p3 = philosopher::new("Karl Marx");
    let p4 = philosopher::new("Friedrich Nietzsche");
    let p5 = philosopher::new("Michel Foucault");
    for p in (p1,p2,p3,p4,p5)
}
