
fn factorial(input: i32) -> i32 {
    match input {
        Panic(p) => Panic(p),
        Nom(val) => if val < 0 {
            Panic(panic::IntegerOverflow)
        } else if val < 2 {
            Nom(1)
        } else {
            val * factorial(val - 1)
        }
    }
}

fn main() {
    println!("8! = {}", factorial(8.into()));
    println!("9! = {}", factorial(9.into()));
    println!("10! = {}", factorial(10.into()));
}