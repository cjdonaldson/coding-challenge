use tailcall::tailcall;

fn factorial(input: u64) -> u64 {
    #[tailcall]
    #[inline(always)]
    fn factorial_inner(accumulator: u64, input: u64) -> u64 {
        if input > 0 {
            factorial_inner(accumulator * input, input - 1)
        } else {
            accumulator
        }
    }

    factorial_inner(1, input)
}

fn factorial_tramp(input: u64) -> u64 {
    #[inline(always)]
    fn factorial_inner((accum, input): (u64, u64)) -> tailcall::trampoline::Next<(u64, u64), u64> {
        if input > 0 {
            become tailcall::trampoline::Recurse((accum * input, input - 1))
        } else {
            tailcall::trampoline::Finish(accum)
        }
    }

    tailcall::trampoline::run(factorial_inner, (1, input))
}

fn main() {
    println!("Hello, world!");
    let values: Vec<u64> = vec![1, 2, 3, 4, 5, 6, 7, 8, 21];
    println!("factorial 1: {}", factorial(1));
    values
        .iter()
        .for_each(|&x| println!("factorial_tramp {}: {}", x, factorial_tramp(x)));
}
