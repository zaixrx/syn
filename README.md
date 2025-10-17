# Syn Programming Language
sen (c in arabic) is a statically typed C-inspired hobby
programming language that runs on a stack-based synVM with it's own runtime
providing (hopefully) garabage collection and JIT compilation

```
// syn is statically typed
func fib(n: Int) -> Int {
    if n <= 1 {
        return 1;
    }
    let a = 1;
    let b = 1;
    while n > 0 {
        a = a + b;
        b = a - b;
        n = n - 1;
    }
    return a; 
} 

// functions are first class
func get_fib() -> Func { 
    return fib; 
}

func main() {
    let x = 30;
    println("get_fib()({}) = {}", x, get_fib()(x));
}
```

# Resources

Writing Interpreters In Rust: https://rust-hosted-langs.github.io/book/chapter-interp-vm-design.html

Crafting Interpreters: https://craftinginterpreters.com/

Pratt Parsing(Operator-precedence parser): https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

Performance in Rust: https://nnethercote.github.io/perf-book/
