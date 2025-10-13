# Syn Programming Language
sen (c in arabic) is a statically typed C-inspired hobby
programming language that runs on a stack-based synVM with it's own runtime
providing (hopefully) garabage collection and JIT compilation

```
func fib(n: Float) -> Float {
    if n <= 1 {
        return 1;
    }
    let a = 1.0;
    let b = 1.0;
    while n > 0 {
        a = a + b;
        b = a - b;
        n = n - 1;
    }
    return a; 
} 

func get_fib() -> Func { 
    return fib; 
}

func main() {
    let target = 123;
    print("get_fib()(", target, ") =", get_fib()(target));
}
```

# Resources

Writing Interpreters In Rust: https://rust-hosted-langs.github.io/book/chapter-interp-vm-design.html

Crafting Interpreters: https://craftinginterpreters.com/

Pratt Parsing(Operator-precedence parser): https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

Performance in Rust: https://nnethercote.github.io/perf-book/
