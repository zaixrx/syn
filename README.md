# Syn Programming Language
syn (c in arabic) is a statically typed C-inspired hobby
programming language that runs on a stack-based synVM with it's own runtime
providing (hopefully) garabage collection and JIT compilation

```
struct f_Vector2 {
	x: Float, 
	y: Float,
}

impl f_Vector2 {
	func new() -> f_Vector2 {
		return f_Vector2 { x: 0.0, y: 0.0 };
	}
}

struct Player {
	position: f_Vector2,
	speed: Float,
}

impl Player {
	func new(speed: Float) -> Player {
		return Player {
			speed: speed,
			position: f_Vector2::new(),
		};
	}
	
	func move(self) {
		self.position.x = self.position.x + self.speed;
		self.position.y = self.position.y + self.speed;
	}
}

func main() {
	let player = Player::new(1.0);
	println("player position before: {}", player.position);
	player.move();
	println("player position after: {}", player.position);
}
```

```
// syn is statically typed
func fib(n: Int) -> Int {
    if n <= 1 {
        return n;
    }
    let a = 0;
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

1) Crafting Interpreters: "https://craftinginterpreters.com/"

2) The Dragon Book: "https://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools"

3) The Rust Programming Languge(Book): "https://doc.rust-lang.org/stable/book/"

3) Pratt Parsing(Operator-precedence parser): "https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html/"

4) Writing Interpreters In Rust: "https://rust-hosted-langs.github.io/book/chapter-interp-vm-design.html/"

5) Garbage Collection: "https://blog.pnkfx.org/blog/categories/gc/"

6) Performance in Rust: "https://nnethercote.github.io/perf-book/"
