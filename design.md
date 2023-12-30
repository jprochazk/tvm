
## Variables

```rust
let v: int = 0;
let v = 0;

v += 1; // <- error

mut v: int = 0;
mut v = 0;

v += 1; // <- ok
```

## Literals

```rust
let v: int = 1; // integer, i32
let v: num = 1.0; // "generic number", f64
let v: bool = true; // true/false
let v: str = "yo"; // strings, immutable

// "todo" literal
// - coerces to any type
// - warning at compile time
// - error at runtime
let v: T = todo;
```

## Operators

```rust
2 + 2;
2 - 2;
2 / 2;
2 * 2;
2 % 2;
2 ** 2;
2 == 2;
2 != 2;
2 > 2;
2 >= 2;
2 < 2;
2 <= 2;
-2;
!true;
true && true;
false || true;
```

```rust
name = 1;
name += 1;
name -= 1;
name /= 1;
name *= 1;
name %= 1;
name **= 1;
```

## Control flow

```rust
// block stmt
{
    let v = 0;
}

// block expr
let v = do { 1 };

// if stmt
if true {}
else if true {}
else {}

// if expr
let v = if true { 0 } else { 1 };

// infinite loop
loop {}
```

```rust
return value;
return;
break;
continue;

// all of the above are expr with type `!`
let v = return none;
let v = break;
let v = continue;
```

## Functions

```rust
fn simple() {}
fn single_param(a: A) {}
fn multi_param(a: A, b: B, c: C) {}
fn with_ret() -> R {}
fn param_and_ret(a: A) -> R {}
fn full_mono(a: A, b: B, c: C) -> R {}
```

## Types

```rust
// Records
type Foo(a: int, b: str); // named fields
type Bar(str); // unnamed fields

// record constructor, field access
let v = Foo(a: 100, b: "test");
print(v.b);

// Enums
type Opt<T> {
    None,
    Some(T),
}

type Baz {
    Lorem,
    Ipsum(a: int, b: str),
    Dolor,
}

// union constructor, match
let v = Lorem;
let v = Ipsum(a: 100, b: "test");

match v {
    Lorem { print("A") }
    Ipsum(a, b) { print("B") }
    _ { print("?") }
}
```

## Code examples


### Tic Tac Toe

```rust
type Player { X, O }
type Cell {
    Empty,
    With(Player),
}

impl ToStr for Cell {
    fn to_str(self, mut buf: fmt.Buffer) {
        match self {
            Empty -> buf.write("-"),
            With(X) -> buf.write("X"),
            With(O) -> buf.write("O"),
        }
    }
}

type TicTacToe(board: [Cell], player: Player)

impl TicTacToe {
    fn new() -> Self {
        TicTacToe(
            board: [Empty; 9],
            player: X,
        )
    }
}

impl ToStr for TicTacToe {
    fn to_str(self, mut buf: fmt.Buffer) {
        // |x|o|x|      |1|2|3|
        // |x|o|o|      |4|5|6|
        // |o|x| |      |7|8|9|
        for i in 0..3 {
            buf.write("|");
            buf.join(self.board.slice(i*3, 3), "|");
            buf.write("|");
            buf.write("      ");
            buf.write("|");
            buf.join([1 + i*3, 1 + i*3+1, 1 + i*3+2], "|");
            buf.write("|");
            buf.writeln();
        }
    }
}

impl TicTacToe {
    fn player_turn(mut self) -> int {
        print("> Playing as {self.current_player}");

        mut pos = int.parse(input("> (1-9): "))
        loop {
            if pos < 1 || pos > 9 {
                print("invalid position");
                continue
            }

            if self.board[pos - 1] != Empty {
                print("{pos} is already taken");
                continue
            }
        }
        
        self.board[pos - 1] = self.player;
    }

    fn swap_player(mut self) {
        self.player = match self.player {
            X -> O,
            O -> X,
        }
    }

    fn check_for_win(self) -> Opt<str> {
        let scan_lines = [
        // rows:
        //   |X|X|X|    |_|_|_|    |_|_|_|
        //   |_|_|_|    |X|X|X|    |_|_|_|
        //   |_|_|_|    |_|_|_|    |X|X|X|
            [0, 1, 2], [3, 4, 5], [6, 7, 8],
        // cols:
        //   |X|_|_|    |_|X|_|    |_|_|X|
        //   |X|_|_|    |_|X|_|    |_|_|X|
        //   |X|_|_|    |_|X|_|    |_|_|X|
            [0, 3, 6], [1, 4, 7], [2, 5, 8],
        // diag:
        //   |X|_|_|    |_|_|X|
        //   |_|X|_|    |_|X|_|
        //   |_|_|X|    |X|_|_|
            [0, 4, 8], [2, 4, 6],
        ];

        for line in scan_lines {
            let p0 = game.board[line[0]];
            let p1 = game.board[line[1]];
            let p2 = game.board[line[2]];
            match (p0, p1, p2) {
                (With(X), With(X), With(X)) {
                    return Some("X wins!")
                }
                (With(O), With(O), With(O)) {
                    return Some("O wins!")
                }
                _ { continue }
            }
        }

        if game.board.contains(Empty) {
            return None
        }
    }
}

fn main() {
    let game = TicTacToe();
    print(game.to_str());

    loop {
        game.player_turn();
        match game.check_for_win() {
            Some(v) {
                print(v);
                break
            }
            None {
                game.swap_player();
                continue
            }
        }
    }
}

main();
```

# MVP

```rust
// extern function, must be linked later
// this can either be declared in Rust code or via "extern" interface
extern fn print(v: any);

type Thing(a: int); // named fields only

let v = Thing(a: 50); // constructor, variables, type inference

fn foo(v: Thing) { // mono functions
    // named field access
    print(v.a)
}

foo(v); // function calls, variable use
```
