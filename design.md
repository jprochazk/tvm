
## Variables

```rust
let v: int = 0;
let v = 0; // type inference

// deeply immutable
v += 1; // <- error

mut v: int = 0;
mut v = 0;

v += 1; // <- ok
```

## Literals

```rust
let v: int = 1; // integers, the default numeric type
let v: num = 1.0; // "generic number", f64
let v: bool = true; // true/false
let v: str = "yo"; // strings, immutable

// built-in data structures:

// List
let v: List<int> = List<int>.new(1, 2, 3);
let v: List<int> = List.new(1, 2, 3);
let v: List<int> = [1, 2, 3];

// Map
mut v: Map<str, int> = Map<str, int>.new(("a", 0), ("b", 1));
mut v: Map<str, int> = Map.new(("a", 0), ("b", 1));
mut v: Map<str, int> = {"a": 0, "b": 1};

// Data structures are mutable (if they appear in a mutable place)
v.set("c", 2);

// Set
let v: Set<str> = Set.new("a", "b", "c"); // no syntax sugar
let v: Set<str> = {"a", "b"};

// Optionals, "nullable value"
let v: Opt<int> = Some(1);
let v: Opt<int> = None;

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

```rust
let a: Opt<T> = todo;
let b: T = todo;

let value = a ?? b;
// equivalent to:
let value = match a {
  Some(a) -> a,
  None -> b,
};
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

// if expr,
// evaluates to:
// - `T` if given an `else` branch
// - `Opt<T>` if not given an `else`
let v: Opt<int> = if true { 0 };
// above is equivalent to:
let v: Opt<int> = if true { Some(0) } else { None };

// If given an `else`, then evaluates to just `T`.
let v: int = if true { 0 } else { 1 };

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
// function declarations:
fn simple() {}
fn single_param(a: A) {}
fn multi_param(a: A, b: B, c: C) {}
fn with_ret() -> R {}
fn param_and_ret(a: A) -> R {}
fn full_mono(a: A, b: B, c: C) -> R {}

fn generic<T>() {}
fn multi_generic<A, B, C>() {}

fn var_args<T>(v: ...T) {}
```

## Function calls

```rust
fn adder(a: int) -> (int) -> int {
  fn inner(b: int) { a + b }
  inner
}

print(adder(10)(50)); // 60
```

## Composite types

```rust
// Records
type Foo = { a: int, b: str };
type Bar = { a: str, b: int };

// record constructor, field access
let v: Foo = { a: 100, b: "test" };
print(v.b);

// Unions
type Bar =
  | Lorem
  | Ipsum(a: int, b: str)
  | Dolor;

// union constructor, match
let v = Lorem
let v = Ipsum(a: 100, b: "test")

match v {
  Lorem -> print("A")
  Ipsum(_, _) -> print("B")
  _ -> print("?")
}

```

## Code examples


```rust

mod foo {
  type Thing = { a: int };

  fn test(v: Thing) -> int {
    v.a
  };
}

mod bar {
  use foo.test;

  // test : (Thing) -> int
  test({ a: 10 })
}

```

### Tic Tac Toe

```rust
type Player = X | O;
type Cell = Empty | With(player: Player);

type TicTacToe(
  board: [Cell],
  player: Player,
);

fn TicTacToe.new() -> Self {
  Self(
    board: [Empty; 9],
    player: X
  )
}

fn Cell.to_str(self) -> str {
  match self {
    Empty -> "-"
    With(player) -> player.to_str()
  }
}

fn Player.to_str(self) -> str {
  match self {
    X -> "X"
    O -> "O"
  }
}

fn to_str(game: TicTacToe) -> str {
  let out = [];
  for i in 0..3 {
    let line = "";
    let map = "";
    for j in 0..3 {
      let row = i * 3;
      line += self.board[row + j].to_str();
      map += str(row + j + 1);
      if j < 2 {
        line += " | ";
        map += "|";
      }
    }
    out.push("{line}      {map}");
  }
  out.join("\n");
}

fn move(game: TicTacToe) -> int {
  print("> Playing as {self.current_player}");

  loop {
    let pos: int =
      input("> Enter the position number (1-9): ")
        .parse_int();
    if pos < 1 || pos > 9 {
      print("Invalid position, please try again.");
      continue
    }

    if self.board[pos - 1] != Cell.Empty {
      print("Position {pos} already taken, please try again.");
      continue
    }

    return pos
  }
}

fn swap(game: TicTacToe) {
  game.player = match game.player {
    Player.X -> Player.O
    Player.O -> Player.X
  }
}

fn win(game: TicTacToe) -> str? {
  let scan = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8], // rows
    [0, 3, 6], [1, 4, 7], [2, 5, 8], // cols
    [0, 4, 8], [2, 4, 6],            // diag
  ];

  for line in scan {
    let p0 = game.board[line[0]];
    let p1 = game.board[line[1]];
    let p2 = game.board[line[2]];
    match p0 {
      Cell.Empty -> {
        continue // not a full line
      }
      Cell.With { player } -> {
        if p0 == p1 && p1 == p2 {
          return "{player.to_str()} wins!"
        }
      }
    }
  }

  if Cell.Empty in game.board {
    return none // game is not over yet
  }

  return "It's a draw!"
}

fn main() {
  let game = TicTacToe();
  print(game.to_str());

  loop {
    let pos = game.move();
    game.board[pos - 1] = game.player;

    let result = game.win();
    if !result {
      game.swap();
      continue
    } else {
      print(result);
      break
    }
  }
}

main();
```

## Parsing / type checking

Semantic changes:
- Top level scope may only contain:
  - Unique declarations (may not be shadowed)
  - Expression statements


```rust
fn f() {
  print(v)
}

// # type checking
// 1: collect and check all declaration signatures
// - records:
//   - 1: assign each record a UserTypeId if it doesn't have on already
//     2: check fields (does the name exist in the current scope -> get its TypeId and store that)
//                     assign offset as (field_index * 8)
//     3: add record to the decl pool
// - function signatures
//   - 1: check parameters and return types (do named types exist? are all generic instantiations valid?)
//     2: add function to the decl pool
// 2: check top-level code
// - traverse each stmt
//   - let -> unify Lty with Rty
//   recursively check each subexpression
//
// # codegen
// result:
//   fn `f`:
//     .data
//     .code
//       mov   m0, r1       ; print(v)
//       call  #print, r1   ;
//   
//   main:
//     .data
//       [0] = function_prototype { name: "f", module_idx: 0 }
//     .code
//       mov   [0], r1    ; fn f() {
//       mov   0, m0      ; let v = 0;
//       call  [0]        ; f()
//       mov   10, m0     ; v = 10;
//       call  [0]        ; f()
```
