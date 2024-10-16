## Benchmark: recursive fibonacci

| entry                 | N=5       | N=10      | N=15     | N=20      | N=25      |
| --------------------- | --------- | --------- | -------- | --------- | --------- |
| Rust (native)         | 12.44 ns  | 146.7 ns  | 1.713 µs | 18.85 µs  | 205.6 µs  |
| hebi3 (tvm)           | 387 ns    | 4.358 µs  | 47.31 µs | 497.4 µs  | 5.716 ms  |
| lua 5.4.6             | 509.00 ns | 3.50 us   | 33.84 us | 370.85 us | 4.10 ms   |
| luajit 2.1.1713773202 | 1.32 us   | 686.00 ns | 4.99 us  | 52.66 us  | 580.21 us |

### Rust (native)
```rust
#[inline(never)]
pub fn fib(n: i32) -> i32 {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
```

### hebi3 (tvm)
```rust
fn fib(n: int) -> int {
    if n < 2 { n }
    else { fib(n - 1) + fib(n - 2) }
}
```

### lua 5.4.6
```lua
function fib(n)
    if n < 2 then return n
    else return fib(n-1) + fib(n-2)
    end
end
```

### luajit 2.1.1713773202
```lua
function fib(n)
    if n < 2 then return n
    else return fib(n-1) + fib(n-2)
    end
end
```

