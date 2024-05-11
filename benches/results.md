## Benchmark: recursive fibonacci

| entry                 | N=5       | N=10      | N=15     | N=20      | N=25      |
| --------------------- | --------- | --------- | -------- | --------- | --------- |
| Rust (native)         | 10.47 ns  | 108.4 ns  | 1.236 µs | 13.34 µs  | 148.2 µs  |
| hebi3 (tvm)           | 37.68 ns  | 62.84 ns  | 400.7 ns | 4.455 µs  | 46.81 µs  |
| lua 5.4.4             | 377.00 ns | 2.84 us   | 29.83 us | 325.43 us | 3.65 ms   |
| luajit 2.1.1713773202 | 414.00 ns | 686.00 ns | 5.95 us  | 64.24 us  | 735.07 us |

> [!NOTE]
> I don't know why `hebi3` is better than native Rust for inputs `N>=10`...

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

### lua 5.4.4
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

