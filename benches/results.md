## Benchmark: recursive fibonacci

| entry                 | N=5       | N=10      | N=15     | N=20      | N=25      |
| --------------------- | --------- | --------- | -------- | --------- | --------- |
| Rust (native)         | 10.53 ns  | 110.4 ns  | 1.227 µs | 13.65 µs  | 150.3 µs  |
| hebi3 (tvm)           | 172.4 ns  | 2.886 µs  | 30.77 µs | 249 µs    | 2.627 ms  |
| lua 5.4.6             | 494.00 ns | 2.57 us   | 25.88 us | 284.65 us | 3.17 ms   |
| luajit 2.1.1720049189 | 530.00 ns | 928.00 ns | 7.28 us  | 78.00 us  | 861.91 us |

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

### luajit 2.1.1720049189
```lua
function fib(n)
    if n < 2 then return n
    else return fib(n-1) + fib(n-2)
    end
end
```

