## Benchmark: recursive fibonacci

| entry                  | N=5       | N=10      | N=15     | N=20      | N=25      |
| ---------------------- | --------- | --------- | -------- | --------- | --------- |
| Rust (native)          | 12.26 ns  | 144.5 ns  | 1.678 µs | 18.33 µs  | 211.7 µs  |
| hebi3 (tvm)            | 437.1 ns  | 4.666 µs  | 45.26 µs | 501.2 µs  | 5.612 ms  |
| lua 5.4.6              | 553.00 ns | 3.67 us   | 34.09 us | 369.81 us | 4.11 ms   |
| luajit 2.1.1713773202  | 1.20 us   | 767.00 ns | 5.64 us  | 61.16 us  | 687.04 us |
| node v20.12.1 (no JIT) | 0.52 us   | 4.80 us   | 50.68 us | 562.28 us | 6.23 ms   |
| node v20.12.1 (JIT)    | 0.28 us   | 0.45 us   | 4.35 us  | 48.73 us  | 539.14 us |

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

### node v20.12.1 (no JIT)
```js
export function fib(n) {
  if (n < 2) {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}
```

### node v20.12.1 (JIT)
```js
export function fib(n) {
  if (n < 2) {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}
```

