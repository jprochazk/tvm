## Benchmark: recursive fibonacci

### hebi3 (tvm)
```
fn fib(n: int) -> int {
    if n < 2 { n }
    else { fib(n - 1) + fib(n - 2) }
}
```

| N  | time     |
| -- | -------- |
| 5  | 34.97 ns |
| 10 | 68.03 ns |
| 15 | 407.8 ns |
| 20 | 4.37 µs  |
| 25 | 45.21 µs |

### lua 5.4.4
```
function fib(n)
    if n < 2 then return n
    else return fib(n-1) + fib(n-2)
    end
end
```

| N  | time      |
| -- | --------- |
| 5  | 381.00 ns |
| 10 | 2.68 us   |
| 15 | 28.71 us  |
| 20 | 322.58 us |
| 25 | 3.63 ms   |

