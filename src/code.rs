mod op;

/*

```rust
fn fib(n: int) -> int {
    if n <= 1 { n }
    else { fib(n-2) + fib(n-1) }
}

fib(20)
```

Module {
    Function(root, 0),
    Function("fib", 1),
}

Function "fib" {
    params: [
        n: int
    ],
    stack: 3,
    bytecode: [
                ; if n <= 1 { n }
                local.get 0
                cmp.le.imm 1
                jmp.if_not @0
                local.get 0
                ret
                ; else { fib(n-2) + fib(n-1) }
        @0      local.get 0
                sub.imm 2
                call fib
                local.get 0
                sub.imm 1
                call fib
                add
                ret
    ],
}

*/
