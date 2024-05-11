# benchmarks

Requirements:
- `cargo`
- `python`, v3.11+
- optional: `lua`, v5.4+
- optional: `luajit`, v2.1+, preferrably [built from source](https://github.com/LuaJIT/LuaJIT)
- optional: `psutil`, installed via `pip install psutil`
  - used to set core affinity before running benchmarks,
    which can significantly reduce run-to-run variance

```
python3 bench.py > data.md
```

Current results: [results.md](./results.md)

If you don't want to use python, you can also run only the Rust benchmarks with just `cargo bench`.
