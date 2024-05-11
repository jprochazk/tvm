# benchmarks

Requirements:
- `cargo`
- `lua`, v5.4+
- `luajit`, v2.1+, preferrably [built from source](https://github.com/LuaJIT/LuaJIT)
- `python`, v3.11+
- optional: `psutil`, installed via `pip install psutil`
  - used to set core affinity before running benchmarks,
    which can significantly reduce run-to-run variance

```
python3 bench.py > data.md
```

Current results: [results.md](./results.md)
