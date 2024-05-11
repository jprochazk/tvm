# benchmarks

Prerequisites:
- `cargo`
- `lua`, v5.4+
- `python`, v3.11+
- optional: `psutil`, installed via `pip install psutil`
  - used to set core affinity before running benchmarks,
    which can significantly reduces run-to-run variance in the results.

```
python3 bench.py > data.md
```

Current results: [results.md](./results.md)
