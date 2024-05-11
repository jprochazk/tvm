#!/usr/bin/python3

"""
Benchmarking harness

To run this, you need the following tools:
- `python` v3.11 or higher
- `cargo`
- `lua` v5.4 or higher
"""

import subprocess
from subprocess import check_output
from pathlib import Path
import os


def run(cmd: str) -> str:
    return check_output(
        cmd.split(),
        text=True,
        stderr=subprocess.STDOUT,
        cwd=script_dir,
    )


def parse_divan(output: str) -> str:
    out = ""
    for line in output[output.find("╰─ fib") :].splitlines()[1:]:
        parts = line.split("│")
        if len(parts) != 6:
            continue
        arg = parts[0].strip().split()[1]
        mean = parts[3].strip()
        out += f"fib({arg})\t{mean}\n"
    return out


def check_lua_version():
    o = run("lua -v")
    assert "Lua 5.4" in o, f"please install lua 5.4, detected version: {o.split()[1]}"


script_dir = Path(os.path.dirname(__file__))

results = {}

results["hebi3 (tvm)"] = {
    "src": (script_dir / "fib.hebi").read_text().strip(),
    "timings": parse_divan(run("cargo bench fib")),
}

check_lua_version()
results["lua 5.4"] = {
    "src": (script_dir / "fib.lua").read_text().strip(),
    "timings": run("lua main.lua"),
}

print("# benchmark: recursive fibonacci\n")
for name, info in results.items():
    print(f'## {name}\n{info["src"]}\n\n{info["timings"]}')
