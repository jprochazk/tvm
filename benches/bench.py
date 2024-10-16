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
import sys


def run(cmd: str, env: dict[str, str] | None = None) -> str:
    print(f"$ {cmd}", file=sys.stderr)
    return check_output(
        cmd.split(), text=True, stderr=subprocess.STDOUT, cwd=script_dir, env=env
    )


def parse_divan(name: str, output: str) -> list[str]:
    rows: list[str] = []

    start = output.find(f"├─ {name}")
    if start == -1:
        start = output.find(f"╰─ {name}")

    for line in output[start:].splitlines()[1:]:
        line = line[3:]
        parts = line.split("│")
        if len(parts) != 6 or len(parts[1].strip()) == 0:
            break
        time = parts[3].strip()
        rows.append(time)
    return rows


def parse_lua(output: str) -> list[str]:
    rows: list[str] = []
    for line in output.splitlines():
        parts = line.split()
        time = " ".join(parts[1:])
        rows.append(time)
    return rows


def render_markdown_table(data: list[list[str]]):
    out = ""
    column_widths = [max(len(str(row[i])) for row in data) for i in range(len(data[0]))]

    def line(row: list[str]):
        nonlocal out
        out += (
            "| "
            + " | ".join(str(row[i]).ljust(column_widths[i]) for i in range(len(row)))
            + " |\n"
        )

    header = data[0]
    line(header)
    out += "| " + " | ".join("-" * width for width in column_widths) + " |\n"
    for row in data[1:]:
        line(row)
    return out


def get_lua_version():
    try:
        o = run("lua -v")
        return o.split()[1]
    except:
        return None


def get_luajit_version():
    try:
        o = run("luajit -v")
        return o.split()[1]
    except:
        return None


script_dir = Path(os.path.dirname(__file__))


def main():
    results = {}

    cargo = run("cargo bench main")

    results["Rust (native)"] = {
        "src": (script_dir / "fib.rs").read_text().strip(),
        "timings": parse_divan("native_fib", cargo),
        "lang": "rust",
    }

    results["hebi3 (tvm)"] = {
        "src": (script_dir / "fib.hebi").read_text().strip(),
        "timings": parse_divan("fib", cargo),
        "lang": "rust",
    }

    lua_v = get_lua_version()
    if lua_v is not None:
        results[f"lua {lua_v}"] = {
            "src": (script_dir / "fib.lua").read_text().strip(),
            "timings": parse_lua(run("lua main.lua")),
            "lang": "lua",
        }

    luajit_v = get_luajit_version()
    if luajit_v is not None:
        results[f"luajit {luajit_v}"] = {
            "src": (script_dir / "fib.lua").read_text().strip(),
            "timings": parse_lua(run("luajit main.lua")),
            "lang": "lua",
        }

    print("## Benchmark: recursive fibonacci\n")
    rows = [["entry", "N=5", "N=10", "N=15", "N=20", "N=25"]]
    code: list[str] = []
    for name, info in results.items():
        src = info["src"]
        timings = info["timings"]
        lang = info["lang"]

        rows.append([name, *timings])
        # fmt:off
        code.append("\n".join([
            f"### {name}",
            f"```{lang}\n{src}\n```",
            ""
        ]))
    print(render_markdown_table(rows))
    print("\n".join(code))


if __name__ == "__main__":
    try:
        import psutil

        psutil.Process().cpu_affinity([0])
    except ImportError:
        print("could not set core affinity", file=sys.stderr)
        pass

    main()
