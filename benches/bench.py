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


def parse_divan(output: str) -> list[list[str]]:
    rows: list[list[str]] = []
    for line in output[output.find("╰─ fib") :].splitlines()[1:]:
        parts = line.split("│")
        if len(parts) != 6:
            continue
        N = parts[0].strip().split()[1]
        time = parts[3].strip()
        rows.append([N, time])
    return rows


def parse_lua(output: str) -> list[list[str]]:
    rows: list[list[str]] = []
    for line in output.splitlines():
        parts = line.split()
        N = parts[0]
        time = " ".join(parts[1:])
        rows.append([N, time])
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
    o = run("lua -v")
    return o.split()[1]


script_dir = Path(os.path.dirname(__file__))


def main():
    results = {}

    results["hebi3 (tvm)"] = {
        "src": (script_dir / "fib.hebi").read_text().strip(),
        "timings": parse_divan(run("cargo bench fib")),
    }

    results[f"lua {get_lua_version()}"] = {
        "src": (script_dir / "fib.lua").read_text().strip(),
        "timings": parse_lua(run("lua main.lua")),
    }

    print("## Benchmark: recursive fibonacci\n")
    for name, info in results.items():
        src = info["src"]
        timings = info["timings"]

        # fmt:off
        output = "\n".join([
            f"### {name}",
            f"```\n{src}\n```",
            "",
            render_markdown_table([
                ["N", "time"],
                *timings,
            ]),
        ])

        print(output)


if __name__ == "__main__":
    try:
        import psutil

        psutil.Process().cpu_affinity([0])
    except ImportError:
        print("could not set core affinity")
        pass

    main()
