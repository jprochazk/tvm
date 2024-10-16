// @ts-check

import { hrtime } from "node:process";

/**
 * @param {number} n
 * @param {(value: number) => void} f
 * @param {number} value
 */
function benchmark(n, f, value) {
  let elapsed_ns = 0;
  for (let i = 0; i < n; i++) {
    let now = hrtime.bigint();
    f(value);
    let time = hrtime.bigint() - now;
    elapsed_ns += Number(time);
  }

  let avg_ns = elapsed_ns / n;
  let output = `${value} ${avg_ns}`;
  console.log(output);
}

import { fib } from "./fib.js";

benchmark(1000, fib, 5);
benchmark(1000, fib, 10);
benchmark(1000, fib, 15);
benchmark(1000, fib, 20);
benchmark(1000, fib, 25);

