do
  dofile('fib.lua')

  local units = { 'ns', 'us', 'ms', 's' }
  local scale = {
      ['s']  = 1000000000,
      ['ms'] = 1000000,
      ['us'] = 1000,
      ['ns'] = 1
  }

  function get_unit(value)
      index = 1
      while value >= 1000 do
          value = value / 1000
          index = index + 1
      end
      return units[index]
  end

  function clock_ns()
      return os.clock() * 1000000000
  end

  function benchmark(n, name, f, ...)
      local decPlaces = 2
      local elapsed_ns = 0
      for i = 1, n do
          local now = clock_ns()
          f(...)
          elapsed_ns = elapsed_ns + (clock_ns() - now)
      end
      
      local avg_ns = elapsed_ns / n
      local unit = get_unit(avg_ns)
      local output = name .. '(' .. table.concat(table.pack(...), ', ') .. ')'
      output = output .. ' ' .. string.format('%.2f %s', avg_ns / scale[unit], unit)
      print(output)
  end

  benchmark(100, "fib", fib, 5)
  benchmark(100, "fib", fib, 10)
  benchmark(100, "fib", fib, 15)
  benchmark(100, "fib", fib, 20)
  benchmark(100, "fib", fib, 25)
end
