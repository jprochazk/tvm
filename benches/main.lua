do
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

    function benchmark(n, f, value)
        local decPlaces = 2
        local elapsed_ns = 0
        for i = 1, n do
            local now = clock_ns()
            f(value)
            elapsed_ns = elapsed_ns + (clock_ns() - now)
        end
        
        local avg_ns = elapsed_ns / n
        local unit = get_unit(avg_ns)
        local output = string.format('%d %.2f %s',
            value,
            avg_ns / scale[unit],
            unit
        )
        output = output 
        print(output)
    end

    dofile('fib.lua')

    benchmark(1000, fib, 5)
    benchmark(1000, fib, 10)
    benchmark(1000, fib, 15)
    benchmark(1000, fib, 20)
    benchmark(1000, fib, 25)
end
