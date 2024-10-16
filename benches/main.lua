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
        local samples = {}
        for i = 1, n do
            local now = clock_ns()
            f(value)
            local time = (clock_ns() - now)
            elapsed_ns = elapsed_ns + time
            samples[#samples+1] = time
        end
        
        table.sort(samples)
        local median_ns = samples[#samples / 2]
        local unit_median = get_unit(median_ns)
        local avg_ns = elapsed_ns / n
        local unit_avg = get_unit(avg_ns)
        local output = string.format('%d %.2f %s',
            value,
            avg_ns / scale[unit_avg],
            unit_avg
        )
        print(output)
    end

    dofile('fib.lua')

    benchmark(1000, fib, 5)
    benchmark(1000, fib, 10)
    benchmark(1000, fib, 15)
    benchmark(1000, fib, 20)
    benchmark(1000, fib, 25)
end
