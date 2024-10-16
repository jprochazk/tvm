do
    function clock_ns()
        return os.clock() * 1000000000
    end

    function benchmark(n, f, value)
        local decPlaces = 2
        local elapsed_ns = 0
        for i = 1, n do
            local now = clock_ns()
            f(value)
            local time = (clock_ns() - now)
            elapsed_ns = elapsed_ns + time
        end
        
        local avg_ns = elapsed_ns / n
        local output = string.format('%d %f', value, avg_ns)
        print(output)
    end

    dofile('fib.lua')

    benchmark(1000, fib, 5)
    benchmark(1000, fib, 10)
    benchmark(1000, fib, 15)
    benchmark(1000, fib, 20)
    benchmark(1000, fib, 25)
end
