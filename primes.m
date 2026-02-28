const sz = 1000
[0:sz]int primes, sieve
int nsieve

func countprimes:int =
    primes[0] := 2
    sieve[0]  := 4

    ++nsieve
    int nprimes := 1, trial := 3, sq := 2

    do
        while sqr(sq) <= trial do ++sq end
        --sq

        for i in 0..nsieve-1 do
            exit when primes[i] > sq
            while sieve[i] < trial do
                sieve[i] +:= primes[i]
            end
            if sieve[i] = trial then
                try_next
            end
        else
            exit all
        end

        if nsieve < sz then
            primes[nsieve] := trial
            sieve[nsieve] := sqr(trial)
            ++nsieve
        end
        ++nprimes
try_next:
        ++trial
    end

    nprimes
end

proc main =
    println "Starting run"
    int ms, res, codesz

    ms := clock()
    res := countprimes()
    ms := clock() - ms

    codesz := main - countprimes

    fprintln "# primes found in # ms", res, ms
    println "Code size is:", codesz
end
