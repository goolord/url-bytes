# url-bytes
memory efficient URL type / parsing for byteverse

### Benchmarks:
```
benchmarked url-bytes 1,000
time                 165.8 μs   (162.8 μs .. 168.7 μs)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 174.6 μs   (171.5 μs .. 181.0 μs)
std dev              13.70 μs   (8.815 μs .. 21.91 μs)
variance introduced by outliers: 50% (moderately inflated)

benchmarked uri-bytestring strict 1,000
time                 1.814 ms   (1.739 ms .. 1.879 ms)
                     0.992 R²   (0.987 R² .. 0.996 R²)
mean                 1.723 ms   (1.698 ms .. 1.766 ms)
std dev              108.1 μs   (56.35 μs .. 181.4 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarked uri-bytestring lax 1,000
time                 1.658 ms   (1.632 ms .. 1.680 ms)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 1.772 ms   (1.737 ms .. 1.846 ms)
std dev              177.2 μs   (87.96 μs .. 290.8 μs)
variance introduced by outliers: 63% (severely inflated)

benchmarked dormouse-uri 1,000
time                 2.816 ms   (2.795 ms .. 2.849 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.802 ms   (2.797 ms .. 2.813 ms)
std dev              24.09 μs   (10.54 μs .. 49.36 μs)

Memory usage:
Case                               Allocated  GCs
url-bytes 1,000 [Maybe Url]          200,000    0
uri-bytestring 1,000 [Maybe URI]   6,767,560    6
dormouse-uri 1,000 [Maybe Uri]    17,656,184   17
```
