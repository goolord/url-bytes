# url-bytes
memory efficient URL type / parsing for byteverse

benchmarks:
```
benchmarked url-bytes 1,000
time                 178.3 μs   (177.6 μs .. 179.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 178.7 μs   (178.2 μs .. 179.2 μs)
std dev              1.560 μs   (1.361 μs .. 1.842 μs)

benchmarked uri-bytestring strict 1,000
time                 1.747 ms   (1.730 ms .. 1.769 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.758 ms   (1.749 ms .. 1.768 ms)
std dev              31.46 μs   (25.94 μs .. 42.44 μs)

benchmarked uri-bytestring lax 1,000
time                 1.750 ms   (1.739 ms .. 1.761 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.767 ms   (1.760 ms .. 1.783 ms)
std dev              34.82 μs   (19.21 μs .. 62.27 μs)

Memory usage:

Case                              Allocated  GCs
url-bytes 1,000 [Maybe Url]         200,000    0
uri-bytestring 1,000 [Maybe URI]  6,771,360    6
```
