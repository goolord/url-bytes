# url-bytes
memory efficient URL type / parsing for byteverse

benchmarks:
```
benchmarked url-bytes 1,000
time                 191.8 μs   (190.8 μs .. 192.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 192.5 μs   (191.9 μs .. 193.5 μs)
std dev              2.394 μs   (1.692 μs .. 3.548 μs)

benchmarked uri-bytestring strict 1,000
time                 1.755 ms   (1.746 ms .. 1.767 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.743 ms   (1.738 ms .. 1.748 ms)
std dev              18.29 μs   (14.19 μs .. 23.28 μs)

benchmarked uri-bytestring lax 1,000
time                 1.776 ms   (1.768 ms .. 1.784 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.772 ms   (1.768 ms .. 1.778 ms)
std dev              15.20 μs   (10.95 μs .. 22.16 μs)
```
