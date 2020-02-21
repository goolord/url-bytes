# url-bytes
memory efficient URL type / parsing for byteverse

benchmarks:
```
benchmarked url-bytes 1000
time                 259.9 μs   (253.1 μs .. 266.7 μs)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 253.0 μs   (251.8 μs .. 254.9 μs)
std dev              4.962 μs   (3.649 μs .. 7.271 μs)

benchmarked uri-bytestring strict 1000
time                 1.743 ms   (1.729 ms .. 1.764 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.758 ms   (1.747 ms .. 1.772 ms)
std dev              43.02 μs   (34.02 μs .. 59.49 μs)

benchmarked uri-bytestring lax 1000
time                 1.837 ms   (1.799 ms .. 1.878 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 1.810 ms   (1.797 ms .. 1.828 ms)
std dev              47.18 μs   (36.41 μs .. 59.74 μs)
variance introduced by outliers: 11% (moderately inflated)
```
