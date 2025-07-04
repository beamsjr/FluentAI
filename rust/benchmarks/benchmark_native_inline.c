#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>

// ClaudeLang generated functions

int64_t simple_arithmetic() {
    int64_t tmp_1 = 26LL;
    return tmp_1;
}

int64_t complex_arithmetic() {
    int64_t tmp_1 = 55LL;
    return tmp_1;
}

int64_t conditional() {
    int64_t tmp_1 = 100LL;
    return tmp_1;
}

// Hand-written comparison functions
int64_t simple_arithmetic_unoptimized() {
    return (2 * 3) + (4 * 5);
}

int64_t complex_arithmetic_unoptimized() {
    return 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + 10))))))));
}

int64_t conditional_unoptimized() {
    if (10 > 5) {
        return 2 * 50;
    } else {
        return 100 + 200;
    }
}

// Fibonacci function (iterative)
int64_t fibonacci(int n) {
    if (n <= 1) return n;
    int64_t a = 0, b = 1;
    for (int i = 2; i <= n; i++) {
        int64_t c = a + b;
        a = b;
        b = c;
    }
    return b;
}

#define ITERATIONS 100000000

void benchmark(const char* name, int64_t (*func)()) {
    clock_t start = clock();
    int64_t result = 0;
    
    for (int i = 0; i < ITERATIONS; i++) {
        result = func();
        // Prevent optimization from removing the loop
        asm volatile("" : : "r"(result));
    }
    
    clock_t end = clock();
    double elapsed = ((double)(end - start)) / CLOCKS_PER_SEC;
    
    printf("%-30s: %8.3f seconds (%lld)\n", name, elapsed, (long long)result);
}

void benchmark_fib(const char* name, int64_t (*func)(int), int n) {
    clock_t start = clock();
    int64_t result = 0;
    
    for (int i = 0; i < ITERATIONS/100; i++) {
        result = func(n);
        asm volatile("" : : "r"(result));
    }
    
    clock_t end = clock();
    double elapsed = ((double)(end - start)) / CLOCKS_PER_SEC;
    
    printf("%-30s: %8.3f seconds (fib(%d)=%lld)\n", name, elapsed, n, (long long)result);
}

int main() {
    printf("ClaudeLang Native Code Performance\n");
    printf("==================================\n\n");
    
    printf("Optimized (constant folded) vs Unoptimized:\n");
    printf("--------------------------------------------\n");
    
    benchmark("Simple arithmetic (opt)", simple_arithmetic);
    benchmark("Simple arithmetic (unopt)", simple_arithmetic_unoptimized);
    
    benchmark("Complex arithmetic (opt)", complex_arithmetic);
    benchmark("Complex arithmetic (unopt)", complex_arithmetic_unoptimized);
    
    benchmark("Conditional (opt)", conditional);
    benchmark("Conditional (unopt)", conditional_unoptimized);
    
    printf("\nFibonacci benchmark:\n");
    printf("--------------------------------------------\n");
    
    benchmark_fib("Fibonacci(20)", fibonacci, 20);
    benchmark_fib("Fibonacci(30)", fibonacci, 30);
    
    return 0;
}