#include <stdio.h>

int gcd(int x, int y) {
    while(x != 0) {
        int r = y % x;
        y = x;
        x = r;
    }
    return y;
}

unsigned long int nFrac(int n) {
    unsigned long int count = 0;
    for (int denom = 1; denom <= n; denom++) {
        for (int numerator = 1; numerator < denom; numerator++) {
            if (gcd(numerator, denom) == 1) {
                count++;
            }
        }
    }
    return count;
}

int main(int argc, char** argv) {
    // printf("%lu\n", nFrac(10));
    // printf("%lu\n", nFrac(100));
    // printf("%lu\n", nFrac(10000));
    printf("%ld\n", nFrac(1000000));
    return 0;
}
