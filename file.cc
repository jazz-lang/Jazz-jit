int test(int x) {
    x = 2;
    return 0;
}

#include "stdio.h"

int main() {
    int b = 0;
    test(b);
    printf("%i",b);
}