#include <cstdio>

bool lucky(int n) {
    int a[6];
    scanf("%d", &n);
    a[5] = n % 10;
    n /= 10;
    a[4] = n % 10;
    n /= 10;
    a[3] = n % 10;
    n /= 10;
    a[2] = n % 10;
    n /= 10;
    a[1] = n % 10;
    n /= 10;
    a[0] = n % 10;
    n /= 10;
    return a[0] + a[1] + a[2] == a[3] + a[4] + a[5];
}

int main() {
    int n;
    scanf("%d", &n);
    while (!lucky(++n)) {
    }
    printf("%d", n);
    return 0;
}

