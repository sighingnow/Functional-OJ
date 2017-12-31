// Codeforces 878A

#include <bits/stdc++.h>
#include <cstdio>

int main() {
    int a = 0, b = 1023, n, x;
    char c;
    scanf("%d", &n);
    for (int i = 0; i < n; ++i) {
        scanf("\n%c %d", &c, &x);
        switch (c) {
            case '|': a |= x; b |= x; break;
            case '&': a &= x; b &= x; break;
            case '^': a ^= x; b ^= x; break;
        }
    }
    int a1 = 0, a2 = 0;
    for (int i = 0, k = 1; i < 10; i += 1, k *= 2) {
        if (!(a & k) && !(b & k)) {
            // 0 -> 0, 1 -> 0: &0, ^0
        } else if (!(a & k) && (b & k)) {
            // 0 -> 0, 1 -> 1: &1, ^0
            a1 += k;
        } else if ((a & k) && !(b & k)) {
            // 0 -> 1, 1 -> 0: &1, ^1
            a1 += k;
            a2 += k;
        } else if ((a & k) && (b & k)) {
            // 0 -> 1, 1 -> 1: &0, ^1
            a2 += k;
        }
    }
    printf("2\n& %d\n^ %d\n", a1, a2);

    return 0;
}
