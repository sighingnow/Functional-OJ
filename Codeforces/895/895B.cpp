#include <bits/stdc++.h>
#include <iostream>

const int MAX_N = 1e5+10;

int numbers[MAX_N] = {0};
int times[MAX_N] = {0};

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int m = 0, n, x, k;
    std::cin >> n >> x >> k;
    for (int i = 0; i < n; ++i) {
        std::cin >> numbers[i];
    }
    std::sort(numbers, numbers + n);

    for (int i = 0; i < n; ++i) {
        times[i] = numbers[i] / x;
    }
    times[n] = 0x7fffffff;

    int i = 0, j = 0;
    long long ans = 0;
    for (int i = 0; i < n; ++i) {
        if (i > 0 && numbers[i] != numbers[i-1]) {
            j = i;
        }
        int base = times[i] - (numbers[i] % x == 0 ? 1 : 0);
        int target = base + k;
        int *p = std::lower_bound(times + j, times + n + 1, target);
        int *q = std::upper_bound(times + j, times + n + 1, target);
        int poffset = p - times, qoffset = q - times;
        if (times[qoffset - 1] == target) {
            ans += (long long)(qoffset - poffset);
        }
    }

    std::cout << ans;

    return 0;
}
