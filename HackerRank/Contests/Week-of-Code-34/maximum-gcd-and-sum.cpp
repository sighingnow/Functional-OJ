#include <cstdio>
#include <cstdlib>
#include <iostream>

int as[1000005] = {0}, bs[1000005] = {0};
int maxa = 1, maxb = 1;

int main() {
    int n, p, u, v;
    scanf("%d", &n);
    for (int i = 1; i <= n; ++i) {
        scanf("%d", &u);
        as[u] = u;
        maxa = std::max(maxa, u);
    }
    for (int i = 1; i <= n; ++i) {
        scanf("%d", &v);
        bs[v] = v;
        maxb = std::max(maxb, v);
    }
    p = std::min(maxa, maxb);
    as[0] = 1;
    bs[0] = 1;

    // process as
    if (maxa > p) {
        for (int k = maxa / 2; k >= p; --k) {
            for (int i = k + k; i < maxa; i += k) {
                if (as[i] > 0) {
                    as[k] = as[i];
                    break;
                }
            }
        }
    }
    // process ba
    if (maxb > p) {
        for (int k = maxb / 2; k >= p; --k) {
            for (int i = k + k; i < maxb; i += k) {
                if (bs[i] > 0) {
                    bs[k] = bs[i];
                    break;
                }
            }
        }
    }

    while ((as[p] == 0 || bs[p] == 0) && p > 1) {
        // process as
        for (int i = p + p; i <= maxa; i += p) {
            if (as[i] > 0) {
                as[p] = as[i];
                break;
            }
        }
        // process bs
        for (int i = p + p; i <= maxb; i += p) {
            if (bs[i] > 0) {
                bs[p] = bs[i];
                break;
            }
        }
        // if needs continue
        if (as[p] == 0 || bs[p] == 0) {
            p -= 1;
        }
    }
    if (p == 0 || p == 1) {
        printf("%d", maxa + maxb);
    } else {
        printf("%d", as[p] + bs[p]);
    }
    return 0;
}

