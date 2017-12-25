// Codeforces 620D

#include <bits/stdc++.h>

constexpr int N = 2000 + 5;

long long ns[N] = {0}, ms[N] = {0};

struct Tick {
    long long val;
    int idx;
    Tick() {}
    Tick(long long val, int idx): val(val), idx(idx) {}
    bool operator<(const Tick & other) {
        return val < other.val;
    }
};

struct Tick nss[N * (N - 1) / 2], mss[N * (N - 1) / 2];

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int n, m;
    long long sa = 0, sb = 0, v0 = INT_MAX;
    long long v1 = INT_MAX, v2 = INT_MAX;
    int idxa, idxb;

    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> ns[i];
        sa += ns[i];
    }
    std::cin >> m;
    for (int i = 0; i < m; ++i) {
        std::cin >> ms[i];
        sb += ms[i];
    }

    // zero swap
    v0 = std::abs(sa - sb);
    v1 = v0;

    // one swap
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            long long x = std::abs(sa - sb + 2 * ms[j] - 2 * ns[i]);
            if (x < v1) {
                v1 = x;
                idxa = i, idxb = j;
            }
        }
    }
    v2 = v1;

    // two swap
    int p = 0, q = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            nss[p].val = ns[i] + ns[j];
            nss[p++].idx = i * n + j;
        }
    }
    std::sort(nss, nss + p);
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < i; ++j) {
            mss[q].val = ms[i] + ms[j];
            mss[q++].idx = i * m + j;
        }
    }
    std::sort(mss, mss + q);

    long long diff = sa - sb;
    int j = 0;
    for (int i = 0; i < p; ++i) {
        long long x = std::abs(diff + 2 * mss[j].val - 2 * nss[i].val);
        while (j < q && x >= std::abs(diff + 2 * mss[j].val - 2 * nss[i].val)) {
            x = std::abs(diff + 2 * mss[j].val - 2 * nss[i].val);
            j += 1;
        }
        j -= 1;
        if (x < v2) {
            v2 = x;
            idxa = nss[i].idx, idxb = mss[j].idx;
        }
    }

    if (v2 < v1) {
        std::cout << v2 << std::endl << 2 << std::endl << (idxa + 1) / n << " " << (idxa + 1) % n
                                          << std::endl << (idxb + 1) / m << " " << (idxb + 1) % m;
    } else if (v1 < v0) {
        std::cout << v1 << std::endl << 1 << std::endl << idxa + 1 << " " << idxb + 1;
    } else {
        std::cout << v0 << std::endl << 0;
    }
}
