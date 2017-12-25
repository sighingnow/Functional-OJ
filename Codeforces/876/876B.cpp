// Codeforces 876B

#include <bits/stdc++.h>

constexpr int MAXN = 200000+5, MAXM = 200000+5;
int n, k, m, cnt = 0, xs[MAXN], rem[MAXN], count[MAXM] = {0};

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::cin >> n >> k >> m;
    for (int i = 0; i < n; ++i) {
        std::cin >> xs[i];
        rem[i] = xs[i] % m;
        count[rem[i]] += 1;
    }
    for (int i = 0; i < m; ++i) {
        if (count[i] >= k) {
            std::cout << "Yes" << std::endl;
            for (int j = 0; j < n && cnt < k; ++j) {
                if (rem[j] == i) {
                    cnt += 1;
                    std::cout << xs[j] << " ";
                }
            }
            return 0;
        }
    }

    std::cout << "No";
    return 0;
}
