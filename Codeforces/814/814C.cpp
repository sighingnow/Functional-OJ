// Codeforces 814C

#include <bits/stdc++.h>

constexpr int MAXN = 1500 + 5;

char xs[MAXN];
int dp[27][MAXN] = {{0}}; // dp[c][n]将n个字母为替换为字母c所能达到的最大长度

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int n;
    std::scanf("%d", &n);
    std::scanf("%s", xs);

    for (int k = 0; k < 26; ++k) {
        for (int i = 0; i < n; ++i) {
            int replaced = 0;
            for (int j = i; j < n; ++j) {
                if (xs[j] - 'a' != k) {
                    replaced += 1;
                }
                dp[k][replaced] = std::max(dp[k][replaced], j - i + 1);
            }
        }
        for (int i = 1; i <= n; ++i) {
            dp[k][i] = std::max(dp[k][i], dp[k][i-1]);
        }
    }

    int q, x;
    char c;
    std::scanf("%d", &q);
    for (int i = 0; i < q; ++i) {
        std::scanf("%d %c", &x, &c);
        printf("%d\n", dp[c - 'a'][x]);
    }
}
