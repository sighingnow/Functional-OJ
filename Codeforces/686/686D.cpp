// Codeforces 686D

#include <iostream>
#include <algorithm>
#include <vector>

constexpr int MAX_N = 300005;

std::vector<int> graph[MAX_N];
int fa[MAX_N], son[MAX_N];
int centeroid[MAX_N];

void dfs(int u) {
    son[u] = 1;
    centeroid[u] = u;
    int maxn = 0;
    for (const int &v: graph[u]) {
        dfs(v);
        son[u] += son[v];
        if (son[v] > son[maxn]) {
            maxn = v;
        }
    }
    if (son[maxn] * 2 > son[u]) {
        int now = centeroid[maxn];
        while ((son[u] - son[now]) * 2 > son[u]) {
            now = fa[now];
        }
        centeroid[u] = now;
    }
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n, q, x;
    std::cin >> n >> q;
    for (int i = 2; i <= n; ++i) {
        std::cin >> fa[i];
        graph[fa[i]].push_back(i);
    }
    dfs(1);
    while (q--) {
        std::cin >> x;
        std::cout << centeroid[x] << std::endl;
    }

    return 0;
}
