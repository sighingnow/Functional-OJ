#include <bits/stdc++.h>

struct Node {
    int x, y, z;
    bool operator < (const struct Node & b) {
        // x 升序
        // y 降序：保证在删除时，留下的一定是 y 对应的 最大的 z
        return x < b.x || (x == b.x && y > b.y);
    }
};

struct Node nodes[500000+5];
std::map<int, int> bounds;

constexpr int INF = INT_MAX;

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int n, ans = 0;
    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> nodes[i].x;
    }
    for (int i = 0; i < n; ++i) {
        std::cin >> nodes[i].y;
    }
    for (int i = 0; i < n; ++i) {
        std::cin >> nodes[i].z;
    }

    std::sort(nodes, nodes + n);

    bounds[-INF] = INF;
    bounds[INF] = -INF;

    for (int i = n - 1; i >= 0; --i) {
        int y = nodes[i].y, z = nodes[i].z;
        auto iter = bounds.upper_bound(y);
        if (iter->second > z) { // 存在 x, y, z 均满足条件的
            ans += 1;
        } else {
            if (bounds[y] < z) {
                // 调整 y 对应的 z 的 upper bound
                bounds[y] = std::max(bounds[y], z);
                for (auto idx = --bounds.find(y); idx->second < z; bounds.erase(idx--)) {
                }
            }
        }
    }

    std::cout << ans;

    return 0;
}
