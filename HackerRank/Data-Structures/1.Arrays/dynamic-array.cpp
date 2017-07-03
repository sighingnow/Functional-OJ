#include <cstdio>
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

vector<int> pool[100010];

int main() {
    int n, q, k, x, y, idx;
    int last = 0;
    scanf("%d %d", &n, &q);
    for(int i = 0; i < q; ++i) {
        scanf("%d %d %d", &k, &x, &y);
        idx = (x ^ last) % n;
        if (k == 1) {
            pool[idx].push_back(y);
        }
        else {
            last = pool[idx][y % pool[idx].size()];
            printf("%d\n", last);
        }
    }

    return 0;
}


