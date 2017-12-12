#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <vector>

const long long MOD = 1e9 + 7;
const int MAXM = 1e5 + 5;
const int MAXN = 2e5 + 5;

// [a b
//  c d]
struct Mat {
    long long a, b, c, d;
    Mat(): a(0), b(0), c(0), d(0) {}
    Mat(long long a, long long b, long long c, long long d): a(a), b(b), c(c), d(d) {}
    Mat operator +(const Mat & b) {
        Mat r;
        r.a = (this->a + b.a) % MOD;
        r.b = (this->b + b.b) % MOD;
        r.c = (this->c + b.c) % MOD;
        r.d = (this->d + b.d) % MOD;
        return std::move(r);
    }
    Mat operator -(const Mat & b) {
        Mat r;
        r.a = (this->a - b.a + MOD) % MOD;
        r.b = (this->b - b.b + MOD) % MOD;
        r.c = (this->c - b.c + MOD) % MOD;
        r.d = (this->d - b.d + MOD) % MOD;
        return std::move(r);
    }
    Mat operator *(const Mat & b) {
        Mat r;
        r.a = (this->a * b.a % MOD + this->b * b.c % MOD) % MOD;
        r.b = (this->a * b.b % MOD + this->b * b.d % MOD) % MOD;
        r.c = (this->c * b.a % MOD + this->d * b.c % MOD) % MOD;
        r.d = (this->c * b.b % MOD + this->d * b.d % MOD) % MOD;
        return std::move(r);
    }
};

Mat fib[MAXM], path[MAXN], ans, cs[MAXN];
std::vector<int> edges[MAXN];
int w[MAXN];

void dfs(int k, int node) {
    for (int i = 0; i < edges[k].size(); ++i) {
        if (edges[k][i] != node) {
            dfs(edges[k][i], k);
        }
    }
    for (int i = 0; i < edges[k].size(); ++i) {
        int t = edges[k][i];
        if (t == node) {
            continue;
        }
        ans = ans + fib[w[k]] * path[t] * (cs[k] - path[t]);
        path[k] = path[k] + path[t];
    }
    path[k] = path[k] * fib[w[k]] + fib[w[k]];
    cs[node] = cs[node] + path[k];
    ans = ans + path[k] + path[k] - fib[w[k]];
}

int main() {
    int n, u, v;
    scanf("%d", &n);
    for (int i = 1; i < n; ++i) {
        scanf("%d %d", &u, &v);
        edges[u].push_back(v);
        edges[v].push_back(u);
    }
    for (int i = 1; i <= n; ++i) {
        scanf("%d", w+i);
    }
    // Calc Fibonacci
    Mat p(1, 1, 1, 0);
    fib[0].a = fib[0].d = 1;
    for (int i = 1; i < MAXM; ++i) {
        fib[i] = fib[i-1] * p;
    }
    // DFS
    dfs(1, 0);
    // Output
    printf("%lld\n", (ans.c + ans.d) % MOD);
    return 0;
}

