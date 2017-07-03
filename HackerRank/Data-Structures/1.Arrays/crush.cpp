#include <map>
#include <algorithm>
#include <iostream>
#include <cstdio>

constexpr int M = 2 * 100000;

using namespace std;

std::map<int, int> Map; // mapping
int Bound = 0;

struct query_t {
    int a, b, k;
} Query[M];

int Node[2*2*M];

struct {
    int l, r;
    long long value;
} Tree[M * 2 * 4];

void initialize_tree(int k, int l, int r) {
    Tree[k].l = l;
    Tree[k].r = r;
    if (r - l > 1) {
        int mid = (l + r) >> 1;
        initialize_tree(2*k+1, l, mid);
        initialize_tree(2*k+2, mid, r);
    }
}

void update_segment(int k, int l, int r, int inc) {
    if (l == Tree[k].l && r == Tree[k].r) {
        Tree[k].value += inc;
        return;
    }
    int mid = (Tree[k].l + Tree[k].r) >> 1;
    if (r <= mid) {
        update_segment(2*k+1, l, r, inc);
    }
    else if (l >= mid) {
        update_segment(2*k+2, l, r, inc);
    }
    else {
        update_segment(2*k+1, l, mid, inc);
        update_segment(2*k+2, mid, r, inc);
    }
}

long long query_max(int k) {
    if (Tree[k].l + 1 == Tree[k].r) {
        return Tree[k].value;
    }
    else {
        return Tree[k].value + std::max(query_max(2*k+1), query_max(2*k+2));
    }
}

int main() {
    int n, m;
    scanf("%d %d", &n, &m);
    for (int i = 0; i < m; ++i) {
        scanf("%d %d %d", &Query[i].a, &Query[i].b, &Query[i].k);
        Node[2*i] = Query[i].a;
        Node[2*i+1] = Query[i].b + 1;
    }
    std::sort(Node, Node + 2 * m);
    for_each(Node, Node + 2 * m, [](int x) {
        if (Map.find(x) == Map.end()) {
            Map[x] = Bound++;
        }
    });
    initialize_tree(0, 0, Bound);
    for_each(Query, Query + m, [](query_t &q) {
        update_segment(0, Map[q.a], Map[q.b+1], q.k);
    });
    std::cout << query_max(0) << std::endl;

    return 0;
}

