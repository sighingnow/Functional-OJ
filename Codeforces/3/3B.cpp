#include <iostream>
#include <cstdio>
#include <vector>
#include <algorithm>
using namespace std;

int n, v;
vector<pair<int, int> > a, b;
vector<int> ka, kb;

bool cmp(const pair<int, int> & l, const pair<int, int> & r) { return r.second < l.second; }

int main() {
    int t, p;
    scanf("%d %d", &n, &v);
    for(int i = 0; i < n; ++i) {
        scanf("%d %d", &t, &p);
        if(t == 1) {
            a.push_back(make_pair(i+1, p));
        }
        else {
            b.push_back(make_pair(i+1, p));
        }
    }
    
    sort(a.begin(), a.end(), cmp);
    sort(b.begin(), b.end(), cmp);
    
    int la = a.size(), lb = b.size();
    ka.push_back(0); kb.push_back(0);
    for(int i = 1; i <= la; ++i) {
        ka.push_back(ka[i-1]+a[i-1].second);
    }
    for(int i = 1; i <= lb; ++i) {
        kb.push_back(kb[i-1]+b[i-1].second);
    }

    int s = 0, ca = 0, cb = 0, ma, mb = min(lb, v/2);
    for(int i = 0; i <= mb; ++i) {
        ma = min(la, v-2*i);
        if(s < ka[ma]+kb[i]) {
            s = ka[ma]+kb[i];
            ca = ma, cb = i;
        }
    }

    printf("%d\n", s);
    ma = 0, mb = 0;
    while(ma < ca) {
        printf("%d ", a[ma++].first);
    }
    while(mb < cb) {
        printf("%d ", b[mb++].first);
    }

    return 0;
}
