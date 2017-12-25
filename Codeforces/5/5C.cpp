// Codeforces 5C

#include <cstdio>
#include <cstring>
#include <iostream>

const int MAX_NLEN = 10e6 + 10;
char s[MAX_NLEN];
int stktop = 0, open[MAX_NLEN] = {0}, stk[MAX_NLEN] = { 0 };

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::cin >> s;

    int maxlen = 0, count = 1;

    for (int i = 0; i < std::strlen(s); ++i) {
        if (s[i] == '(') {
            stk[stktop++] = i;
        } else {
            if (stktop != 0) {
                int t = stk[--stktop];
                open[i] = open[t-1] + (i - t + 1);
                if (open[i] > maxlen) {
                    maxlen = open[i];
                    count = 1;
                } else if(open[i] == maxlen) {
                    count += 1;
                }
            }
        }
    }

    std::cout << maxlen << " " << count;

    return 0;
}
