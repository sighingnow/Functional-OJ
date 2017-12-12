#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <algorithm>

int M[101][11][5][8][26];

int main() {
    char cmd, k;
    int T, d, d_end, p, c, s, r, count;
    scanf("%d\n", &T);
    while (T--) {
        while ((cmd = getchar()) != 'S' && cmd != 'Q') {
            //
        }
        if (cmd == 'S') {
            scanf("%d %d", &d, &p);
            k = getchar();
            if (k == '.') {
                scanf("%d %d", &c, &s);
            } else {
                c = 0;
                scanf("%d", &s);
            }
            k = getchar();
            if (k == '.') {
                scanf("%d\n", &r);
            } else {
                r = 0;
            }
            // insert
            printf("%c: %d %d %d %d %d\n", cmd, d, p, c, s, r);
            if (c != 0 && r != 0) {
                M[d][p][c][s][r] += 1;
                M[d][p][c][s][0] += 1;
                M[d][p][c][0][0] += 1;
                M[d][p][0][s][r] += 1;
                M[d][p][0][s][0] += 1;
                M[d][p][0][0][0] += 1;
                M[d][0][0][s][r] += 1;
                M[d][0][0][s][0] += 1;
                M[d][0][0][0][0] += 1;
            } else if (c != 0 && r == 0) {
                M[d][p][c][s][0] += 1;
                M[d][p][c][0][0] += 1;
                M[d][p][0][s][0] += 1;
                M[d][p][0][0][0] += 1;
                M[d][0][0][s][0] += 1;
                M[d][0][0][0][0] += 1;
            } else if (c == 0 && r != 0) {
                M[d][p][0][s][r] += 1;
                M[d][p][0][s][0] += 1;
                M[d][p][0][0][0] += 1;
                M[d][0][0][s][r] += 1;
                M[d][0][0][s][0] += 1;
                M[d][0][0][0][0] += 1;
            } else if (c == 0 && r == 0) {
                M[d][p][0][s][0] += 1;
                M[d][p][0][0][0] += 1;
                M[d][0][0][s][0] += 1;
                M[d][0][0][0][0] += 1;
            }
        }
        else {
            scanf("%d", &d);
            k = getchar();
            if (k == '.') {
                scanf("%d %d", &d_end, &p);
            } else {
                d_end = d;
                scanf("%d", &p);
            }
            if (p == -1) {
                p = 0;
            }
            k = getchar();
            if (k == '.') {
                scanf("%d %d", &c, &s);
            } else {
                c = 0;
                scanf("%d", &s);
            }
            if (s == -1) {
                s = 0;
            }
            k = getchar();
            if (k == '.') {
                scanf("%d\n", &r);
            } else {
                r = 0;
            }
            // query
            printf("%c: %d %d %d %d %d %d\n", cmd, d, d_end, p, c, s, r);
            count = 0;
            for (int i = d; i <= d_end; ++i) {
                count += M[i][p][c][s][r];
            }
            printf("%d\n", count);
        }
    }

    return 0;
}


