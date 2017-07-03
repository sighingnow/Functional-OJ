#include <stdio.h>

int arr[1000010];
int stack[1000010];

#define max(a, b) ((a) > (b) ? (a) : (b))

int main() {
    int n, i, top = 0, r = 0;
    scanf("%d", &n);
    for (i = 0; i < n; ++i) {
        scanf("%d", arr+i);
    }
    for (i = 0; i < n; ++i) {
        while (top) {
            r = max(r, arr[i] ^ stack[top-1]);
            if (arr[i] < stack[top-1]) {
                --top;
            }
            else {
                break;
            }
        }
        stack[top++] = arr[i];
    }

    printf("%d", r);

    return 0;
}

