// HackerRank challenge - Algorithms - 4-4: correctness-invariant

#include <cstdio>
#include <algorithm>
using namespace std;

int main() {
    int N;
    scanf("%d", &N);
    int arr[1500+5], i;
    for(i = 0; i < N; i++) {
        scanf("%d", &arr[i]);
    }
    std::sort(arr, arr+N);
    for(i = 0; i < N; i++) {
        printf("%d ", arr[i]);
    }
    return 0;
}
