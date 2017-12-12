#include <iostream>
#include <cstdio>
#include <cmath>
#include <cstdlib>

float inputs[101][11], expects[101], outputs[101];
float coffs[11], input[11];

const float RATE = 0.001, LOSS = 0.00005;
const int MAX_ITER = 100000;

void linear_regression(int F, int N, float inputs[][11], float expects[], float outputs[], float coffs[]) {
    float max_loss = 1.0, Ninv = 1.0 / N, loss, update;
    int iter = 0;
    while (iter++ < MAX_ITER && max_loss > LOSS) {
        float s = 0;
        max_loss = 0;
        for (int i = 0; i < N; ++i) {
            s = 0;
            for (int j = 0; j <= F; ++j) {
                s += inputs[i][j] * coffs[j];
            }
            outputs[i] = s;
        }
        for (int j = 0; j <= F; ++j) {
            loss = 0;
            for (int i = 0; i < N; ++i) {
                loss += (outputs[i] - expects[i]) * inputs[i][j];
            }
            loss *= Ninv;
            update = loss * RATE;
            coffs[j] -= update;
            max_loss = std::max(max_loss, std::abs(loss));
        }
    }
}

inline float predict(int F, float coffs[], float input[]) {
    float result = 0;
    for (int i = 0; i <= F; ++i) {
        result += coffs[i] * input[i];
    }
    return result;
}

int main() {
    int F, N, T;
    scanf("%d %d", &F, &N);
    for (int i = 0; i < N; ++i) {
        inputs[i][0] = 1;
        for (int j = 1; j <= F; ++j) {
            scanf("%f", &inputs[i][j]);
        }
        scanf("%f", &expects[i]);
    }
    for (int i = 0; i <= F; ++i) {
        coffs[i] = (float)rand()/(float)(RAND_MAX);
    }
    linear_regression(F, N, inputs, expects, outputs, coffs);

    scanf("%d", &T);
    input[0] = 1.0;
    for (int i = 0; i < T; ++i) {
        for (int j = 1; j <= F; ++j) {
            scanf("%f", &input[j]);
        }
        printf("%f\n", predict(F, coffs, input));
    }

    return 0;
}

