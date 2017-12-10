#include <cstdio>
#include <cstring>
#include <cmath>
#include <iostream>

double unsignal_limit(double v0, double v, double a, double l) {
    double t0 = (v - v0) / a;
    double s0 = (v * v - v0 * v0) / (2.0 * a);
    if (s0 >= l) {
        return (std::sqrt(v0 * v0 + 2.0 * a * l) - v0) / a;
    } else {
        return t0 + (l - s0) / v;
    }
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    double a, v, l, d, w, t;

    std::cin >> a >> v >> l >> d >> w;

    if (w >= v) {
        t = unsignal_limit(0, v, a, l);
    } else {
        double a1 = w * w / (d * 2.0);
        double s = (v * v + v * v - w * w) / (a * 2.0);
        if (a1 <= a) {
            if (s < d) {
                double s1 = (v * v - w * w) / (a * 2.0);
                t = unsignal_limit(0.0, v, a, d - s1) + (v - w) / a + unsignal_limit(w, v, a, l - d);
            } else {
                double vx = std::sqrt(a * (d + w * w / (a * 2.0)));
                t = vx / a + (vx - w) / a + unsignal_limit(w, v, a, l - d);
            }
        } else {
            t = unsignal_limit(0.0, v, a, l);
        }
    }

    std::printf("%.6lf", t);

    return 0;
}
