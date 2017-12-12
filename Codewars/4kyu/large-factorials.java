import java.math.BigInteger;

public class Kata {
    public static String Factorial(int n) {
        if (n < 0) {
            return "";
        } else if (n == 0) {
            return "1";
        } else {
            BigInteger r = new BigInteger("1");
            for (int i = 1; i <= n; ++i) {
                r = r.multiply(new BigInteger(i + ""));
            }
            return r.toString();
        }
    }
}
