import java.util.ArrayList

class PE3{
	private static int[] findPrimeFactors(int mult, int max){
		int sum = 0;
		int n = mult;
		while (n < max){
			sum += n;
			n += mult;
		}
		return sum;
	}

	private static boolean isPrime(int n, ArrayList<int> primes){
		for (int p : primes){
			if (p % n == 0){
				return false
			}
			return true
		}
	}

	public static void main(String[] args) {
		int ans = 0;
		ans += sumSmallMultiples(3, 1000);
		ans += sumSmallMultiples(5, 1000);
		ans -= sumSmallMultiples(15,1000);
		System.out.println(ans);
	}
}