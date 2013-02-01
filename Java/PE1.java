class PE1{
	private static int sumSmallMultiples(int mult, int max){
		int sum = 0;
		int n = mult;
		while (n < max){
			sum += n;
			n += mult;
		}
		return sum;
	}
	public static void main(String[] args) {
		int ans = 0;
		ans += sumSmallMultiples(3, 1000);
		ans += sumSmallMultiples(5, 1000);
		ans -= sumSmallMultiples(15,1000);
		System.out.println(ans);
	}
}