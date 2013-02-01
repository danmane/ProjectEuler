class PE2{
	private static int sumEvenFibs(int max){
		int sum = 0;
		int past=0, current=1;
		int swap;

		while (current <= max){
			if (current % 2 == 0){
				sum += current;
			}
			swap = current;
			current = past + current;
			past = swap;
		}
		return sum;
	}
	public static void main(String[] args) {
		int ans;
		ans = sumEvenFibs(4000000);
		System.out.println(ans);
	}
}