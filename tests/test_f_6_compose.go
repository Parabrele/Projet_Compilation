package main
import "fmt"

func f(n, m, t int) int {
	var x = 1
	return n * m / t + x
}

func g(n int) int, int, int {
	var x = 1
	return n + x, n / x, x
}

func main() {
	var x = 6
	fmt.Print(f(g(x)));
}
