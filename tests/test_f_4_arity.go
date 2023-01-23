package main
import "fmt"

func test(n, m, t int) int {
	var x = 1
	return n * m / t + x
}

func main() {
	var x = 6
	fmt.Print(test(x, 2, x));
}
