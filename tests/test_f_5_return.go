package main
import "fmt"

func test(n int) (int, int, string) {
	var x = 1
	return n + x, n / x, "coucou"
}

func main() {
	var x = 6;
	var y, z, text = test(x)
	fmt.Print(y, z, text);
}
