package main
import "fmt"

func test(n int) int {
	var x = 1
	return n + x
}

func main() {
	var x = 6
	fmt.Print(test(x));
}
