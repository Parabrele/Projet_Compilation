package main
import "fmt"

type T struct {
	x, y int
}

type A struct {
	x int
	t T
	y int
}

func main() {
	var t T
	t.x = 1
	t.y = 2
	fmt.Print(t.y, "\n")
	var p = t.x
	fmt.Print(p, "\n")
	p = 2
	fmt.Print(p, "\n")
	fmt.Print(t, "\n")

	fmt.Print("\n")

	var a A
	a.x, a.y = 3, 4
	a.t = t
	fmt.Print(a, "\n")
	
}
