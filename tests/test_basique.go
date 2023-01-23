package main
import "fmt"

func main() {
	fmt.Print("ah", "\n")
	
	fmt.Print("\n", "operations arithmetiques :", "\n");

    fmt.Print(-1, "\n")
    fmt.Print(5 + 2, "\n")
    fmt.Print(5 - 2, "\n")
    fmt.Print(5 * 2, "\n")
    fmt.Print(5 / 2, "\n")
    fmt.Print(5 % 2, "\n")
    fmt.Print(true, "\n")

	fmt.Print("\n", "and :", "\n");
	
	fmt.Print(true  && true, "\n");
	fmt.Print(true  && false, "\n");
	fmt.Print(false  && true, "\n");
	fmt.Print(false  && false, "\n");

	fmt.Print("\n", "or :", "\n");

	fmt.Print(true  || true, "\n");
	fmt.Print(true  || false, "\n");
	fmt.Print(false || true, "\n");
	fmt.Print(false  || false, "\n");

	fmt.Print("\n", "comparaisons :", "\n");

	fmt.Print(2 > 1, "\n");
	fmt.Print(2 >= 1, "\n");
	fmt.Print(1 < 2, "\n");
	fmt.Print(1 <= 2, "\n");
	fmt.Print(2 <= 2, "\n");
	fmt.Print(2 < 2, "\n");

	fmt.Print("\n", "variable :", "\n");

	var x = 2
	var p = &x

	fmt.Print(x, "\n");
	fmt.Print(&x, "\n");
	fmt.Print(*&x, "\n");

	fmt.Print(*p, "\n");
	x = 3
	fmt.Print(*p, "\n");

	fmt.Print("\n", "boucle for :", "\n");

	for n := 0; n < 10; n++ {
		fmt.Print(n);
	}
	
	fmt.Print("\n", "if :", "\n");

	if true {
	    fmt.Print("e1", "\n");
	} else {
	    fmt.Print("e2", "\n");
	}
	if false {
	    fmt.Print("e1", "\n");
	} else {
	    fmt.Print("e2", "\n");
	}
}
