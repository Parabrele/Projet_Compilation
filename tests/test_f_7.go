package main
import "fmt"

func fact_it(n int) int {
    r := 1;
    for ; n > 1; n-- {
	r = r * n;
    }
    return r;
}

func fact_rec(n int) int {
	if n <= 1 {
		return 1;
	}
	return n * fact(n-1);
}

func fib(n int) int {
	a, b := 0, 1
	for ; n > 0; n-- {
		a, b = b, a+b
	}
	return a;
}


func main() {
    fmt.Print("factorielle itérative :", "\n");
    for n := 0; n <= 10; n++ {
	if n > 4 {
	    fmt.Print(n, "! = ", fact_it(n), "\n");
	} else {
	    fmt.Print(n, "?\n");
	}
    }
	
    fmt.Print("factorielle récursive :", "\n");
	for n := 0; n <= 10; n++ {
		fmt.Print(fact_rec(n));
		fmt.Print("\n")
	}

    fmt.Print("fibonacci :", "\n");
	for n := 0; n <= 10; n++ {
		fmt.Print(fib(n));
		fmt.Print("\n")
	}
}
