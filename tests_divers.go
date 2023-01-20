package main
import "fmt"

type T struct { a,b int }

func foo(t  T) { t.a = t.a + 1; t.b = t.b + 1;  }
func bar(t *T) { t.a = t.a + 1; t.b = t.b + 1;  }

func main() {
	var t T
	t.a = 1
	t.b = 2
	fmt.Print(t.a, t.b, "\n");
	foo(t)
	fmt.Print(t.a, t.b, "\n");
	bar(&t)
	fmt.Print(t.a, t.b, "\n");
	fmt.Print("5" + 37);
}

#######################

func main() {
	fmt.Print("ah");
}

####################

func fact(n int) int {
    r := 1;
    for ; n > 1; n-- {
	r = r * n;
    }
    return r;
}

func main() {
    fmt.Print(1 > 1, "\n");
    for n := 0; n <= 10; n++ {
	if n > 4 {
	    fmt.Print(n, "! = ", fact(n), "\n");
	} else {
	    fmt.Print(n, "?\n");
	}
    }
}

####################

package main
import "fmt"


func main() {
    fmt.Print(5 / 2, "\n")
    fmt.Print(5 % 2, "\n")
    fmt.Print(true || true, "\n")
    fmt.Print(true || true, "\n")
}


func main() {
    fmt.Print(true, "\n")
}


func main() {
	fmt.Print(1 + 1);
	fmt.Print(1 - 1);
	fmt.Print(1 * 1);
	fmt.Print(1 / 1);
	fmt.Print(1 % 1);

	fmt.Print(true  && true, "\n");
	fmt.Print(true  && false, "\n");
	fmt.Print(false  && true, "\n");
	fmt.Print(false  && false, "\n");

	fmt.Print("\n");

	fmt.Print(true  || true, "\n");
	fmt.Print(true  || false, "\n");
	fmt.Print(false || true, "\n");
	fmt.Print(false  || false, "\n");

	fmt.Print("\n");

	fmt.Print(2 > 1, "\n");
	fmt.Print(2 >= 1, "\n");
	fmt.Print(1 < 2, "\n");
	fmt.Print(1 <= 2, "\n");
	fmt.Print(2 <= 2, "\n");
	fmt.Print(2 < 2, "\n");

	var x = 2

	fmt.Print("\n");
	fmt.Print(x, "\n");
	fmt.Print(&x, "\n");
	fmt.Print(*&x, "\n");
}




func fact(n int) int {
	return 2
}

func main() {
	fmt.Print(fact(2));
}


func fact(n int) int {
	return n
}

func main() {
	fmt.Print(fact(2));
}



func fact(n int) int {
	var x = 1
	return n + x
}

func main() {
	fmt.Print(fact(2));
}
