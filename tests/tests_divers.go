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




package main
import "fmt"

type T struct {
	x, y int
}

func main() {
	t := new(T)
	fmt.Print(t.y)
	fmt.Print("\n")
	t.x = 1
	p := &t.x
	fmt.Print(*p)
	fmt.Print("\n")
	*p = 2
	fmt.Print(*p)
	fmt.Print("\n")
}



package main
import "fmt"


func main() {
    fmt.Print(5 / 2, "\n")
    fmt.Print(5 % 2, "\n")
    fmt.Print(true || true, "\n")
    fmt.Print(true || true, "\n")
}


package main
import "fmt"

func foo(x int) (int, int) {
	return x, x+1
}

func bar(x int, y int) int {
	return x + y
}

func main() {
	x := bar(foo(3))
	x++
	fmt.Print(x)
}

package main
import "fmt"

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


package main
import "fmt"

func fact(n int) int {
	if n <= 1 {
		return 1;
	}
	return n * fact(n-1);
}

func main() {
  for n := 0; n <= 10; n++ {
	  fmt.Print(fact(n));
	  fmt.Print("\n")
  }
}

package main
import "fmt"

func fib(n int) int {
	a, b := 0, 1
	for ; n > 0; n-- {
		a, b = b, a+b
	}
	return a;
}

func main() {
  for n := 0; n <= 10; n++ {
	  fmt.Print(fib(n));
	  fmt.Print("\n")
  }
}

package main
import "fmt"

type T struct { Print int }
func dis(x int) { fmt.Print(x, "\n") }
func main() { var fmt T; fmt.Print = 42; dis(fmt.Print) }


package main
import "fmt"
func main() {
	fmt.Print("Hello, world\n")
}

package main
import "fmt"

func main() {
	x := 1
	p := &x
	fmt.Print(*p)
	fmt.Print("\n")
	*p = 2
	fmt.Print(*p)
	fmt.Print("\n")
}


package main

import "fmt"

type L struct {
    x int
    y int
}

func foo() L {
    var l L

    l.x = 42
    l.y = 84

    return l
}

func main() {
    l := foo()

    fmt.Print(l, "\n")
}

package main

import "fmt"

func main() {
    s1 := "Hello"
    s2 := "Hello"
    s3 := "Hello World"

    if (s1 == s2) {
        fmt.Print("s1 == s2\n");
    }
    if (s1 == s3) {
        fmt.Print("s1 == s3\n")
    }
}

package main

import "fmt"

type L struct {
    lx int
    ly *M
    lz int
}

type M struct {
    mx int
    my int
}

func main() {
    var l L

    l.lx = 2
    l.ly = new(M)
    l.ly.mx = 3
    l.ly.my = 4
    l.lz = 5

    x := l

    fmt.Print(l.lx, l.ly.mx, l.ly.my, l.lz, "\n")
    fmt.Print(x.lx, x.ly.mx, x.ly.my, x.lz, "\n")

    l.ly.mx = 4
    l.ly.my = 3

    fmt.Print(l.lx, l.ly.mx, l.ly.my, l.lz, "\n")
    fmt.Print(x.lx, x.ly.mx, x.ly.my, x.lz, "\n")
}

package main
import "fmt"

type A struct {
    i int;
    s string;
    p *A;
}

func main() {
    var a A;
    b := new(A)
    a.i = 5
    a.s = "bonjour"
    a.p = &a
    fmt.Print(a, "\n");
    fmt.Print(b, "\n");
    fmt.Print(a.p, "\n");
}



package main
import "fmt"
func main() {
	fmt.Print("a")
	fmt.Print()
	fmt.Print("b\n")
	fmt.Print(true, "\n")
	fmt.Print(false, "\n")
	fmt.Print(1, 2, 3, "\n")
	fmt.Print(1, "2", 3, "\n")
	fmt.Print(1, "2", 3, 4, "5", "\n")
	fmt.Print(1, "2", 3, true, "5", "\n")
	s := "s"
	fmt.Print(1, s, 3, "\n")
	fmt.Print(nil, "\n")
	var p *int = nil
	fmt.Print("a", p, "b\n")
}


package main
import "fmt"

type tree struct {
    is_empty bool; node int;
    ls *tree; rs *tree
}

func nodes(t *tree) int {
    if t.is_empty {
	return 0
    } else {
	return 1 + nodes(t.ls) + nodes(t.rs)
    }
}

func main() {
    a := new(tree)
    a.is_empty = true
    c := new(tree)
    c.is_empty, c.ls, c.rs, c.node = false, a, a, 5
    d := new(tree)
    d.is_empty, d.ls, d.rs, d.node = false, c, a, 3
    fmt.Print(nodes(d))
}

package main
import "fmt"

func foo(x int) (int, int) {
	return x, x+1
}

func main() {
	x, y := foo(20)
	fmt.Print(x+y+1, "\n");
}


package main
import "fmt"

func main() {
	n := 0;
	{
		n := 1;
		if n == 1 {
			fmt.Print("a");
		}
	}
	if n == 0 {
		fmt.Print("b");
	}
	fmt.Print("\n");
}

package main

import "fmt"

func main() {
	x := ""
	s := &x
	fmt.Print(4, *s, 2, "\n")
	x = "Dave"
	fmt.Print("I'm sorry, ", *s, ". I'm afraid I can't do that.\n")
}

package main

import "fmt"

type L struct {
    lx int
    ly M
}

type M struct {
    mx int
    my N
    mz string
}

type N struct {
    nx int
    ny string
}

func main() {
    var l L

    l.lx = 2
    l.ly.mx = 4
    l.ly.my.nx = 5
    l.ly.my.ny = "Hello"
    l.ly.mz = "World"

    fmt.Print(l.lx, l.ly.mx, l.ly.my.nx, l.ly.my.ny, l.ly.mz, "\n")
    fmt.Print(l.lx, l.ly.mx, l.ly.my, l.ly.mz, "\n")
    fmt.Print(l.lx, l.ly, "\n");
    fmt.Print(l, "\n")
}

package main

import "fmt"

type M struct {
    mx int
    my N
    mz string
}

type N struct {
    ny string
}

func main() {
    var m M

    m.mx = 4
    m.my.ny = "Hello"
    m.mz = "World"

    fmt.Print(m, "\n");
}

package main

import "fmt"

type L struct {
	x    int
	next *L
}

func main() {
	z := new(L)
	z.x = 42
	fmt.Print(z.x)
	fmt.Print(z.next)
	fmt.Print("\n")
	z.next = new(L)
	n := z.next
	n.x = 43;
	fmt.Print(z.x)
	fmt.Print(z.next.x)
	fmt.Print(z.next.next)
	fmt.Print("\n")
}

package main
import "fmt"

type A struct {
    a int
    b int
}

func main() {
    x, y := 0, 1
    x, y = y, x
    fmt.Print(x, y, "\n")
    i, j := new(A), new(A)
    i.a = 5
    j.a, j.b = 4, 8
    i, j = j, i
    fmt.Print(i, j, "\n")
}

package main
import "fmt"

func foo(x int) (int, int) { return x, x+1 }
func bar(x,y int) { fmt.Print(x+y) }
func main() { bar(foo(41)) }

package main
import "fmt"

func main() { fmt.Print() }


############## TEST MARCHE PAS ##############
package main
import "fmt"

type T struct { a int; b bool; p *int }

func main() {
	t := new(T)
	t.p = &t.a
	fmt.Print(-t.a)
	fmt.Print(!t.b)
	fmt.Print(*t.p)
}


package main

func foo(x int) int { if x > 0 { return 1 } else { return 2 } }
func main() {}


package main

func f(x bool) { { x := 1; x++ } }
func main() { f(true) }


package main

type A struct { b *B }
type B struct { a A }

func main() {}

package main
import "fmt"

func main() {
	x, _ := 3, 1
	y, _ := 4, 2
	fmt.Print(x+y, "\n");
}

package main
import "fmt"

func g() (int, int, int) {
    return 1, 2, 3
}

func main() {
	var _, x, _ = g()
	fmt.Print(x)
}

package main
import "fmt"

func main() { a, _ := 1, 2; fmt.Print(a) }
