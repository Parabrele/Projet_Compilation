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
