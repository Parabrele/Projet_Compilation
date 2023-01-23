package main
import "fmt"

type A struct {
    i int;
    s string;
    p *A;
}

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
	
    var a A;
    b := new(A)
    a.i = 5
    a.s = "bonjour"
    a.p = &a
    fmt.Print(a, "\n");
    fmt.Print(b, "\n");
    fmt.Print(a.p, "\n");
}
