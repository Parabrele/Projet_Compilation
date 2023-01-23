package main

import "fmt"

type S struct{
	a int
	b int
	c string
}

func f(p *int) int{
	*p++
	return *p
}


func createS(a int, b int, c string) S {
	var s S
	s.a, s.b, s.c = a , b , c
	return s
}

func afficheS(s S){
	fmt.Print(s,"\n")
}


func gg() *int {
	var i = 75
	return &i
}

func main(){
	var i = 0;
	var s = createS(5,10,"ok")
	var copie = &s
	s.c = "gg"
	fmt.Print(f(&i),f(&i),f(&i))
	fmt.Print(copie,"\n",s,"\n")
	afficheS(s)
	var ptr = gg()
	var a,b,c,d = 5,7,8,9;
	fmt.Print(*ptr, "\n")
	_,_,_,_ = a,b,c,d
}
