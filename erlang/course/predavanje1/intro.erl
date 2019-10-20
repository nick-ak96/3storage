The shell
---------

$ erl
Erlang R16B ...
Eshell V5.9 (abort with ^G)
1> 123456 * 223344.
27573156864

The = operator
--------------

2> X = 123.
123
3> X * 2.
246

4> X = 999.
** exception error: no match of right hand side value 999

1> abc=123.
** exception error: no match of right hand side value 123

Hello world
-----------

$ erl
Erlang R16B ...
1> c(hello).
{ok,hello}
2> hello:start().
Hello world
ok
3> halt().
$

Compiling
---------

$ erlc hello.erl
$ erl -noshell -s hello start -s init stop
Hello world


