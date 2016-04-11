extern printd(x);

def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2);

printd(fib(10));
