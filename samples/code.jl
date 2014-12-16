function main() int
  println(fib(8))
  return 0
end
function fib(int b) int
  if (b == 0)
    return 0
  elseif (b == 1)
    return 1
  else
    return fib(b - 1) + fib(b - 2)
  end
end
