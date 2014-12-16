function main() int
  println(fib(read(int)))
  println(test(4.0))
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

function test(float a) float
  return a / 2.0
end