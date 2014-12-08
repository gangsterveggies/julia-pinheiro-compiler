a = 0
b = 1
c = 10
while c > 0
  tmp = b
  b = a + b
  a = tmp
  c = c - 1
end
println(a,c)
println(a)
