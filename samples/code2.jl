function main() int
  a = new int [5]
  it = 0
  while it < 5
    if it == 0
      a[it] = read(int)
    else
      prev = a[it - 1]
      a[it] = read(int) + prev
    end
    it = it + 1
  end

  it = 0
  while it < 5
    println(a[it])
    it = it + 1
  end

  return 0
end
