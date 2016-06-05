merge <- function(a, b, f) {
  m = length(a)
  n = length(b)
  i = 1
  j = 1
  o = c()
  while (i <= m && j <= n) {
    if (f(a[i], b[j])) {
      o <- c(o, a[i])
      i <- i + 1
    }
    else {
      o <- c(o, b[j])
      j <- j + 1
    }
  }
  while (i <= m) {
    o <- c(o, a[i])
    i <- i + 1
  }
  while (j <= n) {
    o <- c(o, b[j])
    j <- j + 1
  }
  return(o)
}

leq <- function(a, b) return(a <= b)

mergesort <- function(l, f) {
  n = length(l)
  if (n == 1) {
    return(l)
  }
  else {
    midpt <- n %/% 2
    left  <- mergesort(l[1:midpt], f)
    right <- mergesort(l[(midpt + 1):n], f)
    return(merge(left, right, f))
  }
}