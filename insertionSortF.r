#!/usr/bin/Rscript
# insertionSort.r

insert <- function(f, element, list) {
  n <- length(list)
  if (n == 0) {
    return (c(element))
  }
  if (!f(element, list[n])) {
    list[n + 1] <- element
  }
  else {
    list[n + 1] <- list[n]
    k <- n - 1
    while (k > 0 && f(element, list[k + 1])) {
      list[k + 1] <- list[k]
      k <- k - 1
    }
    list[k + 1] <- element
  }
  return (list)
}

leq <- function(a, b) {
  return (a <= b)
}

insertionSortF <- function(l) {
  n = length(l)
  if (n == 0) {
    return []
  } else {
    insert(leq, l[1], sort(l[2:n]))
  }
}

