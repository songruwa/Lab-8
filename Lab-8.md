Lab 8
================
Songru Wang

## Problem 2

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  # return(x)
  x
}
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n * k, lambda), nrow = n, ncol = k, byrow = TRUE)
}
microbenchmark::microbenchmark(
  fun1(n = 1000),
  fun1alt(n = 1000), unit="relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq      max neval
    ##     fun1(n = 1000) 35.13561 35.66572 40.31013 43.85337 53.61735 6.625602   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000 1.000000   100
    ##  cld
    ##    b
    ##   a

## Problem 3

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)
# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2alt <- function(x) {
  # Position of the max value per row of x.
  idx <- max.col(t(x)) 
  
  # Do something to get the actual max value
  # x[cbind(1, 15)] ~ x[1, 15]
  # Want to access x[1, 16], x[4, 1]
  # x[rbind(c(1, 16), c(4, 1))]
  # Want to access x[4, 16], x[4, 1]
  # x[cbind(4, c(16, 1))]
  x[ cbind(idx, 1:ncol(x)) ]
}
# Do we get the same?
all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
x <- matrix(rnorm(5e4), nrow=10)
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval cld
    ##     fun2(x) 11.71247 8.676052 8.991802 9.109145 9.874139 3.530943   100   b
    ##  fun2alt(x)  1.00000 1.000000 1.000000 1.000000 1.000000 1.000000   100  a

Example of the max.col function… what just happened?

``` r
set.seed(42343)
M <- matrix(runif(12), ncol = 4)
M # How does it look?
```

    ##           [,1]      [,2]       [,3]      [,4]
    ## [1,] 0.5193214 0.7539021 0.01253299 0.7891065
    ## [2,] 0.4644698 0.4484567 0.06041682 0.1218734
    ## [3,] 0.7760090 0.1781403 0.76544068 0.6561172

``` r
fun2(M)
```

    ## [1] 0.7760090 0.7539021 0.7654407 0.7891065

``` r
t(M) # Transpose M...
```

    ##            [,1]       [,2]      [,3]
    ## [1,] 0.51932135 0.46446984 0.7760090
    ## [2,] 0.75390212 0.44845674 0.1781403
    ## [3,] 0.01253299 0.06041682 0.7654407
    ## [4,] 0.78910647 0.12187338 0.6561172

``` r
idx <- max.col(t(M)) # Then the max.col
# c(3, 1, 3, 1)
idx
```

    ## [1] 3 1 3 1

``` r
# How do the coordinates look like?
cbind(idx, 1:ncol(M))
```

    ##      idx  
    ## [1,]   3 1
    ## [2,]   1 2
    ## [3,]   3 3
    ## [4,]   1 4

``` r
# The final result
M[cbind(idx, 1:ncol(M))]
```

    ## [1] 0.7760090 0.7539021 0.7654407 0.7891065

## Question 4: Bootstrap

``` r
library(parallel)
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  cl <- makePSOCKcluster(ncpus)
  
  # STEP 2: GOES HERE
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
  clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
  
  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl = cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  stopCluster(cl)
  
  ans
  
}
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))
# DATA SIM
set.seed(1)
n <- 500; R <- 5e3
x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)
# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)
# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1395732 0.05291612
    ## x            4.8686527 5.04503468

Is it faster?

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##   0.136   0.023   5.458

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##   0.193   0.026   3.669
