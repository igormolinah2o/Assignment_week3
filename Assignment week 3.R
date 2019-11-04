print("R PROGRAMMING ASSIGNMENT WEEK 3")

print("En github ahora")

> makeVector <- function(x = numeric()) {
  +     m <- NULL
  +     set <- function(y) {
    +         x <<- y
    +         m <<- NULL
    +     }
  +     get <- function() x
  +     setmean <- function(mean) m <<- mean
  +     getmean <- function() m
  +     list(set = set, get = get,
             +          setmean = setmean,
             +          getmean = getmean)
  + }
> cachemean <- function(x, ...) {
  +     m <- x$getmean()
  +     if(!is.null(m)) {
    +         message("getting cached data")
    +         return(m)
    +     }
  +     data <- x$get()
  +     m <- mean(data, ...)
  +     x$setmean(m)
  +     m
  + }
> makeCacheMatrix <- function(x = matrix()) {
  +     inv <- NULL
  +     set <- function(y){
    +         x <<- y
    +         inv <<- NULL
    +     }
  +     get <- function() x
  +     setInverse <- function(solveMatrix) inv <<- solveMatrix
  +     getInverse <- function() inv
  +     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  + }
> cacheSolve <- function(x, ...) {
  +     ## Return a matrix that is the inverse of 'x'
    +     inv <- x$getInverse()
    +     if(!is.null(inv)){
      +         message("getting cached data")
      +         return(inv)
      +     }
    +     data <- x$get()
    +     inv <- solve(data)
    +     x$setInverse(inv)
    +     inv      
    + }
> TestMatrix <- matrix(1:4,2,2)
> TestMatrix
[,1] [,2]
[1,]    1    3
[2,]    2    4
> 
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$getMatrix()
Error: attempt to apply non-function
> CacheMatrix$getInverse()
NULL
> 
  > cacheSolve(CacheMatrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(CacheMatrix)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
  > 
  > ####Test 2 [2*2 Matrix] #####
> TestMatrix <- matrix(c(1,5,8,2),2,2)
> TestMatrix
[,1] [,2]
[1,]    1    8
[2,]    5    2
> 
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$getMatrix()
Error: attempt to apply non-function
> CacheMatrix$getInverse()
NULL
> 
  > cacheSolve(CacheMatrix)
[,1]        [,2]
[1,] -0.05263158  0.21052632
[2,]  0.13157895 -0.02631579
> cacheSolve(CacheMatrix)
getting cached data
[,1]        [,2]
[1,] -0.05263158  0.21052632
[2,]  0.13157895 -0.02631579
> 
  > 
  > ####Test 3 [3*3 Matrix] --> singulat Matrix #####
> #matrix(1:9,3,3) is not possible (singule matrix) because it is giving det(A)  = 0
  > #matrix = 1/det(A)[3,-6,3,-6,12,-6,3,-6,3] |det(A) = 1/(1*3 +4* (-6) + 7 *3) = 1/0
  > TestMatrix <- matrix(1:9,3,3)
> TestMatrix
[,1] [,2] [,3]
[1,]    1    4    7
[2,]    2    5    8
[3,]    3    6    9
> 
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$getMatrix()
Error: attempt to apply non-function
> CacheMatrix$getInverse()
NULL
> 
  > cacheSolve(CacheMatrix)
Error in solve.default(data) : 
  Lapack routine dgesv: system is exactly singular: U[3,3] = 0
Called from: solve.default(data)
Browse[1]> cacheSolve(CacheMatrix)
Error during wrapup: Lapack routine dgesv: system is exactly singular: U[3,3] = 0
Browse[1]> 
  > ####Test 4 [3*3 Matrix]#####
> TestMatrix <- matrix(1:8,3,3)
Warning message:
  In matrix(1:8, 3, 3) :
  data length [8] is not a sub-multiple or multiple of the number of rows [3]
> TestMatrix
[,1] [,2] [,3]
[1,]    1    4    7
[2,]    2    5    8
[3,]    3    6    1
> 
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$getMatrix()
Error: attempt to apply non-function
> CacheMatrix$getInverse()
NULL
> 
  > cacheSolve(CacheMatrix)
[,1]       [,2]   [,3]
[1,] -1.7916667  1.5833333 -0.125
[2,]  0.9166667 -0.8333333  0.250
[3,] -0.1250000  0.2500000 -0.125
> cacheSolve(CacheMatrix)
getting cached data
[,1]       [,2]   [,3]
[1,] -1.7916667  1.5833333 -0.125
[2,]  0.9166667 -0.8333333  0.250
[3,] -0.1250000  0.2500000 -0.125
> 
  > 
  > ####Test 5 [4*4 Matrix]#####
> TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
> TestMatrix
[,1] [,2] [,3] [,4]
[1,]    2    3    6    4
[2,]    3    7    8    5
[3,]    5    4    0    6
[4,]    1    5    0    0
> 
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$getMatrix()
Error: attempt to apply non-function
> CacheMatrix$getInverse()
NULL
> 
  > cacheSolve(CacheMatrix)
[,1] [,2]       [,3]       [,4]
[1,]  40.0  -30 -1.6666667  19.333333
[2,]  -8.0    6  0.3333333  -3.666667
[3,]   9.5   -7 -0.5000000   4.500000
[4,] -28.0   21  1.3333333 -13.666667
> cacheSolve(CacheMatrix)
getting cached data
[,1] [,2]       [,3]       [,4]
[1,]  40.0  -30 -1.6666667  19.333333
[2,]  -8.0    6  0.3333333  -3.666667
[3,]   9.5   -7 -0.5000000   4.500000
[4,] -28.0   21  1.3333333 -13.666667
> 
  > 
  > ####Test 6 [4*4 Matrix]  --> singular Matrix #####
> TestMatrix <- matrix(5:21,4,4)
Warning message:
  In matrix(5:21, 4, 4) :
  data length [17] is not a sub-multiple or multiple of the number of rows [4]
> TestMatrix
[,1] [,2] [,3] [,4]
[1,]    5    9   13   17
[2,]    6   10   14   18
[3,]    7   11   15   19
[4,]    8   12   16   20
> 
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
> CacheMatrix$getMatrix()
Error: attempt to apply non-function
> CacheMatrix$getInverse()
NULL
> 
  > cacheSolve(CacheMatrix)
Error in solve.default(data) : 
  Lapack routine dgesv: system is exactly singular: U[3,3] = 0
Called from: solve.default(data)
Browse[1]> cacheSolve(CacheMatrix)
Error during wrapup: Lapack routine dgesv: system is exactly singular: U[3,3] = 0
Browse[1]>