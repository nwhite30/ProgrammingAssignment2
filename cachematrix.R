## Put comments here that give an overall description of what your
## functions does

## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will 
## not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  mo <- NULL
  set <- function(y){
    x  <<- y
    mo <<- NULL
  }
  get  <- function() x
  set_inverse <- function(inverse)  mo <<- inverse
  get_inverse <- function() mo
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mo <- x$get_inverse()
  if(!is.null(mo)){
    message("getting cached data")
    return(mo)
  }
  data <- x$get()
  mo <- solve(data, ...)
  x$set_inverse(mo)
  mo
}

mo <- matrix(c(1,2,3,4),2,2)
makeCacheMatrix(mo)
mo1 <- makeCacheMatrix(mo)
cacheSolve(mo1)

# Note to tester:  For whatever reason I had to run each function separately, 
# followed by each line of the testing separately. It worked fine on my work
# computer when I ran it all at once, but was finicky on my home computer.


## SOLUTION
## > mo <- matrix(c(1,2,3,4),2,2)
## > makeCacheMatrix(mo)
## $`set`
## function (y) 
## {
##   x <<- y
##   mo <<- NULL
## }
## <environment: 0x000001906e31b3d8>

##   $get
## function () 
##   x
##<environment: 0x000001906e31b3d8>

##  $set_inverse
##function (inverse) 
##  mo <<- inverse
##<environment: 0x000001906e31b3d8>

##   $get_inverse
## function () 
##   mo
## <environment: 0x000001906e31b3d8>

##  > mo1 <- makeCacheMatrix(mo)
##  > cacheSolve(mo1)
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
