## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to :
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve calculates the matrix inverse of the special "matrix" created with the function makeCacheMatrix. 
## However, it first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the matrix inverse of the data and sets the value of the matrix inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Example of resolution 
## > x <- rbind(c(2,-4,4),c(2,0,1),c(4,1,1))
## > x
## [,1] [,2] [,3]
## [1,]    2   -4    4
## [2,]    2    0    1
## [3,]    4    1    1
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2] [,3]
## [1,]    2   -4    4
## [2,]    2    0    1
## [3,]    4    1    1
## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]  0.5   -4    2
## [2,] -1.0    7   -3
##  -1.0    9   -4
## > cacheSolve(m)
## getting cached data
## [,1] [,2] [,3]
## [1,]  0.5   -4    2
## [2,] -1.0    7   -3
## [3,] -1.0    9   -4