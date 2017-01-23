## Below are two functions below are used to create a special object that stores a matrix and performs 
## a matrix inversion.

## This function creates a matrix and a list of four functions set the value of a matrix, get the value of
## a matrix, set the inversion value of a matrix and get the inversion value of a matrix.
##
##  Example:
## a <- makeCacheMatrix(matrix(1:4,2,2))


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inversion of a matrix created by the above funtion.  However, it first checks
## to see if the inversion has already been created.  If so, it gets the inversion from the cache and skips
## the overhead of recreating it.

##
##  Example:
## > cacheSolve(a)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##  > cacheSolve(a)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > a$set(matrix(4:7,2,2))
## > cacheSolve(a)
## [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2
## > cacheSolve(a)
## getting cached data
## [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2
## > 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
