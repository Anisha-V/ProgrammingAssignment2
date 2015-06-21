## These functions are used to calculate the inverse of a user defined square matrix. the first function sets the matrix  
## and calculates its inverse and the second function checks the cache to see if the inverse is already available in the
## cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##this function first checks if the value of getmatrix function which is the inverse of the matrix is not null,
## in which case it returns the value from the previous computation. This is only possible if we do not reassign the 
## set function.  ELSE it gets the newvalues of the matrix and computes the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
## Testing with a square matrix
test1 <- makeCacheMatrix ()
test1$set (matrix (c(7,4,3,73,67,45,13,31,12),3,3, byrow = T))
cacheSolve(test1)
