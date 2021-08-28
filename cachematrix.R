## Put comments here that give an overall description of what your
## functions do

## This is a function that creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) m <<- inv
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of a special vector

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#Testing 2 functions
#test_matrix = makeCacheMatrix(matrix(c(11,12,13,14), nrow=2, ncol=2))
#test_matrix$get() 

#cacheSolve(test_matrix)

#test_matrix$getinverse()

#cacheSolve(test_matrix)