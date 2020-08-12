## Assignment 2 

## makeCacheMatrix function creates a vector of functions
## and caches x matrix

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve function takes vector x and uses its functions and
## cache to find the inverse of A

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  # Solve for Inverse
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

## Calling functions section

#  Matrix initializing, square matrix
A = matrix(
  c(2, 4, 3, 1, 5, 7, 1, 2, 4), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows

det(A) # Must have a determinant

x <- makeCacheMatrix(A)
i <- cacheSolve(x)
