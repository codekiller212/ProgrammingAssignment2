## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function to initialize the cache matrix and its functionalities with
## respect to the input matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Ensure to initialize the cache matrix to NULL.
  m <- NULL
  set <- function(i){
    x <<- i
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function


## Function to return of the inverse of the matrix if present in the cache else
## solve the matrix for its inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First get the inverse of the matrix.
  m <- x$getInverse()
  ## If cache not null, we can return the cached matrix.
  if(!is.null(m)){
    message("Getting cached data..")
    return(m)
  }
  ## Else calculate the inverse of the matrix and return the result.
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
