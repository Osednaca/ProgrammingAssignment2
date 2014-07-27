## Put comments here that give an overall description of what your
## functions do

## This function is like a class it has the get and set classic fuctions
## that allow set and get the value of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function solve the inverse of the matrix given, if the matrix has already 
## solve it show a message and show the result without calculate again, because 
## is in cache


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
