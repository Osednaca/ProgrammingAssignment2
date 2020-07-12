## Coursera, R Programming Course, July, 2020
## Testing:
## Open a R terminal
## > m <- matrix(1:12, nrow = 2, ncol = 2)
## > mc <- makeCacheMatrix(m)
## > cacheSolve(mc)

## To see cache in action run again:
## > cacheSolve(mc)


## This function creates a new, unique environment. 
# The inverse matrix is cached inside the object m, in the main 
# environment, which is unique for EACH instance the function is called.
## The output of the function is a list with 5 named elements, which are 
# the five functions defined in here: setmatrix, getmatrix, setinverse, 
# getinverse and getenv

## Arguments: 
## x = The matrix which inverse to be calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## Get the value of the matrix
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##return special object
}

## This function use the R solve function to find the inverse of the matrix given, 
## if the matrix is already  in cache it show a message and show the result 
## without calculate again.

## Arguments:
## x = the special object returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
  m <- x$getinverse()
  if(!is.null(m)) { ## If not null show message to the user and return the cached matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## Using the R solve function to find the inverse of the matrix
  x$setinverse(m) ## caching the solution
  m ## return "solved" matrix
  
}

