## Coursera, R Programming Course, July, 2020

## This function is like a class it has the get and set classic fuctions
## that allow set and get the value of matrix

## Arguments: 
## x = The matrix which inverse to be calculated

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
