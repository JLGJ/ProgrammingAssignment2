## Copied the makeVector and cachemean functions provided in class and made adjustments
## as noted in the code below
## Objective: Create a pair of functions that can cache the inverse of a matrix

## this function creates a special "matrix" object that can cache its inverse.
## Began by copying the core code provided in class for makeVector
## Modifications are noted in the code
makeCacheMatrix <- function(x = matrix()) {
  ## Previous line - changed x from a numeric to a matrix
m <- NULL
set <- function(y) {
  x <<- y
  m <<-NULL
}
get <- function() x

## Modification --> "mean" becomes "solve" 
## Minor modification --> changed the names of the setmean and getmean to reflect inversion
setinverst <- function(solve) m <<- solve
getinverst <- function() m
list(set = set, get = get, setinverst = setinverst, getinverst = getinverst)
}


## This function calculated the inverse of the special vector created with "makecacheMatrix"
## Again, I began by copying the class code and modifying as noted.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverst()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##Changed "mean" function to "solve" function
  m <- solve(data, ...)
  x$setinverst(m)
  m
}
