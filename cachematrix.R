## The makeCacheMatrix function stores the matrix and creates four functions in a 
## unique environment. When these functions are called by the cacheSolve function
## they check for prior calculation, get the matrix and the inverse (if calculated) 
## and store the calculated inverse matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x  
      setsolve <- function(solve) m <<- solve 
      getsolve <- function() m  
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve) 
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}