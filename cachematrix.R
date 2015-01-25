## The makeCacheMatrix function stores the matrix and creates four functions in a 
## unique environment. When these functions are called by the cacheSolve function
## they check for prior calculation, get the matrix and the inverse (if calculated) 
## and store the calculated inverse matrix


## The makeCacheMatrix function takes the matrix and stores it to a unique 
## enviorment. It also creates and stores the functions that act on that matrix
## These functions are:-
## set - This function allows the original matrix to be changed. Passing a new 
##    matrix to the enviroment means that the inverse is no longer correct and 
##    therefore needs to be cleared (m <<- NULL). This is not called by cacheSolve
## get - returns the matrix (x) passsed to makeCacheMatrix
## setsolve - is passed the inverse matrix and stores it in m
## getsolve - returns the inverse matix (m). Returns Null if has not calculated


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


## cacheSolve - Return a matrix that is the inverse of 'x'
## Using the matrix stored in the unique enviroment by makeCacheMatrix
## cacheSolve uses the functions in makeCacheMatrix to return the inverse
## First it uses getsolve to see if the inverse has been previously created
##    if getsolve does not return null then the inverse is returned

## If the inverse has not been created then the original matrix is retrieved
##    using the get function and stored in data 
##    The solve function is called to inverse the matrix (using data)
##    the result is stored in m. Setsolve is then called to store the matrix so
##    that there is no need for recalculation if called again

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