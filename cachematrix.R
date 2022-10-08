## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set the values of m and y to NULL 
    m <- NULL 
    y <- NULL 
  #set the value of the matrix
  setmatrix <- function(y) { 
    ## cache the inputted matrix for later checking
    ## whether it has changed
    x <<- y 
    ## reset the value of m (the matrix inverse)
    m <<- NULL 
  }
  # Parts removed
  list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x=matrix(), ...) {
  # check for inverse
  m <- x$getinverse() 
  # check if cacheSolve has run, varify no changes
  if(!is.null(m)){ 
    if(x$setmatrix() == x$getmatrix()) 
      
      return(m)
    }
    # if it has not... 
    # get the value of the input matrix 
    y <- x$getmatrix() 
    # cache the matrix
    x$setmatrix(y)
    # compute the value of the inverse
    m <- solve(y, ...) 
    # cache the inverse
    x$setinverse(m) 
    #return the value
  m
}
 
    
