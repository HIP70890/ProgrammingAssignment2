## This small example will demonstrate the use of lexiacal scoping, by hosting a matrix and its
## inverse in a special object.

## This will create a cached matrix object, which will hold a specific i_var variable which
## is able to hold an inversed matrix. Specific member functions allow set/get the matrix, as well
## as updating the cached object. Re-setting the matrix object, will invalidate the cache

makeCacheMatrix <- function(x = matrix()) {
  i_var <- NULL
  
  ## Set the matrix object again with some new matrix
  ## This will re-use the cached matrix object, but will take care the cache is 'cleared'
  set <- function(y) {
    x <<- y
    i_var <<- NULL
  }
  
  ## Get the matrix object provided during the initialization
  get <- function() x
  
  ## Set the i_var varibale in the cached matrix object
  ## by using the super assignment operator
  setinverse <- function(inv) i_var <<- inv
  
  ## Provides the inverse of the inverse matrix held in varible i_var
  ## which is held in the environment of the cached matrix object
  getinverse <- function() i_var
  
  ## Assign a list of properties to the created object, which 
  ## later will allow accessing the created anonymous functions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i_var <- x$getinverse()
  
  ## Check whether i_var is set, which will mean that a valid cache object is in the cached matrix object
  if (!is.null(i_var)) {
    message("getting cached data")
    
    ## return with the cached object, no further processing
    return(i_var)
  }
  
  ## This will be called if no cached object exists so far
  ## Get the matrix object
  data <- x$get()
  ## Create the inverse
  i_var <- solve(data, ...)
  ## Assigne inverse matrix object to cached matrix object  
  x$setinverse(i_var)
  ## Return inverse object
  i_var
}
