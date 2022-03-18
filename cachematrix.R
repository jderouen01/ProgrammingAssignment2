## This function deals with cached Matrices.

## This function will be storing the solutions found in cacheSolve().


## Takes input of x, where x is a matrix
## Returns a matrix x, with functions get,set,getinv,setinv.

makeCacheMatrix <- function(x = matrix()) {
      
      ## this is the cached matrix inverse. 
      ## Starts as NULL because there is no matrix inverse cached.
      inv <- NULL
      
      ## get simply returns x.
      get <- function() x 
      
      ## set caches x in a different environment
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## getinv simply returns the inverse of a matrix. Where the solution "inv" 
      ## will be found in cache or solved using cacheSolve().
      getinv <- function() inv
      
      ##setinv sets the inverse of a function to this "inv" object in another environment.
      setinv <- function(inverse) inv <<-inverse
      
      ##will return a list of matrix functions.
      list(get=get, set=set, getinv=getinv, setinv=setinv)
}

## cacheSolve finds the inverse of a given matrix
##either through pulling from the cache of already solved matrices
##or by solving the matrix directly
cacheSolve <- function(x, ...) {
      
      ##  Object "inv" is set to the function getinv() defined in makeCacheMatrix()
      inv <- x$getinv()
      
      ## Says that if the object inv is not NULL, meaning this matrix has already been cached,
      ## then it returns the inverse.
      if (!is.null(inv)) {
            message("inverse is cached!")
            return(inv)
      }
      
      ## the if the value of inv is NULL then we must solve the matrix directly.
      
      ## the get() function is called and stored in object m
      m <- x$get()
      
      ## the inverse of m is found and stored in object inv
      inv <- solve(m, ...)
      
      ## inv is then stored in the cache using setinv() function
      ## once the inverse is stored we may use getinv() later to retrieve from cache.
      x$setinv(inv)
      
      ## finally the inverse is returned
      return(inv)
}


