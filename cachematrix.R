
## The following functions allow a matrix to be created/cached as well  
## as the inverse of that matrix to be stored after its initial calculation  


## creates a cached matrix along with get/set functions for both
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL 
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## calculates the inverse or returns it from "cache" if already calculated

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
