## makeCacheMatrix has four subfunctions that create the matrix object, store the object, 
## save the object into cache, return the cache 

## Write a short comment describing this function

makeCacheMatrix <- function(x =  matrix(numeric(), nrow=1, ncol=1)) 
{
  set <- function(y) 
  {
    if((!is.matrix(x)) & (nrow(x)!=ncol(x))) stop("x must be a square matrix")   
    x <<- y
    m <<- NULL
  }
  get <- function() 
  {
    if((!is.matrix(x)) & (nrow(x)!=ncol(x))) stop("x must be a square matrix") 
    x
  }
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function retrieves the cache content, if null gets the matrix, 
## calculates inverse and returns the result

cacheSolve <- function(x, ...) {
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
## Return a matrix that is the inverse of 'x'
}
