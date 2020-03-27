## Function to implement caching of the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {i <<- inverse}
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of a given matrix. 
## If the inverse of it has already been calculated it retrieves it using  makeCacheMatrix
cacheSolve <- function(x, ...) {
      
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setinverse(m)
  
  m
}

