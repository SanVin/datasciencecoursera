makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(z) 
  {
    m<<-z
  }
  getinv <- function() m
  
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve() function takes the vector returned from the first function as arguments.If the inverse of the matrix already exists, it gets it from the cache object and returns it. If it doesn't exist,inverse is calculated,stored and returned 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
