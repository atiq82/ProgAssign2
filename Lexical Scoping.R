## The file defines two functions: makeCacheMatrix and cacheSolve
## 
## makeCacheMatrix defines a list for caching the 
##  matrix as well as its cached inverse and the functions
##  used to maintain the cached matrices.
## 
## cacheSolve utilizes the list created by makeCacheMatrix
##  to invert the cached matrix.  
##  If the inverted matrix is already cached, then the cached
##  matrix is returned.  If not, then the cached original, 
##  non-inverted, matrix is inverted, cached and the inverted
##  matrix returned.

#MakeCacheMatrix#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#CacheSolve#
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


#Example#
a <- diag(5,3)
a

CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)

b <- diag(2,6)
b

CachedMarix <- makeCacheMatrix(b)
cacheSolve(CachedMarix)

cacheSolve(CachedMarix)   #getting cached data
