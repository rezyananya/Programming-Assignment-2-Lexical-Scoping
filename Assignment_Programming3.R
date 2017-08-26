# As per the assignement I have created makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

MakeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# CacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}

B <- matrix(c(8,9,10,11),2,2)#Inverse here has not computed yet.
B1 <- MakeCacheMatrix(B)
cacheSolve(B1) #Inverse here has returned after computation.
cacheSolve(B1) #Inverse here has returned from cachesolve.