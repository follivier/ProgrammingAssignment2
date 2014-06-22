## These 2 functions work together to get the inverse of a matrix, 
## without having to compute it again if it's been computed already


## makeCacheMatrix: manages the cache for the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # set affects the matrix to use
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      # get returns the matrix set by "set"
      get <- function() x
      # setsolve is used to cache an already computed inverse
      setsolve <- function(solve) m <<- solve
      # getsolve returns the cached data (or NULL is it's not cached)
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## it first checks if the inverse is already in the cache
## if so, it will output the cache instead of computing the inverse again

cacheSolve <- function(x, ...) {
      
      # checks if the data is in the cache
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # if the data is not in the cache, the inverse if computed and
      # then pushed in the cache
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
