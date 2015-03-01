## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
##
## The functions contained in this file should be used
## to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
      invertedMatrix <- NULL
      
      ### set the value of the original matrix
      set <- function(y) {
            matrix <<- y
            invertedMatrix <<- NULL
      }
      
      ### get the value of the original matrix
      get <- function() matrix
      
      ### 3. set the value of the inverted matrix
      invertedSet <- function(solve) invertedMatrix <<- solve
      
      ### 4. get the value of the inverted matrix
      invertedGet <- function() invertedMatrix
      
      # return our list
      list(set = set,
           get = get,
           invertedSet = invertedSet,
           invertedGet = invertedGet)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Check in cache to see if the passed matrix is there
      matrix <- x$invertedGet()
      
      ## If it was in the cache, return the cached matrix
      if(!is.null(matrix)) {
            return(matrix)
      }
      
      ## If it wasn't in the cache, we need to put it there.
      
      ## get the original matrix ...
      data <- x$get()
      
      ## ... invert it ...
      matrix <- solve(data, ...)
      
      ## ... put it in the cache ...
      x$invertedSet(matrix)
      
      ## .. and finaly return it
      matrix
}
