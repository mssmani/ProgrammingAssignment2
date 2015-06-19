## This is final version of the assigment (commit 11); please ignore all other versions
## Caching a matrix inverse helps reduce the computational effort
## This R program has a pair of functions (makeCacheMatrix, cacheSolve) to cache the inverse of a matrix

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse
## This function uses the solve function in R to compute the inverse of the matrix
## This function does the following
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of the inverse of the matrix

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## This will return a matrix that is the inverse of 'x'
        ## This will check to see  if the  inverse has already been calculated and that the matrix has not  change
        ## This  will then retrieve  the inverse from the cache
        
        m <- x$getinverse()
	      if(!is.null(m)) {
	              message("getting cached data")
	              return(m)
	      }
	      data <- x$get()
	      m <- solve(data, ...)
	      x$setsolve(m)
        m
}
