## Programming Assignment 2: CacheMatrix
## This function expedites the process of inversing matrices, by storing a previously 
## inversed matrix in a cache. This way, one only needs to compute the inverse when
## confronted with a new matrix. 

## The makeCacheMatrix function builds a set of functions and returns them as a list so that
## they are accessible to the parent function. Basically, it is a mechanism for 
## storing objects and functions, and does not actually execute the solve() function. 

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       set <- function(y) {
                 x <<- y
                 i <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) i <<- inverse
       getinverse <- function() i
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function is the part that actually solves for the inverse of the
## matrix, IF that matrix has not already been inverted.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
          if(!is.null(i)) { 
                    message("getting cached inverse")
                    return(i)
          }
          data <- x$get()
          i <- solve(data)
          x$setinverse(i)
          i
}
