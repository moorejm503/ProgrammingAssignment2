## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {    ## Stores the value of the matrix to be inverted.
                x <<- y
                m <<- NULL
        }
        get <- function() x     ## Calls the matrix to be inverted. Queried by 'cacheSolve'
        setinverse <- function(solve) m <<- solve  ## Calculates the inverted matrix. Queried & updated by 'cacheSolve'
        getinverse <- function() m                 ## Calls the inverted matrix. Queried by 'cacheSolve' to determine if value exists
        list (set = set, get = get,
              setinverse = setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     ## Set 'm' to the value of 'getinverse' from 'makeCacheMatrix'
        if(!is.null(m)) {       ## If value of 'getinverse' exists, display message "getting cached data"
                message("getting cached data")
                return(m)       ## Display value of 'getinverse'
        }
        data <- x$get()         
        m <- solve(data, ...)   ## Find the inverse of matrix 'm'
        x$setinverse(m)         ## Store matrix 'm' in 'setinverse' to detect cached data if matrix values are reused
        m                       ## Display the inverted matrix 'm'
}
