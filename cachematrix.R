## The functions 'makeCacheMatrix' and 'cacheSolve' invert a user-defined square matrix.
## Additionally, these functions will cache matricies as they are entered (through the use 
## of lexical scoping) and recall the cached value if an identical matrix is input by
## the user. Displaying a cached value offers improved performance for large matrices as the 
## functions do not necessarily need to recalculate each time the user enters a matrix.


## The function 'makeCacheMatrix' stores a user-defined square matrix using the function 'set',
## which can be called by the user or 'cacheSolve' function by using the function 'get'.
## 'makeCacheMatrix' also uses the function 'setinverse' to calculate the inverse of the defined
## square matrix, and the inverted matrix can be called by the user or 'cacheSolve' function
## through 'getinverse'. The matrix value 'm' is cached as a global variable that can be called
## by both 'makeCacheMatrix' and 'cacheSolve'.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {    ## Stores the value of the matrix to be inverted.
                x <<- y
                m <<- NULL
        }
        get <- function() x     ## Calls the matrix to be inverted. Queried by 'cacheSolve'
        setinverse <- function(solve) m <<- solve  ## Calculates and caches the inverted matrix. Queried & updated by 'cacheSolve'
        getinverse <- function() m                 ## Calls the inverted matrix. Queried by 'cacheSolve' to determine if value exists
        list (set = set, get = get,
              setinverse = setinverse, getinverse=getinverse)
}


## The function 'cacheSolve' calls the functions 'get', 'getinverse', and 'setinverse' from
## 'makeCacheMatrix'. If the matrix 'm' from 'getinverse' exists, 'cacheSolve' returns the
## message "getting cached data" and returns the cached matrix. If the matrix has not been
## cached previously, the function will find the inverse of matrix 'm', display the matrix,
## and store the value in 'setinverse' to be cached.

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
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
