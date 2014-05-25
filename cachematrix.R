## makeCacheMatrix(x)
##
## Create a matrix 'x' that can cache its inverse
## 
## After created use the following functions
##
## x$set(y) : Set the value of 'x' with 'y'
## x$get() : Return the value of 'x'
## x$setinv(y) : Set the value of inverse of 'x' with 'y'
## x$getinv() : Return the value of inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve(x, ...)
##
## Solve the inverse of 'x' looking for (and returning) cached data 

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
