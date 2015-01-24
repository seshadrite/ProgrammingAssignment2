## This program is an example for illustrating the caching feature 
## There are two functions called makeCacheMatrix & cacheSolve

##  makeCacheMatrix function accepts Input Matrix to be inversed and 
##  encapsulates with an object that contains both input matrix and
## its inverse cache 

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  cacheSolve function extracts the Input Matrix from  makeCacheMatrix
##  object, computes the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
