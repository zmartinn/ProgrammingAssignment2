## The behaviour of both functions is very similar to the examples provided
## makeCacheMatrix is a function that creates a special matrix thatcan store
## its inverse as cache data
## cacheSolve computes the inverse of the matrix created by makeCacheMatrix
## and stores it as cache data. This function can also return the cached
## inverse if the matrix has not changed

## makeCacheMatrix is a function that has one argument (a matrix) and stores 
## four other functions:
## When creating a new matrix, makeCacheMatrix sets the inverse
## as NULL in order to eliminate previous cached data. 

## get() returns the original matrix
## getinv() returns the inverse matrix (if computed or manually set) or NULL
## set() allows the user to set a new matrix; in case the inverse of the 
## previous matrix were stored, it is eliminated
## setinv() allows the user to set manually the value of the inverse matrix
## and store it as cache data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix stored in makeCacheMatrix.

## The function firsts checks if the inverse has been already computed or 
## set manually by the user, if so it returns the cached inverse.

## If the inverse hasn't been already computed (or manually set) or the 
## user has set a new matrix, cacheSolve calculates the inverse and 
## stores it as cache data in the makeCacheMatrix function through the 
## setinv() function.

cacheSolve <- function(x, ...) {
	  inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
