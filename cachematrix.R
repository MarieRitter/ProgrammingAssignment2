## These functions create an object in the cache that is the inverse of a matrix x
## Useful especially for large matrices. Saves computation time.
## After the inverse matrix is stored in the cache it can be retrieved with the 2nd function.

## This function computes the inverse matrix of matrix x and stores it in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # create object that will contain inverse
        set <- function(y) {
                x <<- y
                inverse <<- NULL
                
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve # actual computation
        getinverse <- function() inverse # print inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # define list of functions

}

## This function checks if the inverse is already stored in the cache and - in this case - retrieves it.
## If not, it computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() # open funtion in function list
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        } # check if inverse is already cached
        data <- x$get() # else get the data
        inverse <- solve(data, ...) # compute inverse
        x$setinverse(inverse) # save inverse
        return(inverse) # print
}