## These functions allow us to create matrix objects that cache their inverse and 
## return the cached inverse. These matrices improve the performance of 
## calculations where reversal matrix is done many items. 

## A special "matrix" object that can hold cache of its inverse
makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    
    set <- function(y) {
            x <<- y
            r <<- NULL
    }
    get <- function() x
    
    setreverse <- function(reverse) r <<- reverse
    getreverse <- function() r
    
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
}

## Function that retrieves the cached instance of reverse matrix. 
## It creates and cahces a reverse matrix instance it if it doesn't exist in the cache. 
cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    r <- x$getreverse()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    
    data <- x$get()
    r <- solve(data)
    x$setreverse(r)
    
    message("cached reverse matrix")
    r    
}
