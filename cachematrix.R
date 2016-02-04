## This function calculates the inverse of a matrix and caches it. It will 
## return the cached value if one has already been calculated.

## makeCacheMatrix function will create a matrix object which can be cached. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function compute the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## then the cached object is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
