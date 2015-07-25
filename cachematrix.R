## These functions create a matrix and cache its inverse,  
## re-calculating -caching whenever the special matrix changes

## makeCacheMatrix creates a special matrix which:
## i) sets the elements of the matrix
## ii) gets the inverse of the matrix
## iii) sets the value of the inverse
## iv) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following caches the inverse of the special matrix:
## i) Checks to see if inverse has already been calculated/cached then retrieves
## ii) Calculates and caches inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("retrieving cached data..")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}