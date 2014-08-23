## In general terms, the two functions makeCacheMatrix and cacheSolve 
## allow you to calculate the inverse of a matrix and store the outcome.
## If you have to calculate the inverse of the same matrix repeatedly,
## caching the outcome and fetching it when needed is far more efficient
## then calculating it over and over.

## The first function, makeCacheMatrix, creates a list of four functions 
## to set and get the value of a matrix, and to set and get the inverse 
## of this matrix. For a new matrix, it stores the input and makes it
## available to the second function, cacheSolve. When the cacheSolve 
## function is subsequently run, makeCacheMatrix stores the outcome
## of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## The second function, cacheSolve, accesses the outcomes of the first
## function. It first checks if the inverse has already been calculated.
## If so, it will fetch the outcome and return it. If not, it will retrieve
## the matrix, calculate its inverse, store the outcome in makeCacheMatrix, 
## and finally return the outcome.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
