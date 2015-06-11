## Put comments here that give an overall description of what your
## functions do
#### Functions that cache the inverse of a matrix
####
#### Usage example:
####
#### > source('cachematrix.R')
#### > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
#### > cacheSolve(m)
#### [,1] [,2]
#### [1,]  0.5  0.0
#### [2,]  0.0  0.5

## Write a short comment describing this function
#### Create a special "matrix", which is a list containing
#### a function to
####   - set the value of the matrix
####   - get the value of the matrix
####   - set the value of the inverse matrix
####   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(m) {
        x <<- m
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solution) inv <<- solution
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix
    )
}


## Write a short comment describing this function
#### Calculate the inverse of the special "matrix" created with the above
#### function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$getmatrix()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}
