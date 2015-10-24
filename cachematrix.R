
## The two functions will permit to store the inverse of a matrix.
## Hence, you need to calculate the inverse only once and the result will be stored.

## The function makeCacheMatrix takes a matrix and return a special matrix which is list of 4 functions :
## set : set the value of the matrix
## get : get the value of the matrix
## setinv : set the inverse of the matrix
## getinv : get the inverse of the matrix

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

## The cacheSolve fonction returns the inverse of the special matrix created by the previous function.
## It first tests if the inverse has already been calculated. If so, it returns the cached value.
## Otherwise, it calculates and return the inverse of the function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
