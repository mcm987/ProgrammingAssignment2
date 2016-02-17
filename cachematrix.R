## These two functions are build analog to the example "Caching the Mean of a 
## Vector" in the assignment description. The function "makeCacheMatrix" creates
## a special "matrix" object and provides a list with functions to 1) set the
## value of the matrix, 2) get the value of the matrix, 3) set the value of 
## the inverse matrix, 4) get the value of the inverse matrix. The function
## "cacheSolve" checks, if the inverse matrix of the input matrix has already
## been calculated. If this is the case, the inverse matrix is return from the
## cache. If the inverse matrix has not been calculated yet, "cacheSolve"
## calculates the inverse matrix.
 
## How does the function "makeCacheMatrix" work? When we start out NULL is
## assigned to inv (for "inverse matrix"). If we call "set", a new matrix "y" 
## is assigned to our working variable "x" and inv is set to NULL with the
## superassignment operator ("<<-"). With "get" we can get our matrix printed, 
## with "setinv" we solve for our inverse matrix and with "getinv" we display 
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## How does the function "cacheSolve" work? The inverse matrix of matrix x is
## assigned to the variable "inv" (for "inverse matrix"). If "inv" != NULL, then
## cached data is returned (by "return(inv)"). If "inv" equals NULL (because
## we just started out or because we set a new matrix using "set" in the function
## "makeCacheMatrix", then the matrix data is called into the cacheSolve function
## using x$get() and then the inverse matrix is computed and set using the
## superassignment operator.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
