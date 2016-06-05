## The function makeCacheMatrix() creates a special "matrix" object
## that can cache its inverse.

## The function cacheSolve() computes the inverse of the special
## "matrix" returned by makeCacheMatrix(). If the inverse has already
## been calculated, this function retrieves the inverse from the cache.

## makeCacheMatrix() :- This function receives as input a matrix and
## returns a special "matrix" object which is a list of functions:-
## 1. set()        - sets the value of a matrix
## 2. get()        - retrieves the value of a matrix
## 3. setinverse() - sets the value of the inverse of a matrix in cache
## 4. getinverse() - retrieves the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve()  :- This function receives as input the special "matrix" 
## object created by the function makeCacheMatrix() and computes the 
## inverse of the matrix. 
##
## It first looks in cache to check if the inverse has already been 
## created (if the matrix has not changed). 
## If so, it skips the computation and retrieves the value from cache.
## If not, it computes the inverse of the matrix and stores it in cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
