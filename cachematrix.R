## This R script is about somewhat efficient matrix inversion, based on "caching".  
## There are 2 functions that help achive this 
##
## One is "makeCacheMatrix" helps in capturing the matrix to invert,
## but more importantly to "cache" the inverse once computed should it be needed later 
## 
## The other is to actually comnpute the inverse. 
## The trick here is that if the inverse was calculated earlier, then no need to recalculate 
## Just fetch from 'cache' and thus to avoid possibly expensive inversion 


## makeCacheMatrix <- function(x = matrix())
## This function creates a special "matrix" object that can cache its inverse.
## The object can hold both the matrix that needs to be inverted, and it's inverse 
## To achive so, it has utility methods, like in a 'Class' in object-oriented-prorgamming...
# ... to set and get both the matrix and the inverse 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve <- function(x, ...)
## This is the workhorse function to actually invert the matric and return it 
## It takes as argument, not a standard matrix object, but the 'special matrix' object ... 
## ... that is returned by "makeCacheMatrix" above 
## It then checks if the inverse exists - from previous computation of course... 
## ... else computes one by calling "solve". It then stores (or caches) the inverese ... 
## ... so that if needed again in future, it need not be re-computed, thus saving ...
## ... a potentially expensive operation, esp if the matrix is large

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
