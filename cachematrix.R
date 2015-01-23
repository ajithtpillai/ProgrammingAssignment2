## Put comments here that give an overall description of what your
## functions do

## caches matrix and its inverse    

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    get <- function() x

    ##set and get inverse of matrix
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    
    ##return a vector of list of functions possible on matrix
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of a matrix from cache, or after computing it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)){
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    ## get the inverse of given matrix
    im <- solve(data,...)
    x$setinverse(im)
    im
}
