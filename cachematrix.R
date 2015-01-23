## caches the inverse of a matrix    

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


## returns the inverse of a matrix  'x' from cache, 
## or after computing it the first time

cacheSolve <- function(x, ...) {  
    
    ## check if inverse was computed earlier
    im <- x$getinverse()
    if(!is.null(im)){
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    
    ## get the inverse of given matrix
    im <- solve(data,...)
    
    ##save the inverse
    x$setinverse(im)
    im
}
