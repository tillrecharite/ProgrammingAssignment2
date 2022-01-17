## makeCacheMatrix is a function that transforms a matrix to an "envelope" in 
## form of a list of functions as well as the matrix and possibly the cached
## inverse stored within the main environment of this list.


makeCacheMatrix <- function(x = matrix()) {
        ## initalize cache by creating empty variable for later filling
        cachedinverse <- NULL
        
        ## set new matrix as target (x), in higher environment
        ## therefore also delete previously cached inverse, in higher environm. 
        set <- function (z){
                x <<- z
                cachedinverse <<- NULL
        }
        
        ## return the matrix itself, the inverse of which should be cached
        ## not the inverse
        get <- function() {x}
        
        ## set the inversed matrix as cached value in cachedinverse
        setinverse <- function (inversedmatrix) {cachedinverse<<-inversedmatrix}
        
        ## return the cached inversed matrix
        getinverse <- function () {cachedinverse}
        
        ## compile list of functions for accessibility via "$"
        list (set = set, get = get, setinverse=setinverse, getinverse=getinverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## load cached inversed matrix
        s <-x$getinverse()
        
        ## if the inversed matrix was cached, return the cached data with a 
        ## message. If the cache is empty ('else') load the original matrix, 
        ## calculate the inversed matrix, use 'setinverse' to save it to the 
        ## cache, and print it, with a different message.
        if(!is.null(s)) {
                message("getting cached inversed matrix")
                return (s)
        }
        else {
                data<- x$get()
                s<- solve(data)
                x$setinverse(s)
                message("calculating freshly, caching and now serving:")
                return (s)
        }
}
