## These two functions shows the usage of superassignment and 
## how the value of an object can be maintained.


## makeCacheMatrix creates an object that can save a matrix and its inverse internally.  
## thru using the get/set methods, the internal matrix and its inverse can be reset or 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve operates on an object of 'makeCacheMatrix' type. 
## If the inverse of the object has been cached, it will return the cached data.
## Otherwise, it will calculate the inverse of the object of 'makeCacheMatrix' type,
## and save the inverse value in the object of 'makeCacheMatrix' type. 

cacheSolve <- function(m, ...) {
        i <- m$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        i
}
