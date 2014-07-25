

## This function creates a special "matrix" object that can cache its inverse.
## The function creates a list of functions that are used to handle the cache of the matrix 
makeCacheMatrix <- function(x = matrix()) {
    #initialize the object
    i <- NULL
    #set the matrix itself
    set <- function(y) {
        x <<- y
        ##It first checks to see if the inverse has already been calculated
        i <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    #sets the value of the inverse. It first checks to see if the inverse has already been calculated
    setinverse <- function(inverse) i <<- inverse
    #get the value of the inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The following function calculates the mean of the special "matrix" created with the
## makeCacheMatrix function. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ##first retrieve the inverse
        i <- x$getinverse()
        ##is the inverse not null?
        if(!is.null(i)) {
          message("getting cached data")
          ##since the (pre-calculated) inverse is not null return it
          return(i)
        }
        #the following code is executed if and only if the inverse has not already been calculated
        #get the original matrix
        data <- x$get()
        ##solve it!
        i <- solve(data, ...)
        ##remember to set the inverse so the next time it won't be necessary to calculate it
        x$setinverse(i)
        i
        
}
