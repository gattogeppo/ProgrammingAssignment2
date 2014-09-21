## A function to create a list with 4 methods to
## set/get value and inverse values from matrix

makeCacheMatrix <- function(x = matrix()) {

        inve <- NULL
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inve <<- inverse
        getinverse <- function() inve
        list(
                set=set, 
                get=get, 
                setinverse=setinverse, 
                getinverse=getinverse
        )
}


## This function returns the inverse of the matrix

cacheSolve <- function(x = matrix(), ...) {

        inve <- x$getinverse()
        
        #Data exist in cache
        if(!is.null(inve)) {
                return(inve)
        }
        
        data <- x$get()
        inve <- solve(data)
        x$setinverse(inve)
        inve
}
