##Below are two functions that cache the inverse of a matrix

##create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x   #get the value of the matrix
        setInverse <- function(inverse) m <<- inverse  #set the value of the inverse matrix
        getInverse <- function() m           #get the value of the inverse matrix
        list(set = set, get = get,...=       #return the list
             setInverse = setInverse,
             getInverse = getInverse)
}


##compute the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
        m <- x$getInverse()     #query the x vector's cache
        if(!is.null(m)) {        #if there is a cache
                message("getting cached data")
                return(m)        # return the cache
        }
        data <- x$get()          #if there's no cache
        m <- solve(data, ...)     #compute cache
        x$setInverse(m)          #save the result back to x's cache
        m                        #return the result
}
