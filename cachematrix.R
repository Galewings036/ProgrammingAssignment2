#This function creates a list containing functions to set/get the value of the matrix, and set/get the value of its inverse.
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m<<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#This function returns the inverse of the matrix.
cacheSolve <- function(x, ...){
        #Check if the inverse of the current matrix is calculated. If so, return the cached result.
        m<- x$getinverse()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
