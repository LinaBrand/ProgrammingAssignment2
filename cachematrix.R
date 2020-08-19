## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)                     
        i
}

# Create sample matrix
x <- matrix( c(2, 4, 3, 1, 5, 6, 7, 3, 1), # the data elements 
                nrow=3,              # number of rows 
                ncol=3,              # number of columns 
                byrow = TRUE)


det(x)
solve(x)

# Now apply cacheSolve 
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
