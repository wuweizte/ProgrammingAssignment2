## cache the inverse of a matrix to lessen time-consuming computations


## creates a special "matrix" object that can cache its inverse, which can be 
## visited by calling function getInverseMatrix()

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
        getInverseMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
        
}
