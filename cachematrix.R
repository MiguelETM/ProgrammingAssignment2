## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creates a special object that contains the matrix and its inverse. 
##This function also contains a set of function that let cacheSolve modify this special object.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inverse <<-inverseMatrix
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function
##cacheSolve function returns the inverse of a matrix. If the inverse was not computed before,
##cacheSolve function compute and store it for future uses. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setInverse(inverse)
        inverse
}

