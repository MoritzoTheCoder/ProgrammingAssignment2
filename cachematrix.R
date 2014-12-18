################ Function 1 ###########################################################
## This function takes a matrix as its input; its purpose is to wrap the input matrix in a kind of container which in addition can later store the inverse of this matrix.
## Note: This function does NOT calculate the inverse itself, but once the inverse is calculated by the funtion 'cacheSolve' below, it stores it in addition to the input matrix.
## This function superassigns the original input matrix to a variable called 'x' and superassigns a placeholder for the inverse of the matrix to variable 'm'.
## The output of this function technically is a object of type 'list'. 
## On a sublevel this output list contains the original input matrix and the inverse, after the function 'cacheSolve' has been called.

#######################################################################################
## Define function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}
## Call the function 'makeCacheMatrix', example: a 10x10 matrix with random numbers as input:
m1 <- makeCacheMatrix( matrix(rnorm(100), 10) )

################ Function 2 ###########################################################
## The purpose of this function is to actually calculate the inverse of a matrix.
## It does so by taking the object created by the call of function 'makeCacheMatrix' as input.
## It stores the inverse of the orginal matrix back into the input object.
## The consequence is:
## If this function is called a second time it won't calculate the inverse again, 
## but just retrieve its former calculation from the input object in which it has stored the inverse before.
#######################################################################################
## Define function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
## Call function, with the example object created by the call of the first function above:
cacheSolve(m1)

################### Validity Check ####################################################
#######################################################################################
## Rational: A matrix multiplied with its inverse results in a matrix with '1' in its diagonal, called the 'identity matrix'.
## Just retrieve the original matrix from the object created by calling the first function 'makeCacheMatrix'.
## and multiply it with the output produced by the second function (which should just be the inverse of the orginal matrix).
## The result should be an identity matrix.
m1$get() %*% cacheSolve(m1)

