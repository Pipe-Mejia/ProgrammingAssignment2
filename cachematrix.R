###############################################################################
################## Assignment 2 Week 3 Functions by Pipe-Mejia ################
###############################################################################

# IMPORTANT !!! The following makeCacheMatrix function builds on the makeVector
# function as described in the course week 3 assignment description web page.

# The x argument captures the data matrix which inverse may be calculated
# with the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    # Initialize inv as an empty object, which will store the inverse of x
    inv <- NULL
    # The function set stores the values of x and inv in an upper
    # environment than its own one, so those values won't be destroyed
    # outside of it. Those values will be the ones of y and NULL.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # The function get() gets the value of x from the upper environment
    get <- function() x
    # The function setinverse(inverse) stores the matrix x inverse in an upper
    # environment
    setinverse <- function(inverse) inv <<- inverse
    # The function getinverse() takes the value of the matrix inverse
    # from the upper environment
    getinverse <- function() inv
    # Create the list which stores the mentioned 4 functions, so they can be
    # called from the function cacheSolve
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# IMPORTANT !!! The following cacheSolve function builds on the cachemean
# function as described in the course week 3 assignment description web page.

cacheSolve <- function(x) {
    # Retrieve from the cache the matrix which inverse is to be calculated
    # Note that the cacheSolve only takes one argument because the matrix 
    # inversion operation is always applied in the same way
    inv <- x$getinverse()
    # Call the getinverse function to get the inverse from the upper environment
    # If the value of inv is not null, the inverse has already been calculated, 
    # and return its value as the function result
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # So, if inv is null proceed to get from the cache the value of the
    # matrix to be inverted and store it in the object data
    data <- x$get()
    # Calculate the inverse of the matrix stored in the object data, and
    # store the result in the inv object
    inv <- solve(data)
    # Call the function setinverse from makeCacheMatrix and store the
    # inverse in the cache
    x$setinverse(inv)
    # Return de value of inv as the final result of the matrix inversion
    # process
    inv
}

# AUTHOR NOTE: The following piece of code uses the makeCacheMatrix function to take a 
# 3 x 3 matrix with a previously known inverse, and the cacheSolve function to
# calculate its inverse; then it takes the result of cacheSolve and compares
# it with the inverse matrix previously known. If both results are equal
# within a very small tolerance internal rounding value, the cacheSolve has
# returned a correct inverse.
# Note the order in which the makeCacheMatrix and cacheSolve functions are
# used.
#
# z <- makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, byrow = TRUE))
# inversa <- cacheSolve(z)
# R <- matrix(c(-24,18,5,20,-15,-4,-5,4,1), nrow = 3, byrow = TRUE)
# print(inversa)
# if(max(abs(inversa-R)) < 1e-9) {print("Correct Inverse")
# } else {print("Error !!!")
# }


###############################################################################
########################### END Assignment ####################################
###############################################################################

