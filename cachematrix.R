######################################################################
# cachematrix.R - Programming Assignment 2: Lexical Scoping
#
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly. 
# The assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# These two functions are makeCacheMatrix and cacheSolve which are further 
# documented below.
#
# A testing function called test_cacheSolve is also provided for validation.
#
# Assumptions: 
# 1. For this assignment, assume that the matrix supplied is always invertible.
#
# Sample usage: 
#  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#  h8 <- hilbert(8)            # 8 X 8 invertible matrix
#  cm <- makeCacheMatrix(h8)   # cm = "Cache Matrix" object
#  cs <- cacheSolve(cm)        # cs = inverted 8 X 8 matrix
# 
#  h4 <- hilbert(4)            # 4 X 4 invertible matrix
#  cm$set(h4)                  # set function updates the internal matrix and clears the cache
#  cs <- cacheSolve(cm)        # cs = inverted 4 X 4 matrix
#
#
### makeCacheMatrix <- function(m = matrix())
#
# This function creates a special "Cache Matrix" object that can cache its inverse.
#
# It takes as input an invertible matrix.  It creates the needed data structures 
# and provides the following functions:
#   set <- function(y) - takes as input an invertible matrix.
#       o It updates the stored matrix and clears the cached inverse.
#   get function() - takes not parameters and returns the stored matrix.
#   setinverse <- function(inverse) - takes the inverse of the matrix as input.
#       o It stored the inverted matrix in the cache for later recall.
#   getinverse <- function() - takes no parameters.
#       o It returns the cached inverted matrix or NULL
#
### cacheSolve <- function(x, ...)
#
# This function takes as input a "Cache Matrix" object and returns the inverse
# of the associated internal invertible matrix.
#
# This function uses the sub-functions of the "Cache Matrix" object to provide 
# a cached response, avoiding the inversion computations on repeated calls. 
#
### test_cacheSolve <- function()
#
# This function takes no parameters and exercises the above two functions
# in order to test them.
#
# Two sets of model matrices and their associated expected results are 
# used to validate the function.
#
######################################################################


# makeCacheMatrix - creates the special "Cache Matrix" object used by the cacheSolve function.
#
# This function creates the data structure and functions, and stores the matrix 
# that the solve() function will later be run on, by cacheSolve.
#
# The set function allows for updating the stored matrix and clearing the cache.
#
# The actual solve() calculation is carried out by the cacheSolve function, if the 
# cached "solve" (mareix inversion) is null.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# cacheSolve - utilizes the "Cache Matrix" object created by the makeCacheMatrix function
# to provide a caching matrix inversion function that avoids the inversion overhead on 
# repeated calls.
#
# If the cache is NULL the inversion is calculated via the solve() function and the 
# result is stored in the cache and returned.
#
# If the cache is holding a result, that result is returned from the cache, avoiding 
# the inversion overhead.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# test_cacheSolve - tests the functions above
#
# Two sets of model matrices and their associated expected results are 
# used to validate the function.

test_cacheSolve <- function() {

	# taken from sample code om ?solve documentation
	#
	# Create solvable matrices to test with

	hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

	# 8 X 8 
	h8 <- hilbert(8)
	sh8 <- solve(h8)
	print("Model 8 x 8 and its inverse")
	print(h8)
	print(sh8)

	# 4 X 4 
	h4 <- hilbert(4)
	sh4 <- solve(h4)
	print("Model 4 x 4 and its inverse")
	print(h4)
	print(sh4)

	# Test 1 
	print("########  Test 1 (8X8 via initialization)  ########")

	test_cm <- makeCacheMatrix(h8)

	print("cacheSolve(test_cm) -- #1")
	test_csa <- cacheSolve(test_cm)

	print("cacheSolve(test_cm) -- #2")
	test_csb <- cacheSolve(test_cm)

	print("cacheSolve(test_cm) -- #3")
	test_csc <- cacheSolve(test_cm)

	print("Are the 3 inverted matrices identical?")
	print(all(identical(test_csa,test_csb),identical(test_csb,test_csc)))
	print("Cache processed result:")
	print(test_csa)
	print("Model expected result")
	print(sh8)
	print("Does the cached processed result match the model?")
	print(identical(test_csa,sh8))

	# Test 2 
	print("########  Test 2 (4X4 via set)  ########")

	test_cm$set(h4)  # use the set function to change to a 4X4 matrix

	print("cacheSolve(test_cm) -- #1")
	test_csa <- cacheSolve(test_cm)

	print("cacheSolve(test_cm) -- #2")
	test_csb <- cacheSolve(test_cm)

	print("cacheSolve(test_cm) -- #3")
	test_csc <- cacheSolve(test_cm)

	print("Are the 3 inverted matrices identical?")
	print(all(identical(test_csa,test_csb),identical(test_csb,test_csc)))
	print("Cache processed result:")
	print(test_csa)
	print("Model expected result")
	print(sh4)
	print("Does the cached processed result match the model?")
	print(identical(test_csa,sh4))

}

