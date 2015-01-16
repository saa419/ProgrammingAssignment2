## A pair of functions that cache the inverse of a matrix
## Comments and code adapted from "Making Sense of
## Assignment 2" thread by Bill Hilton
## https://class.coursera.org/rprog-009/forum/thread?thread_id=457

## Creates a special matrix object that can cache its inverse
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {	## input m will be a matrix


	i <- NULL	##  i will be our 'inverse' and it's reset to NULL every 
			##  time makeCacheMatrix is called

	set <- function(matrix) {	## takes an input matrix
		m <<- matrix	## saves the input matrix
		i <<- NULL	## resets the inverse to NULL, basically what happens when a new object is generated.
    }

					##  note these next three functions are defined but not run when makeVector is called.
					##  instead, they will be used by cacheSolve() to get values for x or for
					##  i (inverse) and for setting the inverse.  These are usually called object 'methods'

	get <- function() { m }	## this function returns the value of the original matrix
    
	setinverse <- function(inverse)  { i <<- inverse }
					## this is called by cachemean() during the first cacheSolve()
					## access and it will store the value using superassignment
        
	getinverse <- function() { i }
					## this will return the cached value to cacheSolve() on
					##  subsequent accesses


	list( set = set, get = get,	##  OK, this is accessed each time makeVector() is called,       
	setinverse = setinverse,	##   that is, each time we make a new object.  This is a list of 
	getinverse = getinverse)	##  the internal functions ('methods') so a calling function
						##   knows how to access those methods.                        
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {	## the input x is an object created by makeVector
    m <- x$getinverse()			## accesses the object 'x' (a matrix) and gets the inverse of it

    ## Just return the inverse if its already set
    if(!is.null(m)) {	## if inverse was already cached (not NULL) ...
            message("getting cached data")		# ... send this message to the console
            return(m)	## ... and return the inverse ... "return" ends 
                        	##   the function cacheSolve(), note
    }

    data <- x$get()	## we reach this code only if x$getinverse() returned NULL

    m <- solve(data)	## if i was NULL then we have to calculate the inverse with solve()

    x$setinverse(m)	# store the calculated inverse matrix in x (see setinverse() in makeVector)

    message("data not cached. computing the inverse")    # ... send this message to the console
    
    m	# return the matrix to the code that called this function
}
