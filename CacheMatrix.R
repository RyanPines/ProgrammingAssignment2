## The below functions (makeCacheMatrix and cacheSolve) caches the inverse of a SQUARE matrix.

## The makeCacheMatrix functions creates a list that contains a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inverseCache <- NULL
	## We create and initialize our cache object called 'inverseCache' to null

	set <- function(y) {
		x <<- y
		inverseCache <<- NULL
	}
	## 'x' and 'inverseCache' are free variables that exist in a different environment 
	## because they are not defined in the function header nor do they exist in 
	## the function body. Given that 'x' and 'inverseCache' exist in a different
	## environemnt, we use the '<<--' symbol to assign values to them.
	## 'set' is a function that will set the value of the matrix. 

	get <- function() {x}
	## 'get' is a function that will get the value of the matrix
	
	setInverse <- function(inverse) {
 		inverseCache <<- inverse
		}
	## 'inverseCache' is a free variable that exists in a different environment
	## because it is not defined in the function header nor does it exist in the
	## function body. Given that 'inverseCache' exists in a different environment,
	## we will use the '<<--' symbol to assign values to it.
	## 'setInverse' is a function that will set the value of the inverse matrix.

	getInverse <- function() {inverseCache}
	## 'getInverse' is a function that will get the value of the inverse matrix.

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The cacheSolve function computes the inverse of the square matrix that is returned from the above makeCacheMatrix function
## If the inverse has already been calculated, then the function will retrive the inverse from the cache.
## If not, then the function will calculate the inverse using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverseCache <- x$getInverse()
	## We assign the inverse matrix to the 'inverseCache' object

	if(!is.null(inverseCache)) {
		message("getting cached data")
		return(inverseCache)
	## if the cached inverse matrix already exists (is not null)
	## then return the cached inverse matrix
	
	} else {
		matrix <- x$get()
		## creates a matrix

		inverseCache <- solve(matrix, ...)
		## the solve functions returns the inverse of a SQUARE matrix

		x$setInverse(inverseCache)
		## sets the value of 'inverseCache', the inverse of matrix

		inverseCache
		## returns the value of 'inverseCache', the inverted martix
	}

}