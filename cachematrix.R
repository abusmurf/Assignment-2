## Put comments here that give an overall description of what your
## functions do

## Function takes a matrix and returns a special vector containing a number of functions.
## In the set function the  <<- operator searches the parent environments for an existing
## definition of x. If one is found it's value is changed to y, otherwise assignment is made in the global environment.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL					# m is a placeholder for a future value.
	set<-function(y){		# function sets the vector x to a new vector y
		x<<-y				# and resets the mean m to NULL.
		m<<-NULL
	}
	get<-function() x
	setMatrix<-function(solve) m<<-solve								# solve() is a built-in function that returns the inverse of a square matrix.
	getMatrix<-function() m
	list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)		#returns the 'special vector' containing all the functions just defined.
	
}


## Function calculates the inverse matrix of the special vector created by makeCacheMatrix.
## It checks first whether it has already been calculated in which case it gets the inverse from cache.
## Otherwise it calculates the inverse through the function setMatrix.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
		m<-x$getMatrix()
		if(!is.null(m)){
			message("getting cached data")
			return(m)
		}
		data<-x$get()
		m<-solve(data,...)
		x$setMatrix(m)
		m
}

#main
a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))
b<-cacheSolve(a)
print(b)
