## Functions to cache matrix inversion computation to make the 
## computation more efficient

## The first function does this by creating a list of functions 

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get <-function() x
	setinverse <- function(Solve) m<<-Solve
	getinverse <- function() m
	list(set=set, get=get, setinverse = setinverse,
	getinverse = getinverse)
	

}


## the second one does the inversion by first checking if the inversion
## on the given matrix

cacheSolve <- function(x, ...) {
	m<-x$getinverse()
	if(!is.null(m)) {
		message("getting cashed data")
		return(m)
	}
	data <-x$get()
	m<-Solve(data,...)
	x$setinverse(m)
	m
}
