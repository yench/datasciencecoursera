##These two functions create a special matrix and compute the inverse of it.

## This function creates a special "matrix" that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	s=NULL
	set=function(y){
		x<<-y
		s<<-NULL
	}
	get=function() x
	setsolve=function(solve) s<<-solve
	getsolve=function() s
	list(set=set,get=get,
		setsolve=setsolve,
		getsolve=getsolve)

}



## This function computes or retrieves from the cache the inverse of the 
## special "matrix" returned by makeCachedMatrix

cacheSolve <- function(x, ...) {
	s=x$getsolve()
	if(!is.null(s)){
		messages("getting cashed data")
		return(s)
	}
	data=x$get()
	s=solve(data,...)
	x$setsolve(s)	
	s
}