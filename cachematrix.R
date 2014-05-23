## The cacheSolve function provide the ablity to calculate the inverse matrix and to improve the performance there is a inner cache. however we don't provide auto clear or LRU mechanism, so there might be a OOM risk


## makeCacheMatrix function is a value holder which has several attrubite method like set/get

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setResolve <- function(solve) m <<- solve
    getResolve <- function() m
    list(set = set, get = get,
        setResolve = setResolve,
        getResolve = getResolve)
}




## cacheSolve first try to get inverse matrix by  looking up the cache, when the cache miss, it will do the real calculate and refill the cache for next time use
cacheSolve <- function(x, ...) {

	m <- x$getResolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
    data <- x$get()
    m <- solve(data)
	x$setResolve(m)
	m
    ## Return a matrix that is the inverse of 'x'

}
