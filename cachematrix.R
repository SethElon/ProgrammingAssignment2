## These two functions create a special "matrix" capable of cacheing
## the matrix's inverse so that if it has already been calculated then
## the that calculation does not need to be repeated. These functions
## assume that th provided matrix is invertable.

## makeCacheMatrix is function that stores a list of four functions designed
## to cache a matrix's inverse. The for functions are set, get, setinverse
## and getinverse. This makes a special "matrix" object that can caches its
## inversion

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        ## the set function replaces the existing x matrix with a new matrix y
        ## and clears the inverse cache values (i)
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        ## the get function just returns the existing matrix x
        get<-function()x
        ## the setinverse function sets a value for the cached matrix inverse
        ## note that the matrix set here does not have to actually be the inverse 
        setinverse<-function(inverse)i<<-inverse
        ## the getinverse functon returns the cached value of the matrix inverse
        getinverse<-function()i
        ## the list function call here ensures that when an object is assigned to
        ## makeCacheMatrix that all for functions are in the object
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse matrix of the "special" matrix returned in
## the above function (makeCacheMatrix). If the inversion has already been solved
## then this function just retrieves it from the cache. This function always
## assumes that the matrix is invertable. This function should be called with an 
## object that is the makeCacheMatrix function (so the special "matrix").

cacheSolve <- function(x, ...) {
        ## retrieve the cached value of the inverse matrix
        i <- x$getinverse()
        ## check if there is a non-null value in the cached inverse matrix
        if(!is.null(i)) {
                ##if there is an existing cached inverse matrix then return it
                message("getting cached inverse matrix")
                return(i)
        }
        ## if a cached value was not found then proceed to calculate the
        ## matrix 'x's inverse
        data <- x$get() ##get the matrix 'x'
        i <- solve(data, ...) ##calculate the inverse matrix of 'x'
        x$setinverse(i) ##cache the inverse matrix
        i
        ## Return a matrix that is the inverse of 'x'
}