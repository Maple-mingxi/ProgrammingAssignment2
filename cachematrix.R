##  Below are two functions that are used to create a special object that stores a matrix and cache's its inversion.

## The first function, makeVector creates a special "matrix", which is really a list containing a function to

##set the matrix
##get the matrix
##set the inversion of the matrix
##get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<- y
                i<<- NULL
        }
        get<- function() x
        setinversion<- function(inversion) i<<- inversion
        getinversion<- function() i
        list(set=set, get=get, setinversion=setinversion, getinversion= getinversion)

}


## The following function creat the inversion of the special "matrix" created with the above function. However, it first checks to see if the inversion has already been created. If so, it gets the inversion from the cache and skips the computation. Otherwise, it creat the inversion of the data and sets the inversion of the matrix in the cache via the setinversion function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinversion
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<- x$get()
        i<- solve(data, ...)
        x$getinversion(i)
        i
}
