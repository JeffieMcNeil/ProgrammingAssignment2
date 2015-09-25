## makeCacheMatrix is a set of four functions that allow you to:
##      set the value of a matrix (set)
##      get the value of the matrix (get)
##      set the value of the inverse matrix (setinv)
##      get the value of the invserse matrix (getinv)
##  It works in conjunction with cacheSolve (below)


makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL 
    set <- function(y) {
        x <<- y  
        invmat <<- NULL
    }
    get <- function() x 
    setinv <- function(inv) invmat <<- inv
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve is a function that takes the matrix identified in makeCacheMatrix 
## and returns its inverse. First it checks to see if the inverse has previously
## been calculated and stored in makeCacheMatrix$setinv and if so, returns the
## stored value. If not, it calculates the inverse matrix, stores it and returns
## it.

cacheSolve <- function(x, ...) {
    invmat <- x$getinv()
    if(!is.null(invmat)) { 
        message("getting cached matrix")
        return(invmat)  ## returns the previously calculated inverse matrix, if
                        ## there is one stored
    }
    data <- x$get()             ## if the inverse has not yet been calculated,
    invmat <- solve(data, ...)  ## calculates it using the solve function
    x$setinv(invmat)
    invmat
}
## To use this pair of functions, assign makeCacheMatrix with a matrix as a
## parameter to a variable (e.g. bob<-makeCacheMatrix(matrix(1:4,2,2)))
## Then call cacheSolve, giving the variable as a parameter 
## (e.g. cacheSolve (bob))

## To verify the results, you can true matrix multiply (%*%) the original 
## matrix (bob$get()) by the inverse (cacheSolve(bob)). It should return 
## an identity matrix (1s on the diagonal and zeros everywhere else)
