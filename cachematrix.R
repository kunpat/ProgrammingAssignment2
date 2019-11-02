## The makeCacheMatrix has list of functions that can be called to store (in cache) original
## data matrix and inverted data matrix. If inverted data matrix exists in cache, you can retrive 
##it  using getInverse...if not then you can calculate it and pass it to setInverse function and
## it will store it in cache. 


## this function creates a list of functions that can be called to store and retrive
## cached matrix/inversed matrix data. 

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y) { #y is the dataMatrix to be inverted or original data matrix
                x <<- y #cache the original data matrix into x
                inverseM <<- NULL #since we don't have the inverse yet set this to NULL
        }
        get <- function() x #returns the Matrix to be inverted...our stored original cached  data matrix
        setInverse <- function(IM) inverseM <<- IM # receives the inverted matrix into var IM and sets the stored cache var
        getInverse <- function() inverseM #returns the cached inverted matrix when called
        list(set = set, get = get, setInverse = setInverse, getInverse=getInverse) #list of functions

}


## this function receives the x vector which is a list of all the functions. These functions can be called to
## store and read matrix/invertedmatrix in cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. 'x' is actually list of functions to be called...
        ## one of the function in x list is passed the original matrix. That function stores the data in cache. 
        
        inverseMatrix <- x$getInverse() #check if inverseMatrix was already calculated
        if(!is.null(inverseMatrix)) { #if inverseMatrix is cached, then return this matrix
                message("getting cached data")
                return(inverseMatrix) #return matrix
        }
        dataMatrix <- x$get()   # if inverse matrix is not calculated then call the get matrix to be inverted
        invertedMatrix <- solve(dataMatrix, ...) #invert teh matrix
        x$setInverse(invertedMatrix) #store the inverted matrix for future
        invertedMatrix #return the inverted matrix
        
}

