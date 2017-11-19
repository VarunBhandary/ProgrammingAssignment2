## makeCacheMatrix function generates matrix object that will cache 
## inverse of a matrix 

## makeCacheMatrix 
## takes a invertible matrix 'x' as an input
## returns a list containing functions to set the matrix , get the  matrix,
## set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
       Minv <- NULL
       
       #used when to reset the invertible matrix 
       set <- function(y){
               x <<- y
               inv <<- NULL
       }
       get <- function() x
       setinv <- function(inverse) Minv <<- inverse
       getinv <- function() Minv
       list(set = set, get = get, setinv = setinv, getinv = getinv)
        }


## cacheSolve function takes the o/p of the  makeCacheMatrix and returns the
## inverse of the matrix which was inputted to the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        Minv <- x$getinv()
        
        # Check if the inverse has already been calculated & return from cache 
        # if it is present
        if(!is.null(Minv)){
                message("geting data from the cache")
                return(Minv)
        }
        
        #The below willonly execute if thecache is not present
        matrix.data <- x$get()
        Minv <- solve(matrix.data, ...)
        
        x$setinv(Minv)
        
        return(Minv)
}
