## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #asuming the matrix is invertible
        invertible <- NULL
        #setting the matrix
        set <- function(matrix){
                x <<- matrix
                invertible <<- NULL
        }       
        #getting the value of the matrix
        get <- function() x
        #setting the inverse
        setInverse <- function(inverse) invertible <<- inverse
        #getting the inverse
        getInverse <- function() invertible
        #creating a return list
        return(list(set = set, get = get, 
                    setInverse = setInverse,
                    getInverse = getInverse))
        
}


##This function computes the inverse of the speecial matrix, returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertible <- x$getInverse()
        if(!is.null(invertible)) {
                message("getting cached data")
                return(invertible)
        }
        # getting the matrix from our object
        data <- x$get()
        #calculating the inverse with the "solve" function
        invertible<- solve(data, ...)
        # setting the inverse to the object
        x$setInverse(invertible)
        #return the inverse matrix
        return(invertible)
}

