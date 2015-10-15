## Assignment 2 is to write a pair of functions that cache theinverse of a 
## matrix.
## The following functions hae been defined.




## makeCacheMatrix : This function creates a special "matrix" object that 
## can cache its inverse. 

makeCacheMatrix <- function(x = matrix()){
        m <- NULL   ## variable in local environment initialised as NULL
        set <- function(y) {
                x <<- y
                m <<- NULL ## variable in containing environment
        }
        ## to display contents of matrix
        get <- function() x 
        
        ## to reset m in containing environment as its not NULL as inverse 
        ##exists after the cacheSolve function is executed
        
        setinverse <- function(inverse) m <<- inverse 
        
        ## to return matrix inverse 
        getinverse <- function() m  
        
        ## list with all the names/computed elements
                
        list(set = set, get = get,setinverse=setinverse,
             getinverse = getinverse) 
        
        }

## cacheSolve : This function computes the inverse of the special "matrix" 
## returned by  makeCacheMatrix  above. If the inverse has already been 
## calculated (and the matrix has not changed), then  cacheSolve  should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                m <- x$getinverse() ## would be null for new matrix
                
                if(!is.null(m)) {
                        message("getting cached data") 
                        return(m)
                }
                data <- x$get()  ## computes inverse if it does not exist already
                m <- solve(data, ...)
                x$setinverse(m)  ## sets value of inverse in cache 
                m
                
        }

